#!/usr/bin/env Rscript
# Apply phenology gap-fill to assigned_year=Y.parquet (overlay; does not
# overwrite canonical assigned files).
#
# Rules: if MSLSP metric present -> keep. Else for row/rice/hay with observed
# LandIQ ADOY: intercept[CLASS] + slope_adoy[CLASS] * ADOY. Else CLASS x
# SUBCLASS x PFT / CLASS x PFT / CLASS / global mean. Woody uses means only
# (not the ADOY LM). Idle/fallow "other" is not filled.
#
# Date DOY is relative to the assigned year (1 = Jan 1; negative = prior
# calendar year), matching match_landiq_mslsp.R.
#
# Fills EVI-based MSLSP phenometrics in place for row/rice/hay/woody:
#   dates: OGI, 50PCGI, OGMx, Peak, OGD, 50PCGD, OGMn
#   evi:   EVImax, EVIamp, EVIarea
#
# Provenance column:
#   gapfill_date_source (mslsp | lm_adoy | mean_crop | none)
#   -- based on whether any date metric was observed vs gap-filled for the row.
#
# USAGE
#   Rscript apply_phenology_gapfill.R <year>
#   Rscript apply_phenology_gapfill.R 2016 2017 2018
#
# ENV: PRODUCTS_INVENTORY, MATCHED_DIR, GAPFILL_MODEL_DIR, GAPFILL_DATES_DIR
#
# Reads:  $GAPFILL_MODEL_DIR/phenology_gapfill_*.csv + phenology_gapfill_meta.json
#         (default: this package phenology/gapfill/outputs, shipped in the clone)
# Writes: phenology/matched_landiq_mslsp_v4.1.2/gapfill_dates/assigned_year=Y_gapfilled.parquet

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
  library(jsonlite)
})

.this_dir <- {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg)) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else {
    getwd()
  }
}
source(file.path(.this_dir, "R", "dates.R"), local = TRUE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript apply_phenology_gapfill.R <year> [year2 ...]")
}
years <- as.integer(args)
if (any(is.na(years))) {
  stop("Years must be integers")
}

path_inventory <- Sys.getenv("PRODUCTS_INVENTORY", "")
if (!nzchar(trimws(path_inventory))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set PRODUCTS_INVENTORY or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_inventory <- file.path(.root, "products", "inventory")
}
matched_dir <- Sys.getenv(
  "MATCHED_DIR",
  file.path(path_inventory, "phenology", "matched_landiq_mslsp_v4.1.2")
)
model_dir <- Sys.getenv("GAPFILL_MODEL_DIR", "")
if (!nzchar(trimws(model_dir))) {
  model_dir <- file.path(.this_dir, "outputs")
}
out_dir <- Sys.getenv(
  "GAPFILL_DATES_DIR",
  file.path(matched_dir, "gapfill_dates")
)
lm_f <- file.path(model_dir, "phenology_gapfill_lm.csv")
g_f <- file.path(model_dir, "phenology_gapfill_means_global.csv")
if (!file.exists(lm_f) || !file.exists(g_f)) {
  stop("Missing gap-fill tables in ", model_dir, ". Run fit_phenology_gapfill_models.R.")
}
lm_tab <- fread(lm_f)
means_cp <- fread(file.path(model_dir, "phenology_gapfill_means_class_pft.csv"))
means_c <- fread(file.path(model_dir, "phenology_gapfill_means_class.csv"))
means_g <- fread(g_f)
cs_f <- file.path(model_dir, "phenology_gapfill_means_class_subclass.csv")
means_cs <- if (file.exists(cs_f)) {
  fread(cs_f)
} else {
  data.table(
    metric = character(), landiq_CLASS = character(),
    landiq_SUBCLASS = character(), landiq_PFT = character(),
    y_mean = numeric(), n = integer()
  )
}
if ("landiq_PFT" %in% names(means_cp)) {
  means_cp[, landiq_PFT := tolower(trimws(as.character(landiq_PFT)))]
}
if ("landiq_PFT" %in% names(means_cs) && nrow(means_cs)) {
  means_cs[, landiq_PFT := tolower(trimws(as.character(landiq_PFT)))]
  means_cs[, landiq_SUBCLASS := trimws(as.character(landiq_SUBCLASS))]
}
meta_f <- file.path(model_dir, "phenology_gapfill_meta.json")
meta <- if (file.exists(meta_f)) jsonlite::fromJSON(meta_f, simplifyVector = TRUE) else list()
metric_order <- c("OGI", "50PCGI", "OGMx", "Peak", "OGD", "50PCGD", "OGMn", "EVImax", "EVIamp", "EVIarea")
metric_names <- intersect(metric_order, unique(lm_tab$metric))
models <- list(
  version = as.character(meta$version),
  meta = meta,
  metrics = {
    out <- lapply(metric_names, function(nm) {
      lm_m <- lm_tab[metric == nm]
      g <- means_g[metric == nm]
      list(
        name = nm,
        col = lm_m$col[1],
        type = lm_m$type[1],
        lm_params = lm_m[, .(landiq_CLASS, intercept, slope_adoy)],
        means_class_subclass = if (nrow(means_cs)) {
          means_cs[metric == nm, .(landiq_CLASS, landiq_SUBCLASS, landiq_PFT, y_mean, n)]
        } else {
          means_cs
        },
        means_class_pft = means_cp[metric == nm, .(landiq_CLASS, landiq_PFT, y_mean, n)],
        means_class = means_c[metric == nm, .(landiq_CLASS, y_mean, n)],
        global_mean = g$y_mean[1]
      )
    })
    names(out) <- metric_names
    out
  }
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message(
  "[apply] models version=", models$version,
  " train_years=", paste(models$meta$train_years, collapse = ","),
  " dir=", model_dir
)

predict_lm_value <- function(lm_params, adoy, class_chr, type) {
  n <- length(adoy)
  out <- rep(NA_real_, n)
  cls <- trimws(as.character(class_chr))
  ad <- as.numeric(adoy)
  i <- match(cls, lm_params$landiq_CLASS)
  ok <- !is.na(ad) & is.finite(ad) & !is.na(i)
  if (!any(ok)) {
    return(out)
  }
  pred <- lm_params$intercept[i[ok]] + lm_params$slope_adoy[i[ok]] * ad[ok]
  pred[!is.finite(pred)] <- NA_real_
  if (identical(type, "date")) {
    pred[is.na(pred) | abs(pred) > .doy_abs_max] <- NA_real_
  }
  out[ok] <- pred
  out
}

lookup_mean_value <- function(class_chr, pft_chr, subclass_chr,
                              means_cs, means_cp, means_c, global_mean) {
  cls <- trimws(as.character(class_chr))
  pft <- trimws(as.character(pft_chr))
  subc <- trimws(as.character(subclass_chr))
  m <- rep(NA_real_, length(cls))
  if (!is.null(means_cs) && nrow(means_cs)) {
    key <- paste(cls, subc, pft, sep = "\r")
    cs_key <- paste(
      means_cs$landiq_CLASS, means_cs$landiq_SUBCLASS, means_cs$landiq_PFT,
      sep = "\r"
    )
    m <- means_cs$y_mean[match(key, cs_key)]
  }
  miss <- is.na(m)
  if (any(miss)) {
    key2 <- paste(cls[miss], pft[miss], sep = "\r")
    cp_key <- paste(means_cp$landiq_CLASS, means_cp$landiq_PFT, sep = "\r")
    m[miss] <- means_cp$y_mean[match(key2, cp_key)]
  }
  miss <- is.na(m)
  if (any(miss)) {
    m[miss] <- means_c$y_mean[match(cls[miss], means_c$landiq_CLASS)]
  }
  ifelse(is.na(m), global_mean, m)
}

fill_one_metric <- function(dt, mmod, is_crop, has_class, adoy_lm_ok, yr_col) {
  col <- mmod$col
  type <- mmod$type
  if (!col %in% names(dt)) {
    if (identical(type, "date")) {
      dt[, (col) := as.Date(NA)]
    } else {
      dt[, (col) := NA_real_]
    }
  }

  if (identical(type, "date")) {
    missing_obs <- date_col_missing(dt, col)
  } else {
    missing_obs <- is.na(suppressWarnings(as.numeric(dt[[col]])))
  }
  need <- which(is_crop & has_class & missing_obs)
  if (!length(need)) {
    return(list(dt = dt, n_lm = 0L, n_mean = 0L, filled_idx = integer()))
  }

  new_val <- rep(NA_real_, length(need))
  pred <- predict_lm_value(
    mmod$lm_params,
    dt$landiq_ADOY_num[need],
    dt$landiq_CLASS_chr[need],
    type
  )
  use_lm <- adoy_lm_ok[need] & !is.na(pred)
  if (any(use_lm)) {
    new_val[use_lm] <- pred[use_lm]
  }
  need_mean <- which(is.na(new_val))
  n_mean <- 0L
  if (length(need_mean)) {
    mv <- lookup_mean_value(
      dt$landiq_CLASS_chr[need][need_mean],
      dt$landiq_PFT_chr[need][need_mean],
      dt$landiq_SUBCLASS_chr[need][need_mean],
      mmod$means_class_subclass,
      mmod$means_class_pft,
      mmod$means_class,
      mmod$global_mean
    )
    ok <- !is.na(mv)
    if (any(ok)) {
      new_val[need_mean[ok]] <- mv[ok]
      n_mean <- sum(ok)
    }
  }

  filled_local <- which(!is.na(new_val))
  if (!length(filled_local)) {
    return(list(dt = dt, n_lm = sum(use_lm), n_mean = n_mean, filled_idx = integer()))
  }
  ii <- need[filled_local]
  vv <- new_val[filled_local]

  if (identical(type, "date")) {
    dt[ii, (col) := phenology_doy_to_date(yr_col[ii], vv)]
  } else {
    dt[ii, (col) := vv]
  }

  used_lm <- filled_local[use_lm[filled_local]]
  if (length(used_lm)) {
    ii_lm <- need[used_lm]
    still <- ii_lm[dt$gapfill_date_source[ii_lm] == "none"]
    if (length(still)) dt[still, gapfill_date_source := "lm_adoy"]
  }
  used_mean <- filled_local[!use_lm[filled_local]]
  if (length(used_mean)) {
    ii_mean <- need[used_mean]
    still <- ii_mean[dt$gapfill_date_source[ii_mean] == "none"]
    if (length(still)) dt[still, gapfill_date_source := "mean_crop"]
  }

  list(dt = dt, n_lm = sum(use_lm), n_mean = n_mean, filled_idx = ii)
}

fill_one_year <- function(yr) {
  f <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", yr))
  if (!file.exists(f)) {
    message("[apply] skip missing ", f)
    return(invisible(NULL))
  }
  dt <- as.data.table(arrow::read_parquet(f))
  message("[apply] year=", yr, " rows=", nrow(dt))

  dt[, landiq_CLASS_chr := trimws(as.character(landiq_CLASS))]
  dt[, landiq_PFT_chr := tolower(trimws(as.character(landiq_PFT)))]
  if ("landiq_SUBCLASS" %in% names(dt)) {
    dt[, landiq_SUBCLASS_chr := trimws(as.character(landiq_SUBCLASS))]
  } else {
    dt[, landiq_SUBCLASS_chr := NA_character_]
  }
  dt[, landiq_ADOY_num := suppressWarnings(as.numeric(landiq_ADOY))]
  # LandIQ missing peak is 0. Negative ADOY is a real winter / prior-year peak.
  dt[!is.finite(landiq_ADOY_num) | landiq_ADOY_num == 0, landiq_ADOY_num := NA_real_]

  annual_pfts <- c("row", "rice", "hay")
  crop_pfts <- c(annual_pfts, "woody")
  is_crop <- dt$landiq_PFT_chr %in% crop_pfts
  is_annual <- dt$landiq_PFT_chr %in% annual_pfts
  has_class <- !is.na(dt$landiq_CLASS_chr) & nzchar(dt$landiq_CLASS_chr)
  yr_col <- if ("year" %in% names(dt)) as.integer(dt$year) else rep(as.integer(yr), nrow(dt))

  adoy_lm_ok <- !is.na(dt$landiq_ADOY_num) & is_annual
  src_col <- intersect(c("landiq_adoy_source", "adoy_source"), names(dt))
  if (length(src_col)) {
    src <- tolower(trimws(as.character(dt[[src_col[[1]]]])))
    adoy_lm_ok <- adoy_lm_ok & !is.na(src) & src == "observed"
  } else {
    ad <- dt$landiq_ADOY_num
    looks_observed <- !is.na(ad) & (
      ad < 0 | abs(ad - round(ad)) < 1e-6
    )
    adoy_lm_ok <- adoy_lm_ok & looks_observed
  }

  ogi_missing <- date_col_missing(dt, "mslsp_OGI")
  cand <- which(is_crop & has_class & ogi_missing)
  message("[apply] candidates (crop PFT, CLASS, missing OGI; woody = mean only):")
  print(dt[cand, .N, by = .(assigned_by, match_outcome, landiq_PFT)])

  date_mods <- models$metrics[vapply(models$metrics, function(m) m$type == "date", logical(1))]
  any_date_obs <- rep(FALSE, nrow(dt))
  for (mmod in date_mods) {
    any_date_obs <- any_date_obs | (is_crop & !date_col_missing(dt, mmod$col))
  }
  dt[, gapfill_date_source := "none"]
  dt[any_date_obs, gapfill_date_source := "mslsp"]

  for (nm in names(models$metrics)) {
    mmod <- models$metrics[[nm]]
    res <- fill_one_metric(dt, mmod, is_crop, has_class, adoy_lm_ok, yr_col)
    dt <- res$dt
    message(
      "  ", nm, ": filled lm=", res$n_lm, " mean=", res$n_mean,
      " rows=", length(res$filled_idx)
    )
  }

  dt[, c(
    "landiq_CLASS_chr", "landiq_PFT_chr", "landiq_SUBCLASS_chr", "landiq_ADOY_num"
  ) := NULL]

  drop_legacy <- intersect(
    names(dt),
    c(
      "planting_doy_filled", "harvest_doy_filled",
      "planting_date_filled", "harvest_date_filled",
      "gapfill_planting_source", "gapfill_harvest_source"
    )
  )
  if (length(drop_legacy)) {
    dt[, (drop_legacy) := NULL]
  }

  out_f <- file.path(out_dir, sprintf("assigned_year=%d_gapfilled.parquet", yr))
  arrow::write_parquet(dt, out_f)
  message(
    "[apply] wrote ", out_f,
    " gapfill_date_source: ",
    paste(names(table(dt$gapfill_date_source)), table(dt$gapfill_date_source), collapse = ", ")
  )
  invisible(out_f)
}

for (yr in years) {
  fill_one_year(yr)
}
message("[apply] done")
