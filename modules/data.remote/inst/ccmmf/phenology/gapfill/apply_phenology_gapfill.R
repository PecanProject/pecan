#!/usr/bin/env Rscript
# Apply phenology gap-fill to assigned_year=Y.parquet (overlay; does not
# overwrite canonical assigned files).
#
# Rules: if MSLSP metric present -> keep; else if landiq_ADOY present ->
# lm(ADOY * CLASS); else crop CLASSxPFT / CLASS / global mean; else leave NA.
#
# Fills all EVI-based MSLSP phenometrics in place for crop PFTs only
# (row/rice/hay/woody; not idle/fallow "other"):
#   dates: OGI, 50PCGI, OGMx, Peak, OGD, 50PCGD, OGMn
#   evi:   EVImax, EVIamp, EVIarea
#
# Provenance column:
#   gapfill_date_source (mslsp | lm_adoy | mean_crop | none)
#   -- based on whether any date metric was observed vs invented for the row.
#
# USAGE
#   Rscript apply_phenology_gapfill.R <year>
#   Rscript apply_phenology_gapfill.R 2016 2017 2018
#
# ENV: CCMMF_MANAGEMENT, CCMMF_MATCHED_DIR, GAPFILL_MODEL_DIR, GAPFILL_DATES_DIR
#
# Writes: phenology/matched_landiq_mslsp_v4.1.2/gapfill_dates/assigned_year=Y_gapfilled.parquet

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript apply_phenology_gapfill.R <year> [year2 ...]")
}
years <- as.integer(args)
if (any(is.na(years))) {
  stop("Years must be integers")
}

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
matched_dir <- Sys.getenv(
  "CCMMF_MATCHED_DIR",
  file.path(path_management, "phenology", "matched_landiq_mslsp_v4.1.2")
)
model_dir <- Sys.getenv(
  "GAPFILL_MODEL_DIR",
  file.path(path_management, "phenology", "gapfill_models")
)
out_dir <- Sys.getenv(
  "GAPFILL_DATES_DIR",
  file.path(matched_dir, "gapfill_dates")
)
model_rds <- file.path(model_dir, "phenology_date_gapfill_models.rds")
if (!file.exists(model_rds)) {
  stop("Missing models RDS. Run fit_phenology_gapfill_models.R first: ", model_rds)
}
models <- readRDS(model_rds)
if (is.null(models$metrics) || !identical(as.character(models$version), "2")) {
  stop(
    "Model RDS is not version 2 (all-metric). Re-run fit_phenology_gapfill_models.R: ",
    model_rds
  )
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

doy_to_date <- function(year, doy) {
  yr <- as.integer(year)
  d <- as.integer(round(doy))
  out <- as.Date(rep(NA_character_, length(d)))
  ok <- !is.na(yr) & !is.na(d) & d >= 1L & d <= 366L
  if (any(ok)) {
    out[ok] <- as.Date(d[ok] - 1L, origin = paste0(yr[ok], "-01-01"))
  }
  out
}

doy_from_date <- function(x) {
  d <- as.Date(x)
  as.integer(lubridate::yday(d))
}

predict_lm_value <- function(lm_fit, adoy, class_chr, allowed_levels, type) {
  n <- length(adoy)
  out <- rep(NA_real_, n)
  cls <- trimws(as.character(class_chr))
  ok <- !is.na(adoy) & is.finite(adoy) & cls %in% allowed_levels
  if (!any(ok)) {
    return(out)
  }
  nd <- data.frame(
    landiq_ADOY = as.numeric(adoy[ok]),
    landiq_CLASS = factor(cls[ok], levels = allowed_levels)
  )
  pred <- as.numeric(predict(lm_fit, newdata = nd))
  pred[!is.finite(pred)] <- NA_real_
  if (identical(type, "date")) {
    pred <- pmin(pmax(pred, 1), 366)
  }
  out[ok] <- pred
  out
}

lookup_mean_value <- function(class_chr, pft_chr, means_cp, means_c, global_mean) {
  cls <- trimws(as.character(class_chr))
  pft <- trimws(as.character(pft_chr))
  key <- paste(cls, pft, sep = "\r")
  cp_key <- paste(means_cp$landiq_CLASS, means_cp$landiq_PFT, sep = "\r")
  m <- means_cp$y_mean[match(key, cp_key)]
  miss <- is.na(m)
  if (any(miss)) {
    m2 <- means_c$y_mean[match(cls[miss], means_c$landiq_CLASS)]
    m[miss] <- m2
  }
  ifelse(is.na(m), global_mean, m)
}

obs_numeric <- function(dt, col, type) {
  if (!col %in% names(dt)) {
    return(rep(NA_real_, nrow(dt)))
  }
  if (identical(type, "date")) {
    v <- doy_from_date(dt[[col]])
    v[is.na(v) | v < 1L | v > 366L] <- NA_real_
    return(as.numeric(v))
  }
  suppressWarnings(as.numeric(dt[[col]]))
}

fill_one_metric <- function(dt, mmod, is_crop, has_class, yr_col) {
  col <- mmod$col
  type <- mmod$type
  if (!col %in% names(dt)) {
    if (identical(type, "date")) {
      dt[, (col) := as.Date(NA)]
    } else {
      dt[, (col) := NA_real_]
    }
  }

  obs <- obs_numeric(dt, col, type)
  need <- which(is_crop & has_class & is.na(obs))
  if (!length(need)) {
    return(list(dt = dt, n_lm = 0L, n_mean = 0L, filled_idx = integer()))
  }

  new_val <- rep(NA_real_, length(need))
  pred <- predict_lm_value(
    mmod$lm,
    dt$landiq_ADOY_num[need],
    dt$landiq_CLASS_chr[need],
    mmod$class_levels,
    type
  )
  use_lm <- !is.na(dt$landiq_ADOY_num[need]) & !is.na(pred)
  if (any(use_lm)) {
    new_val[use_lm] <- pred[use_lm]
  }
  need_mean <- which(is.na(new_val))
  n_mean <- 0L
  if (length(need_mean)) {
    mv <- lookup_mean_value(
      dt$landiq_CLASS_chr[need][need_mean],
      dt$landiq_PFT_chr[need][need_mean],
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
    dt[ii, (col) := doy_to_date(yr_col[ii], vv)]
  } else {
    dt[ii, (col) := vv]
  }

  # Provenance: only upgrade from none
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
  dt[, landiq_ADOY_num := suppressWarnings(as.numeric(landiq_ADOY))]
  dt[landiq_ADOY_num <= 0, landiq_ADOY_num := NA_real_]

  crop_pfts <- c("row", "rice", "hay", "woody")
  is_crop <- dt$landiq_PFT_chr %in% crop_pfts
  has_class <- !is.na(dt$landiq_CLASS_chr) & nzchar(dt$landiq_CLASS_chr)
  yr_col <- if ("year" %in% names(dt)) as.integer(dt$year) else rep(as.integer(yr), nrow(dt))

  # Any observed date metric => mslsp provenance for the row
  date_mods <- models$metrics[vapply(models$metrics, function(m) m$type == "date", logical(1))]
  any_date_obs <- rep(FALSE, nrow(dt))
  for (mmod in date_mods) {
    any_date_obs <- any_date_obs | (is_crop & !is.na(obs_numeric(dt, mmod$col, "date")))
  }
  dt[, gapfill_date_source := "none"]
  dt[any_date_obs, gapfill_date_source := "mslsp"]

  for (nm in names(models$metrics)) {
    mmod <- models$metrics[[nm]]
    res <- fill_one_metric(dt, mmod, is_crop, has_class, yr_col)
    dt <- res$dt
    message(
      "  ", nm, ": invented lm=", res$n_lm, " mean=", res$n_mean,
      " rows=", length(res$filled_idx)
    )
  }

  dt[, c("landiq_CLASS_chr", "landiq_PFT_chr", "landiq_ADOY_num") := NULL]

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
