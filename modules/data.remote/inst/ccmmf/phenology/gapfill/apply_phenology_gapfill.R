#!/usr/bin/env Rscript
# Apply phenology date gap-fill to assigned_year=Y.parquet (overlay; does not
# overwrite canonical assigned files).
#
# Rules (Mike): if MSLSP date present → use it; else if landiq_ADOY present →
# lm(ADOY * CLASS); else crop-class mean DOY; else none.
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

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
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
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

doy_to_date_str <- function(year, doy) {
  yr <- as.integer(year)
  d <- as.integer(doy)
  out <- rep(NA_character_, length(d))
  ok <- !is.na(yr) & !is.na(d) & d >= 1L & d <= 366L
  if (any(ok)) {
    out[ok] <- as.character(as.Date(d[ok] - 1L, origin = paste0(yr[ok], "-01-01")))
  }
  out
}

doy_from_date <- function(x) {
  d <- as.Date(x)
  as.integer(lubridate::yday(d))
}

predict_lm_doy <- function(lm_fit, adoy, class_chr, allowed_levels) {
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
  pred <- pmin(pmax(pred, 1), 366)
  out[ok] <- pred
  out
}

lookup_plant_mean <- function(class_chr, means_dt, global_mean) {
  cls <- trimws(as.character(class_chr))
  m <- means_dt[match(cls, landiq_CLASS), planting_doy_mean]
  ifelse(is.na(m), global_mean, m)
}

lookup_harvest_mean <- function(class_chr, pft_chr, means_cp, means_c, global_mean) {
  cls <- trimws(as.character(class_chr))
  pft <- trimws(as.character(pft_chr))
  key <- paste(cls, pft, sep = "\r")
  cp_key <- paste(means_cp$landiq_CLASS, means_cp$landiq_PFT, sep = "\r")
  m <- means_cp$harvest_doy_mean[match(key, cp_key)]
  miss <- is.na(m)
  if (any(miss)) {
    m2 <- means_c$harvest_doy_mean[match(cls[miss], means_c$landiq_CLASS)]
    m[miss] <- m2
  }
  ifelse(is.na(m), global_mean, m)
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
  dt[, landiq_PFT_chr := trimws(as.character(landiq_PFT))]
  dt[, landiq_ADOY_num := suppressWarnings(as.numeric(landiq_ADOY))]
  dt[landiq_ADOY_num <= 0, landiq_ADOY_num := NA_real_]

  # Observed MSLSP dates
  dt[, planting_doy_obs := doy_from_date(mslsp_OGI)]
  dt[, harvest_date_obs := as.Date(NA)]
  if ("mslsp_OGMn" %in% names(dt)) {
    dt[landiq_PFT_chr %in% c("row", "rice"), harvest_date_obs := as.Date(mslsp_OGMn)]
  }
  if ("mslsp_OGD" %in% names(dt)) {
    dt[landiq_PFT_chr %in% c("hay", "woody"), harvest_date_obs := as.Date(mslsp_OGD)]
  }
  dt[, harvest_doy_obs := doy_from_date(harvest_date_obs)]

  # Planting fill
  dt[, gapfill_planting_source := "none"]
  dt[, planting_doy_filled := NA_real_]
  has_p <- !is.na(dt$planting_doy_obs)
  dt[has_p, `:=`(planting_doy_filled = planting_doy_obs, gapfill_planting_source = "mslsp")]

  need_p <- which(!has_p & !is.na(dt$landiq_CLASS_chr) & nzchar(dt$landiq_CLASS_chr))
  if (length(need_p)) {
    pred_p <- predict_lm_doy(
      models$lm_planting,
      dt$landiq_ADOY_num[need_p],
      dt$landiq_CLASS_chr[need_p],
      models$plant_class_levels
    )
    use_lm <- !is.na(dt$landiq_ADOY_num[need_p]) & !is.na(pred_p)
    if (any(use_lm)) {
      ii <- need_p[use_lm]
      dt[ii, planting_doy_filled := pred_p[use_lm]]
      dt[ii, gapfill_planting_source := "lm_adoy"]
    }
    need_mean <- need_p[is.na(dt$planting_doy_filled[need_p])]
    if (length(need_mean)) {
      mp <- lookup_plant_mean(
        dt$landiq_CLASS_chr[need_mean], models$plant_means, models$planting_doy_global
      )
      ok <- !is.na(mp)
      if (any(ok)) {
        ii <- need_mean[ok]
        dt[ii, planting_doy_filled := mp[ok]]
        dt[ii, gapfill_planting_source := "mean_crop"]
      }
    }
  }

  # Harvest fill
  dt[, gapfill_harvest_source := "none"]
  dt[, harvest_doy_filled := NA_real_]
  has_h <- !is.na(dt$harvest_doy_obs)
  dt[has_h, `:=`(harvest_doy_filled = harvest_doy_obs, gapfill_harvest_source = "mslsp")]

  need_h <- which(
    !has_h & !is.na(dt$landiq_CLASS_chr) & nzchar(dt$landiq_CLASS_chr) &
      dt$landiq_PFT_chr %in% c("row", "rice", "hay", "woody")
  )
  if (length(need_h)) {
    pred_h <- predict_lm_doy(
      models$lm_harvest,
      dt$landiq_ADOY_num[need_h],
      dt$landiq_CLASS_chr[need_h],
      models$harvest_class_levels
    )
    use_lm <- !is.na(dt$landiq_ADOY_num[need_h]) & !is.na(pred_h)
    if (any(use_lm)) {
      ii <- need_h[use_lm]
      dt[ii, harvest_doy_filled := pred_h[use_lm]]
      dt[ii, gapfill_harvest_source := "lm_adoy"]
    }
    need_mean <- need_h[is.na(dt$harvest_doy_filled[need_h])]
    if (length(need_mean)) {
      mh <- lookup_harvest_mean(
        dt$landiq_CLASS_chr[need_mean],
        dt$landiq_PFT_chr[need_mean],
        models$harvest_means_class_pft,
        models$harvest_means_class,
        models$harvest_doy_global
      )
      ok <- !is.na(mh)
      if (any(ok)) {
        ii <- need_mean[ok]
        dt[ii, harvest_doy_filled := mh[ok]]
        dt[ii, gapfill_harvest_source := "mean_crop"]
      }
    }
  }

  yr_col <- if ("year" %in% names(dt)) dt$year else yr
  dt[, planting_date_filled := doy_to_date_str(yr_col, planting_doy_filled)]
  dt[, harvest_date_filled := doy_to_date_str(yr_col, harvest_doy_filled)]

  dt[, c(
    "landiq_CLASS_chr", "landiq_PFT_chr", "landiq_ADOY_num",
    "planting_doy_obs", "harvest_date_obs", "harvest_doy_obs"
  ) := NULL]

  out_f <- file.path(out_dir, sprintf("assigned_year=%d_gapfilled.parquet", yr))
  arrow::write_parquet(dt, out_f)
  message(
    "[apply] wrote ", out_f,
    " planting sources: ", paste(names(table(dt$gapfill_planting_source)), table(dt$gapfill_planting_source), collapse = ", "),
    "; harvest sources: ", paste(names(table(dt$gapfill_harvest_source)), table(dt$gapfill_harvest_source), collapse = ", ")
  )
  invisible(out_f)
}

for (yr in years) {
  fill_one_year(yr)
}
message("[apply] done")
