#!/usr/bin/env Rscript
# Fit phenology gap-fill models from matched LandIQ-MSLSP rows for all EVI-based
# MSLSP phenometrics (dates + EVI magnitude).
#
# For each metric: value ~ landiq_ADOY * landiq_CLASS, plus CLASS / CLASSxPFT /
# global means when ADOY is missing.
#
# Date metrics (fit as DOY): OGI, 50PCGI, OGMx, Peak, OGD, 50PCGD, OGMn
# Continuous: EVImax, EVIamp, EVIarea
#
# USAGE
#   Rscript fit_phenology_gapfill_models.R
#   GAPFILL_TRAIN_YEARS=2018,2019,2020,2021,2022,2023 Rscript fit_phenology_gapfill_models.R
#
# ENV: MANAGEMENT, MATCHED_DIR, GAPFILL_TRAIN_YEARS, GAPFILL_MODEL_DIR
#
# Writes: phenology/gapfill_models/phenology_date_gapfill_models.rds (+ summaries)

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

path_management <- Sys.getenv("MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
matched_dir <- Sys.getenv(
  "MATCHED_DIR",
  file.path(path_management, "phenology", "matched_landiq_mslsp_v4.1.2")
)
model_dir <- Sys.getenv(
  "GAPFILL_MODEL_DIR",
  file.path(path_management, "phenology", "gapfill_models")
)
train_years_raw <- Sys.getenv("GAPFILL_TRAIN_YEARS", "2018,2019,2020,2021,2022,2023")
train_years <- as.integer(strsplit(train_years_raw, ",", fixed = TRUE)[[1L]])
train_years <- train_years[!is.na(train_years)]

doy_from_date <- function(x) {
  d <- as.Date(x)
  as.integer(lubridate::yday(d))
}

# Metric registry: column on matched parquet + type
metric_specs <- list(
  list(name = "OGI", col = "mslsp_OGI", type = "date"),
  list(name = "50PCGI", col = "mslsp_50PCGI", type = "date"),
  list(name = "OGMx", col = "mslsp_OGMx", type = "date"),
  list(name = "Peak", col = "mslsp_Peak", type = "date"),
  list(name = "OGD", col = "mslsp_OGD", type = "date"),
  list(name = "50PCGD", col = "mslsp_50PCGD", type = "date"),
  list(name = "OGMn", col = "mslsp_OGMn", type = "date"),
  list(name = "EVImax", col = "mslsp_EVImax", type = "evi"),
  list(name = "EVIamp", col = "mslsp_EVIamp", type = "evi"),
  list(name = "EVIarea", col = "mslsp_EVIarea", type = "evi")
)

load_training_rows <- function(years, matched_dir) {
  rows <- lapply(years, function(yr) {
    f <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", yr))
    if (!file.exists(f)) {
      message("[fit] skip missing ", f)
      return(NULL)
    }
    dt <- as.data.table(arrow::read_parquet(f))
    dt <- dt[assigned_by == "matched"]
    dt <- dt[
      !is.na(landiq_CLASS) & !is.na(landiq_SUBCLASS) & !is.na(landiq_PFT)
    ]
    dt[, year := as.integer(year)]
    dt
  })
  rbindlist(rows, use.names = TRUE, fill = TRUE)
}

obs_value <- function(dt, col, type) {
  if (!col %in% names(dt)) {
    return(rep(NA_real_, nrow(dt)))
  }
  if (identical(type, "date")) {
    v <- doy_from_date(dt[[col]])
    v[v < 1L | v > 366L] <- NA_real_
    return(as.numeric(v))
  }
  as.numeric(dt[[col]])
}

fit_one_metric <- function(train, spec, min_n_class = 30L) {
  name <- spec$name
  col <- spec$col
  type <- spec$type
  y <- obs_value(train, col, type)
  train_m <- data.table::copy(train)
  train_m[, y_obs := y]

  lm_train <- train_m[!is.na(y_obs) & !is.na(landiq_ADOY)]
  message("[fit] ", name, " LM candidates n=", nrow(lm_train))
  if (nrow(lm_train) < 50L) {
    stop("Insufficient training rows for metric ", name)
  }
  ok_cls <- lm_train[, .N, by = landiq_CLASS][N >= min_n_class, landiq_CLASS]
  lm_train <- lm_train[landiq_CLASS %in% ok_cls]
  lm_train[, landiq_CLASS := droplevels(landiq_CLASS)]
  if (nrow(lm_train) < 50L || nlevels(lm_train$landiq_CLASS) < 2L) {
    stop("Insufficient CLASS coverage for metric ", name)
  }

  lm_fit <- lm(y_obs ~ landiq_ADOY * landiq_CLASS, data = lm_train)

  means_class_pft <- train_m[
    !is.na(y_obs),
    .(y_mean = mean(y_obs), n = .N),
    by = .(
      landiq_CLASS = as.character(landiq_CLASS),
      landiq_PFT = as.character(landiq_PFT)
    )
  ]
  means_class <- train_m[
    !is.na(y_obs),
    .(y_mean = mean(y_obs), n = .N),
    by = .(landiq_CLASS = as.character(landiq_CLASS))
  ]
  global_mean <- mean(train_m$y_obs, na.rm = TRUE)

  list(
    name = name,
    col = col,
    type = type,
    lm = lm_fit,
    class_levels = levels(lm_train$landiq_CLASS),
    means_class_pft = means_class_pft,
    means_class = means_class,
    global_mean = global_mean,
    n_lm = nrow(lm_train)
  )
}

message("[fit] training years: ", paste(train_years, collapse = ", "))
message("[fit] matched_dir=", matched_dir)
train <- load_training_rows(train_years, matched_dir)
if (nrow(train) == 0L) {
  stop("No training rows found under ", matched_dir)
}
message("[fit] matched training rows: ", nrow(train))

train[, landiq_CLASS := factor(trimws(as.character(landiq_CLASS)))]
train[, landiq_PFT := trimws(as.character(landiq_PFT))]
train[, landiq_ADOY := suppressWarnings(as.numeric(landiq_ADOY))]
train[landiq_ADOY <= 0, landiq_ADOY := NA_real_]

# Crop PFTs only in training for means used on crop fill
train <- train[tolower(landiq_PFT) %in% c("row", "rice", "hay", "woody")]
message("[fit] crop-PFT training rows: ", nrow(train))

metric_models <- lapply(metric_specs, function(spec) {
  out <- fit_one_metric(train, spec)
  gc(verbose = FALSE)
  out
})
names(metric_models) <- vapply(metric_specs, `[[`, character(1), "name")

models <- list(
  version = "2",
  created = as.character(Sys.time()),
  train_years = train_years,
  matched_dir = matched_dir,
  metrics = metric_models,
  # Back-compat aliases used by older docs / callers
  lm_planting = metric_models$OGI$lm,
  lm_harvest = metric_models$OGMn$lm,
  plant_class_levels = metric_models$OGI$class_levels,
  harvest_class_levels = metric_models$OGMn$class_levels,
  planting_doy_global = metric_models$OGI$global_mean,
  harvest_doy_global = metric_models$OGMn$global_mean
)

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
out_rds <- file.path(model_dir, "phenology_date_gapfill_models.rds")
saveRDS(models, out_rds)
message("[fit] wrote ", out_rds)

sink(file.path(model_dir, "phenology_date_gapfill_models_summary.txt"))
cat("Phenology EVI-metric gap-fill models (version 2)\n")
cat("Created: ", models$created, "\n", sep = "")
cat("Train years: ", paste(train_years, collapse = ", "), "\n\n", sep = "")
for (nm in names(metric_models)) {
  m <- metric_models[[nm]]
  cat("=== ", nm, " (", m$type, ", n_lm=", m$n_lm, ") ===\n", sep = "")
  print(summary(m$lm))
  cat("\n")
}
sink()
message("[fit] wrote summary txt")
