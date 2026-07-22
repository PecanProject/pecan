#!/usr/bin/env Rscript
# Fit phenology date gap-fill models from matched LandIQ–MSLSP rows.
#
# Training years (default 2018–2023): assigned_by == "matched" with usable MSLSP
# planting/harvest dates and LandIQ CLASS. Fits:
#   planting_doy ~ landiq_ADOY * landiq_CLASS
#   harvest_doy  ~ landiq_ADOY * landiq_CLASS
# plus crop-class means (and harvest means by CLASS×PFT) when ADOY is missing.
#
# USAGE
#   Rscript fit_phenology_gapfill_models.R
#   GAPFILL_TRAIN_YEARS=2018,2019,2020,2021,2022,2023 Rscript fit_phenology_gapfill_models.R
#
# ENV: CCMMF_MANAGEMENT, CCMMF_MATCHED_DIR, GAPFILL_TRAIN_YEARS, GAPFILL_MODEL_DIR
#
# Writes: phenology/gapfill_models/phenology_date_gapfill_models.rds (+ summaries)

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(lubridate)
})

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
matched_dir <- Sys.getenv(
  "CCMMF_MATCHED_DIR",
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

train[, planting_doy := doy_from_date(mslsp_OGI)]
train[, harvest_date_raw := as.Date(NA)]
train[landiq_PFT %in% c("row", "rice"), harvest_date_raw := as.Date(mslsp_OGMn)]
train[landiq_PFT %in% c("hay", "woody"), harvest_date_raw := as.Date(mslsp_OGD)]
train[, harvest_doy := doy_from_date(harvest_date_raw)]

# Stable DOY range for training
train[planting_doy < 1L | planting_doy > 366L, planting_doy := NA_integer_]
train[harvest_doy < 1L | harvest_doy > 366L, harvest_doy := NA_integer_]

plant_train <- train[!is.na(planting_doy) & !is.na(landiq_ADOY)]
harv_train <- train[!is.na(harvest_doy) & !is.na(landiq_ADOY)]
message("[fit] planting LM n=", nrow(plant_train), "; harvest LM n=", nrow(harv_train))

if (nrow(plant_train) < 50L || nrow(harv_train) < 50L) {
  stop("Insufficient training rows with ADOY + MSLSP dates for LM fit.")
}

# Drop CLASS levels with too few ADOY rows for interaction stability
min_n_class <- 30L
plant_ok <- plant_train[, .N, by = landiq_CLASS][N >= min_n_class, landiq_CLASS]
harv_ok <- harv_train[, .N, by = landiq_CLASS][N >= min_n_class, landiq_CLASS]
plant_train <- plant_train[landiq_CLASS %in% plant_ok]
harv_train <- harv_train[landiq_CLASS %in% harv_ok]
plant_train[, landiq_CLASS := droplevels(landiq_CLASS)]
harv_train[, landiq_CLASS := droplevels(landiq_CLASS)]

lm_planting <- lm(planting_doy ~ landiq_ADOY * landiq_CLASS, data = plant_train)
lm_harvest <- lm(harvest_doy ~ landiq_ADOY * landiq_CLASS, data = harv_train)

# Class means (all matched with dates; ADOY not required)
plant_means <- train[
  !is.na(planting_doy),
  .(planting_doy_mean = mean(planting_doy), n = .N),
  by = .(landiq_CLASS = as.character(landiq_CLASS))
]
harv_means <- train[
  !is.na(harvest_doy),
  .(harvest_doy_mean = mean(harvest_doy), n = .N),
  by = .(
    landiq_CLASS = as.character(landiq_CLASS),
    landiq_PFT = as.character(landiq_PFT)
  )
]
# Fallback: CLASS-only harvest mean, then global
harv_means_class <- train[
  !is.na(harvest_doy),
  .(harvest_doy_mean = mean(harvest_doy), n = .N),
  by = .(landiq_CLASS = as.character(landiq_CLASS))
]
plant_global <- mean(train$planting_doy, na.rm = TRUE)
harv_global <- mean(train$harvest_doy, na.rm = TRUE)

models <- list(
  version = "1",
  created = as.character(Sys.time()),
  train_years = train_years,
  matched_dir = matched_dir,
  lm_planting = lm_planting,
  lm_harvest = lm_harvest,
  plant_class_levels = levels(plant_train$landiq_CLASS),
  harvest_class_levels = levels(harv_train$landiq_CLASS),
  plant_means = plant_means,
  harvest_means_class_pft = harv_means,
  harvest_means_class = harv_means_class,
  planting_doy_global = plant_global,
  harvest_doy_global = harv_global,
  n_plant_lm = nrow(plant_train),
  n_harvest_lm = nrow(harv_train)
)

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
out_rds <- file.path(model_dir, "phenology_date_gapfill_models.rds")
saveRDS(models, out_rds)
message("[fit] wrote ", out_rds)

# Text summaries for docs / QC
sink(file.path(model_dir, "phenology_date_gapfill_models_summary.txt"))
cat("Phenology date gap-fill models\n")
cat("Created: ", models$created, "\n", sep = "")
cat("Train years: ", paste(train_years, collapse = ", "), "\n\n", sep = "")
cat("=== Planting LM ===\n")
print(summary(lm_planting))
cat("\n=== Harvest LM ===\n")
print(summary(lm_harvest))
cat("\n=== Planting means by CLASS ===\n")
print(plant_means)
cat("\n=== Harvest means by CLASS x PFT ===\n")
print(harv_means)
sink()
message("[fit] wrote summary txt")
