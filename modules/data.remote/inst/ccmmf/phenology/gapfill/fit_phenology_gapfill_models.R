#!/usr/bin/env Rscript
# Fit phenology gap-fill models from matched LandIQ-MSLSP rows.
# LM is row/rice/hay only. Means include woody (woody apply uses means, not LM).
#
# For each metric: value ~ landiq_ADOY * landiq_CLASS, written as per-CLASS
# intercept and ADOY slope, plus CLASS x SUBCLASS x PFT / CLASS x PFT / CLASS /
# global means when ADOY is missing or not observed.
# Date metrics are day-of-year relative to the assigned year (1 = Jan 1;
# negative = prior calendar year). Do not use calendar yday.
#
# Date metrics: OGI, 50PCGI, OGMx, Peak, OGD, 50PCGD, OGMn
# Continuous: EVImax, EVIamp, EVIarea
#
# USAGE
#   Rscript fit_phenology_gapfill_models.R
#   GAPFILL_TRAIN_YEARS=2018,2019,2020,2021,2022,2023 Rscript fit_phenology_gapfill_models.R
#
# ENV: PRODUCTS_INVENTORY, MATCHED_DIR, GAPFILL_TRAIN_YEARS, GAPFILL_MODEL_DIR
#
# Writes: $GAPFILL_MODEL_DIR/phenology_gapfill_{lm,means_*,meta}.csv/json

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

crop_pfts <- c("row", "rice", "hay", "woody")
annual_pfts <- c("row", "rice", "hay")
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

lm_class_params <- function(lm_fit, class_levels, n_by_class) {
  levs <- as.character(class_levels)
  nd0 <- data.frame(
    landiq_ADOY = 0,
    landiq_CLASS = factor(levs, levels = levs)
  )
  nd1 <- data.frame(
    landiq_ADOY = 1,
    landiq_CLASS = factor(levs, levels = levs)
  )
  intercept <- as.numeric(stats::predict(lm_fit, newdata = nd0))
  slope <- as.numeric(stats::predict(lm_fit, newdata = nd1)) - intercept
  sm <- summary(lm_fit)
  n_vec <- as.integer(n_by_class[match(levs, names(n_by_class))])
  dt <- data.table(
    landiq_CLASS = levs,
    intercept = intercept,
    slope_adoy = slope,
    n = n_vec
  )
  dt <- dt[is.finite(intercept) & is.finite(slope_adoy)]
  list(table = dt, r_squared = unname(sm$r.squared), adj_r_squared = unname(sm$adj.r.squared))
}

write_model_outputs <- function(model_dir, meta, lm_tab, means_cs, means_cp, means_c, means_g) {
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(lm_tab, file.path(model_dir, "phenology_gapfill_lm.csv"))
  fwrite(means_cs, file.path(model_dir, "phenology_gapfill_means_class_subclass.csv"))
  fwrite(means_cp, file.path(model_dir, "phenology_gapfill_means_class_pft.csv"))
  fwrite(means_c, file.path(model_dir, "phenology_gapfill_means_class.csv"))
  fwrite(means_g, file.path(model_dir, "phenology_gapfill_means_global.csv"))
  jsonlite::write_json(
    meta, file.path(model_dir, "phenology_gapfill_meta.json"),
    auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"
  )
  message("[fit] wrote tables in ", model_dir)
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
model_dir <- Sys.getenv(
  "GAPFILL_MODEL_DIR",
  file.path(path_inventory, "phenology", "gapfill_models")
)
train_years_raw <- Sys.getenv("GAPFILL_TRAIN_YEARS", "2018,2019,2020,2021,2022,2023")
train_years <- as.integer(strsplit(train_years_raw, ",", fixed = TRUE)[[1L]])
train_years <- train_years[!is.na(train_years)]

train_cols <- unique(c(
  "assigned_by", "year", "landiq_CLASS", "landiq_SUBCLASS", "landiq_PFT",
  "landiq_ADOY", "landiq_adoy_source",
  vapply(metric_specs, `[[`, character(1), "col")
))

load_training_rows <- function(years, matched_dir) {
  rows <- lapply(years, function(yr) {
    f <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", yr))
    if (!file.exists(f)) {
      message("[fit] skip missing ", f)
      return(NULL)
    }
    schema_names <- names(arrow::read_parquet(f, as_data_frame = FALSE)$schema)
    cols <- intersect(train_cols, schema_names)
    dt <- as.data.table(arrow::read_parquet(f, col_select = cols))
    dt <- dt[assigned_by == "matched"]
    dt <- dt[
      !is.na(landiq_CLASS) & !is.na(landiq_SUBCLASS) & !is.na(landiq_PFT)
    ]
    if (!"year" %in% names(dt)) {
      dt[, year := as.integer(yr)]
    } else {
      dt[, year := as.integer(year)]
      dt[is.na(year), year := as.integer(yr)]
    }
    dt
  })
  rbindlist(rows, use.names = TRUE, fill = TRUE)
}

obs_value <- function(dt, col, type) {
  if (!col %in% names(dt)) {
    return(rep(NA_real_, nrow(dt)))
  }
  if (identical(type, "date")) {
    return(phenology_doy_from_date(dt[[col]], dt$year))
  }
  as.numeric(dt[[col]])
}

adoy_observed <- function(dt) {
  ad <- suppressWarnings(as.numeric(dt$landiq_ADOY))
  if ("landiq_adoy_source" %in% names(dt)) {
    src <- tolower(trimws(as.character(dt$landiq_adoy_source)))
    if (any(!is.na(src) & nzchar(src))) {
      return(!is.na(src) & src == "observed")
    }
  }
  !is.na(ad) & (ad < 0 | abs(ad - round(ad)) < 1e-6)
}

fit_one_metric <- function(train, spec) {
  name <- spec$name
  col <- spec$col
  type <- spec$type
  y <- obs_value(train, col, type)
  train_m <- data.table::copy(train)
  train_m[, y_obs := y]
  train_m[, landiq_SUBCLASS := trimws(as.character(landiq_SUBCLASS))]
  obs_ok <- adoy_observed(train_m)
  lm_train <- train_m[
    landiq_PFT %in% annual_pfts & !is.na(y_obs) & !is.na(landiq_ADOY) & obs_ok
  ]
  message("[fit] ", name, " LM candidates n=", nrow(lm_train))
  empty_lm <- data.table(
    landiq_CLASS = character(),
    intercept = numeric(),
    slope_adoy = numeric(),
    n = integer()
  )
  if (nrow(lm_train) == 0L) {
    params <- list(table = empty_lm, r_squared = NA_real_, adj_r_squared = NA_real_)
  } else {
    lm_train[, landiq_CLASS := droplevels(landiq_CLASS)]
    lm_fit <- lm(y_obs ~ landiq_ADOY * landiq_CLASS, data = lm_train)
    n_by <- lm_train[, .N, by = landiq_CLASS]
    n_named <- setNames(n_by$N, as.character(n_by$landiq_CLASS))
    params <- lm_class_params(lm_fit, levels(lm_train$landiq_CLASS), n_named)
  }

  means_class_subclass <- train_m[
    !is.na(y_obs) & !is.na(landiq_SUBCLASS) & nzchar(landiq_SUBCLASS),
    .(y_mean = mean(y_obs), n = .N),
    by = .(
      landiq_CLASS = as.character(landiq_CLASS),
      landiq_SUBCLASS = landiq_SUBCLASS,
      landiq_PFT = as.character(landiq_PFT)
    )
  ]
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
  n_mean <- train_m[!is.na(y_obs), .N]
  global_mean <- mean(train_m$y_obs, na.rm = TRUE)

  lm_table <- data.table::copy(params$table)
  lm_table[, `:=`(metric = name, metric_type = type, col = col)]
  means_class_subclass[, metric := name]
  means_class_pft[, metric := name]
  means_class[, metric := name]

  list(
    name = name,
    col = col,
    type = type,
    lm_table = lm_table[, .(
      metric, type = metric_type, col, landiq_CLASS, intercept, slope_adoy, n
    )],
    means_class_subclass = means_class_subclass[, .(
      metric, landiq_CLASS, landiq_SUBCLASS, landiq_PFT, y_mean, n
    )],
    means_class_pft = means_class_pft[, .(metric, landiq_CLASS, landiq_PFT, y_mean, n)],
    means_class = means_class[, .(metric, landiq_CLASS, y_mean, n)],
    means_global = data.table(
      metric = name, type = type, col = col,
      y_mean = global_mean, n = n_mean
    ),
    n_lm = nrow(lm_train),
    n_mean = n_mean,
    r_squared = params$r_squared,
    adj_r_squared = params$adj_r_squared,
    global_mean = global_mean
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
train[, landiq_PFT := tolower(trimws(as.character(landiq_PFT)))]
train[, landiq_ADOY := suppressWarnings(as.numeric(landiq_ADOY))]
train[!is.finite(landiq_ADOY) | landiq_ADOY == 0, landiq_ADOY := NA_real_]

train <- train[landiq_PFT %in% crop_pfts]
message("[fit] crop-PFT training rows: ", nrow(train))
lm_train_n <- train[landiq_PFT %in% annual_pfts, .N]
message("[fit] annual-PFT rows for LM: ", lm_train_n)

metric_models <- lapply(metric_specs, function(spec) {
  out <- fit_one_metric(train, spec)
  gc(verbose = FALSE)
  out
})
names(metric_models) <- vapply(metric_specs, `[[`, character(1), "name")

lm_tab <- rbindlist(lapply(metric_models, `[[`, "lm_table"), use.names = TRUE)
means_cs <- rbindlist(lapply(metric_models, `[[`, "means_class_subclass"), use.names = TRUE)
means_cp <- rbindlist(lapply(metric_models, `[[`, "means_class_pft"), use.names = TRUE)
means_c <- rbindlist(lapply(metric_models, `[[`, "means_class"), use.names = TRUE)
means_g <- rbindlist(lapply(metric_models, `[[`, "means_global"), use.names = TRUE)

created <- as.character(Sys.time())
meta <- list(
  version = "4",
  format = "tables",
  created = created,
  train_years = as.integer(train_years),
  matched_dir = matched_dir,
  date_doy = "year_relative",
  formula = "value ~ landiq_ADOY * landiq_CLASS",
  prediction = "intercept[CLASS] + slope_adoy[CLASS] * landiq_ADOY",
  hierarchy = c(
    "mslsp: keep observed MSLSP metric",
    "lm_adoy: intercept + slope * observed ADOY for row/rice/hay when CLASS in lm table",
    "mean_crop: CLASS x SUBCLASS x PFT, else CLASS x PFT, else CLASS, else global"
  ),
  crop_pfts = crop_pfts,
  metrics = unname(lapply(metric_models, function(m) {
    list(
      name = m$name,
      col = m$col,
      type = m$type,
      n_lm = m$n_lm,
      n_mean = m$n_mean,
      r_squared = m$r_squared,
      adj_r_squared = m$adj_r_squared,
      global_mean = m$global_mean
    )
  }))
)

write_model_outputs(model_dir, meta, lm_tab, means_cs, means_cp, means_c, means_g)
message("[fit] done")
