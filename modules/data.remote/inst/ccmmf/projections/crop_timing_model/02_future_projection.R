# =====================================================================================
# 02_future_projection.R : run the calibrated model on future climate, 2025-2051.
#
# Uses the Fcrit values from 01_historical_model.R and the same anchor and daily score,
# applied to CalAdapt WRF 45 km daily climate (SSP3-7.0) from 8 downscaled CMIP6 models.
# Fcrit is held fixed, so any change in predicted dates comes from the changing weather.
#
# Needs (from 01_historical_model.R): calibrated_Fcrit.rds, parcel_classes.rds
# Design
#   * one model at a time, one 45 km cell at a time (daily series are not kept in memory)
#   * each finished model is cached to future_<model>.rds, so after a crash or time-out
#     you just run it again and finished models are skipped
#   * parallel over cells using NSLOTS cores
# Test on one model first: Sys.setenv(MODELS = "CESM2") before sourcing (see 00_setup.R)
#
# Outputs (rds): future_<model>.rds, future_predictions_all_models.rds,
#   future_decomposition_all_models.rds, future_ensemble_summary.rds,
#   future_covariate_trends_by_model.rds
# =====================================================================================

source("00_setup.R")
suppressPackageStartupMessages({ library(parallel) })
cat("models:", paste(models, collapse = ", "), "| cores:", n_cores, "\n")

calib_tab <- readRDS(file.path(out_dir, "calibrated_Fcrit.rds"))
sites <- site_weights()$sites          # future-climate cells that contain modelled parcels
cat("cells:", nrow(sites), "\n")

# ---------------------------------------------------------------- one model x one cell
process_site <- function(model, cell_id, lat, lon) {
  fs <- file.path(future_dir, cell_id, sprintf("%s.%s.%d.nc", model, scenario, future_years))
  if (!all(file.exists(fs))) return(list(skip = paste(model, cell_id, "missing files")))
  sd <- bind_rows(lapply(fs, read_daily)) %>%
    filter(is.finite(tmax), is.finite(tmin), is.finite(prcp)) %>%
    distinct(date, .keep_all = TRUE) %>% arrange(date)
  if (nrow(sd) < 365 * 26) return(list(skip = paste(model, cell_id, "too few days:", nrow(sd))))
  d <- sd$date
  S <- build_S(matrix(sd$tmax, 1), matrix(sd$tmin, 1), matrix(sd$prcp, 1), d, lat)
  out <- list()
  for (a in c("c7", "rel")) {
    ct_a <- calib_tab %>% filter(anchor == a)
    for (yr in future_seasons) {
      on <- find_onset(d, sd$tmin, sd$prcp, yr, a)
      if (is.na(on)) {            # no anchor this season: kept as NA so it is counted
        out[[length(out) + 1]] <- data.frame(anchor = a, segment = ct_a$segment, key = ct_a$key, season = yr,
                                             onset_off = NA_real_, days_t = NA_real_, anchored = FALSE)
        next
      }
      st <- match(on, d); on_off <- as.numeric(on - as.Date(sprintf("%d-09-01", yr - 1)))   # days since Sep 1 of Y-1
      k <- vapply(seq_len(nrow(ct_a)), function(i) pred_k(S, 1, st, ct_a$Fcrit[i], ct_a$max_days[i]), 0)
      out[[length(out) + 1]] <- data.frame(anchor = a, segment = ct_a$segment, key = ct_a$key, season = yr,
                                           onset_off = on_off, days_t = k, anchored = TRUE)
    }
  }
  pred <- bind_rows(out) %>% mutate(site = cell_id, lat = lat, lon = lon)
  # yearly climate summaries of the cell, for the climate-trend table
  cov <- sd %>% mutate(yr = as.integer(format(date, "%Y")), mo = as.integer(format(date, "%m")),
                       tmean = (tmax + tmin) / 2) %>%
    group_by(yr) %>%
    summarise(winter_tmin = mean(tmin[mo %in% c(11, 12, 1, 2)]), spring_tmean = mean(tmean[mo %in% 3:5]),
              summer_tmean = mean(tmean[mo %in% 6:8]), annual_tmean = mean(tmean),
              annual_precip = sum(prcp), wet_days = sum(prcp >= PRECIP_THRESH)) %>%
    mutate(site = cell_id)
  list(pred = pred, cov = cov)
}

run_model <- function(model) {
  f <- file.path(out_dir, sprintf("future_%s.rds", model))
  if (file.exists(f)) { cat("cached:", model, "\n"); return(readRDS(f)) }
  cat("Model", model, "started", format(Sys.time()), "\n")
  res <- mclapply(seq_len(nrow(sites)), function(i)
    tryCatch(process_site(model, sites$cell_id[i], sites$cell_lat[i], sites$cell_lon[i]),
             error = function(e) list(skip = paste(model, sites$cell_id[i], conditionMessage(e)))),
    mc.cores = n_cores, mc.preschedule = FALSE)
  ok <- vapply(res, function(r) is.list(r) && !is.null(r$pred), TRUE)
  skips <- unlist(lapply(res[!ok], function(r) if (is.list(r)) r$skip else "worker died"))
  cat("  cells done:", sum(ok), "of", nrow(sites), "| skipped:", sum(!ok), "\n")
  if (length(skips)) print(head(skips, 10))
  out <- list(model = model,
              pred = bind_rows(lapply(res[ok], `[[`, "pred")) %>% mutate(model = model),
              cov  = bind_rows(lapply(res[ok], `[[`, "cov"))  %>% mutate(model = model),
              skipped = skips)
  saveRDS(out, f); cat("Model", model, "finished", format(Sys.time()), "\n"); out
}
results <- lapply(models, run_model)

# ---------------------------------------------------------------- combine and summarise
# total_off = anchor offset + days after the anchor = predicted date in days since Sep 1 of Y-1
# (subtract 122 for an approximate day of year). Negative trends mean earlier dates.
pred_all <- bind_rows(lapply(results, `[[`, "pred")) %>% mutate(total_off = onset_off + days_t)
cov_all  <- bind_rows(lapply(results, `[[`, "cov"))
saveRDS(pred_all, file.path(out_dir, "future_predictions_all_models.rds"))

cat("\n--- share with no anchor / no crossing, by model ---\n")
print(as.data.frame(pred_all %>% group_by(model, anchor, segment) %>%
  summarise(no_anchor = round(mean(!anchored), 3), no_cross = round(mean(is.na(days_t[anchored])), 3))))

# Trend per decade for each model x cell x class: the anchor part, the accumulation part, and the total
decomp_all <- pred_all %>% group_by(model, site, lat, lon, anchor, segment, key) %>%
  summarise(anchor_shift = slope10(onset_off, season), accum_shift = slope10(days_t, season),
            total_shift = slope10(total_off, season), .groups = "drop")
saveRDS(decomp_all, file.path(out_dir, "future_decomposition_all_models.rds"))

model_means <- decomp_all %>% group_by(model, anchor, segment, key) %>%
  summarise(across(c(anchor_shift, accum_shift, total_shift), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
ensemble <- model_means %>% group_by(anchor, segment, key) %>%
  summarise(n_models = n(), anchor_mean = round(mean(anchor_shift), 2), accum_mean = round(mean(accum_shift), 2),
            total_mean = round(mean(total_shift), 2), total_min = round(min(total_shift), 2),
            total_max = round(max(total_shift), 2), n_models_later = sum(total_shift > 0), .groups = "drop")
saveRDS(ensemble, file.path(out_dir, "future_ensemble_summary.rds"))
cat("\n--- ensemble: mean shift across models, days/decade (positive = later) ---\n")
print(as.data.frame(ensemble))

cov_slopes <- cov_all %>% group_by(model, site) %>%
  summarise(across(c(winter_tmin, spring_tmean, summer_tmean, annual_tmean, annual_precip, wet_days),
                   ~ slope10(.x, yr)), .groups = "drop")
cov_model <- cov_slopes %>% group_by(model) %>% summarise(across(-site, ~ round(median(.x, na.rm = TRUE), 2)))
saveRDS(cov_model, file.path(out_dir, "future_covariate_trends_by_model.rds"))
cat("\n--- median cell trend per decade of the climate itself, by model ---\n")
print(as.data.frame(cov_model))
cat("\nDONE. Outputs in", out_dir, "\n")
