# Unit test verifying the validation framework tutorial vignette execution workflow

library(testthat)

test_that("Validation framework tutorial workflow executes deterministically", {
  extdata_dir <- system.file("extdata", package = "PEcAn.benchmark")
  if (extdata_dir == "" || !file.exists(file.path(extdata_dir, "small_salinas_ensemble.csv"))) {
    extdata_dir <- file.path(getwd(), "../../inst/extdata")
  }

  model_csv <- file.path(extdata_dir, "small_salinas_ensemble.csv")
  obs_csv   <- file.path(extdata_dir, "small_salinas_obs.csv")

  expect_true(file.exists(model_csv))
  expect_true(file.exists(obs_csv))

  raw_model <- read.csv(model_csv)
  raw_obs   <- read.csv(obs_csv)

  ens_mat <- efi_long_to_array(raw_model, var = "TotSoilCarb", site = "socs_sys1")
  expect_true(is.matrix(ens_mat))

  model_summary <- data.frame(
    time = attr(ens_mat, "time"),
    value = rowMeans(ens_mat, na.rm = TRUE),
    model_q05 = apply(ens_mat, 1, quantile, probs = 0.05, na.rm = TRUE),
    model_q95 = apply(ens_mat, 1, quantile, probs = 0.95, na.rm = TRUE),
    site = "socs_sys1"
  )

  obs_filtered <- raw_obs[raw_obs$site_id == "socs_sys1" & raw_obs$variable == "TotSoilCarb", ]
  obs_summary <- data.frame(
    time = as.POSIXct(obs_filtered$date, tz = "UTC"),
    value = obs_filtered$obs_mean,
    obvs_sd = obs_filtered$obs_sd,
    site = "socs_sys1"
  )

  aligned_df <- align_by_time(model_summary, obs_summary, tolerance_secs = 365 * 86400 / 2)
  aligned_indices <- match(aligned_df$time, model_summary$time)
  aligned_ens_mat <- ens_mat[aligned_indices, , drop = FALSE]
  attr(aligned_df, "ensemble_matrix") <- aligned_ens_mat

  expect_true(nrow(aligned_df) > 0)

  res <- compute_metrics(aligned_df, metrics = c("BIAS", "RMSE", "MAE", "R2", "COVERAGE", "CRPS"))
  expect_equal(nrow(res), 1)
  expect_true(all(c("BIAS", "RMSE", "MAE", "R2", "COVERAGE", "CRPS") %in% names(res)))

  p_ts <- metric_timeseries_plot(aligned_df, var = "TotSoilCarb")
  expect_s3_class(p_ts, "ggplot")
})
