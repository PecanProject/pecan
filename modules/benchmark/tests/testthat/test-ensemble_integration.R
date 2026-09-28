# Synthetic integration test for Phase 4 Ensemble Pipeline

library(testthat)

test_that("Ensemble benchmarking pipeline runs end-to-end with fast synthetic data", {
  # 1. Create synthetic model ensemble output (POSIXct time, value, ensemble members)
  set.seed(42)
  times <- seq(as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
               as.POSIXct("2024-01-05 00:00:00", tz = "UTC"),
               by = "1 day")

  # 5 ensemble members per timestep
  ens_mat <- matrix(rnorm(length(times) * 5, mean = 50, sd = 5),
                    nrow = length(times), ncol = 5)
  colnames(ens_mat) <- paste0("member_", 1:5)

  model_df <- data.frame(
    time = times,
    value = rowMeans(ens_mat),
    model_q05 = apply(ens_mat, 1, quantile, probs = 0.05),
    model_q95 = apply(ens_mat, 1, quantile, probs = 0.95),
    site = "SiteA"
  )
  attr(model_df, "ensemble_matrix") <- ens_mat

  # 2. Create synthetic observations
  obs_df <- data.frame(
    time = times,
    value = c(49.5, 51.0, 48.2, 52.1, 50.3),
    obvs_sd = c(1.0, 1.2, 0.8, 1.5, 1.1),
    site = "SiteA"
  )

  # 3. Align by time
  aligned <- align_by_time(model_df, obs_df, tolerance_secs = 3600)
  expect_equal(nrow(aligned), 5)
  expect_true("model" %in% names(aligned))
  expect_true("obvs" %in% names(aligned))
  expect_true(all(c("model_q05", "model_q95") %in% names(aligned)))

  # Re-attach ensemble matrix to aligned data for CRPS
  attr(aligned, "ensemble_matrix") <- ens_mat

  # 4. Compute full suite of metrics (mean metrics + ensemble spread metrics)
  metrics_to_test <- c("RMSE", "MAE", "R2", "COVERAGE", "CRPS")
  res <- compute_metrics(aligned, metrics = metrics_to_test)

  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1)
  expect_equal(res$Site, "SiteA")
  expect_true(all(metrics_to_test %in% names(res)))

  # Check that all calculated metrics are valid finite numbers
  for (m in metrics_to_test) {
    expect_false(is.na(res[[m]]))
    expect_true(is.numeric(res[[m]]))
  }
})
