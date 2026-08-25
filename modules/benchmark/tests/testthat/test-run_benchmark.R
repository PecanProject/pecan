library(testthat)

model_df <- data.frame(
  time  = as.POSIXct(seq(0, 3600*3, by = 3600), origin = "1970-01-01", tz = "UTC"),
  value = c(1, 2, 3, 4)
)
obs_df <- data.frame(
  time  = as.POSIXct(seq(0, 3600*3, by = 3600), origin = "1970-01-01", tz = "UTC"),
  value = c(1.1, 1.9, 3.2, 3.9)
)

test_that("run_benchmark returns correct structure", {
  res <- run_benchmark(model_df, obs_df, metrics = c("RMSE", "MAE"))
  expect_true("metrics" %in% names(res))
  expect_true("aligned" %in% names(res))
  expect_true("plot"    %in% names(res))
  expect_equal(nrow(res$metrics), 1)
})

test_that("bm_validate rejects bad input", {
  bad_type_df <- data.frame(time = c("2023-01-01"), value = c(1.0))
  expect_error(bm_validate(bad_type_df, obs_df), "POSIXct")
  
  missing_time_df <- data.frame(timestamp = as.POSIXct("2023-01-01"), value = c(1.0))
  expect_error(bm_validate(missing_time_df, obs_df), "Missing required column: 'time'")
  
  missing_val_df <- data.frame(time = as.POSIXct("2023-01-01"), val = c(1.0))
  expect_error(bm_validate(missing_val_df, obs_df), "Missing required column: 'value'")
})

test_that("compute_metrics returns correct values", {
  aligned <- data.frame(
    time  = model_df$time,
    model = c(1, 2, 3, 4),
    obvs  = c(1, 2, 3, 4)
  )
  res <- compute_metrics(aligned, c("RMSE", "MAE", "NSE", "R2"))
  expect_equal(res$RMSE[1], 0)
  expect_equal(res$MAE[1],  0)
  expect_equal(res$NSE[1],  1)
  expect_equal(res$R2[1],   1)
})

test_that("register_metric extends the registry", {
  aligned <- data.frame(
    time  = as.POSIXct("2023-01-01", tz="UTC"),
    model = 1,
    obvs  = 1
  )
  register_metric("CUSTOM", function(dat) 999)
  res <- compute_metrics(aligned, c("CUSTOM"))
  expect_equal(res$CUSTOM[1], 999)
})

test_that("align_by_time matches exact timestamps", {
  aligned <- align_by_time(model_df, obs_df)
  expect_equal(nrow(aligned), 4)
  expect_true(all(c("model", "obvs", "time") %in% names(aligned)))
  expect_equal(aligned$model, c(1, 2, 3, 4))
  expect_equal(aligned$obvs, c(1.1, 1.9, 3.2, 3.9))
})

test_that("align_by_time pairs each observation with exactly one model prediction", {
  monthly_model <- data.frame(
    time = as.POSIXct(seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month")),
    value = 1:12
  )
  annual_obs <- data.frame(
    time = as.POSIXct(c("2020-06-01", "2020-11-01")),
    value = c(6.1, 11.2)
  )

  aligned <- align_by_time(monthly_model, annual_obs, tolerance_secs = 365 * 86400)

  expect_equal(nrow(aligned), nrow(annual_obs))
  expect_equal(length(unique(aligned$obs_time)), nrow(annual_obs))
})

test_that("compute_metrics correctly subsets ensemble_matrix by site for CRPS", {
  aligned <- data.frame(
    time = as.POSIXct(c("2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01"), tz = "UTC"),
    model = c(10, 20, 100, 200),
    obvs  = c(10, 20, 100, 200),
    site  = c("SiteA", "SiteA", "SiteB", "SiteB")
  )

  ens_mat <- matrix(c(
    9, 11,
    19, 21,
    98, 102,
    198, 202
  ), nrow = 4, byrow = TRUE)

  attr(aligned, "ensemble_matrix") <- ens_mat

  res <- compute_metrics(aligned, metrics = c("CRPS"))

  expect_equal(nrow(res), 4) # SiteA, SiteB, Rollup (Mean), Rollup (Median)
  expect_true(all(c("SiteA", "SiteB") %in% res$Site))
  expect_true(is.numeric(res$CRPS))
  expect_false(any(is.na(res$CRPS)))
})

test_that("align_by_time drops points outside tolerance", {
  model_df_tol <- data.frame(
    time = as.POSIXct(c(0, 3600), origin = "1970-01-01", tz = "UTC"),
    value = c(1, 2)
  )
  obs_df_tol <- data.frame(
    time = as.POSIXct(c(10, 5000), origin = "1970-01-01", tz = "UTC"),
    value = c(1.1, 2.1)
  )
  
  # Tolerance of 20 seconds should keep the first point (diff=10s) but drop the second (diff=1400s)
  aligned <- align_by_time(model_df_tol, obs_df_tol, tolerance_secs = 20)
  expect_equal(nrow(aligned), 1)
  expect_equal(aligned$model, 1)
  expect_equal(aligned$obvs, 1.1)
})

test_that("align_by_time passes through metadata columns", {
  model_df_meta <- data.frame(
    time = as.POSIXct(c(0, 3600), origin = "1970-01-01", tz = "UTC"),
    value = c(1, 2),
    model_q025 = c(0.5, 1.5),
    model_q975 = c(1.5, 2.5)
  )
  obs_df_meta <- data.frame(
    time = as.POSIXct(c(0, 3600), origin = "1970-01-01", tz = "UTC"),
    value = c(1.1, 1.9),
    obs_se = c(0.1, 0.2),
    obs_n = c(3, 3)
  )
  
  aligned <- align_by_time(model_df_meta, obs_df_meta)
  expect_true(all(c("model_q025", "model_q975", "obs_se", "obs_n") %in% names(aligned)))
})

test_that("metric_Coverage calculates correct fraction", {
  aligned <- data.frame(
    model = c(1, 2, 3),
    obvs = c(1, 4, 3),
    model_q025 = c(0.5, 1.5, 2.5),
    model_q975 = c(1.5, 2.5, 3.5)
  )
  
  expect_equal(metric_Coverage(aligned), 2/3)
})

test_that("metric_PMU calculates correct pooled uncertainty", {
  aligned <- data.frame(
    obs_se = c(0.1, 0.2),
    obs_n = c(3, 5)
  )
  
  expect_equal(metric_PMU(aligned), sqrt(0.23/8))
})
