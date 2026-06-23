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
  expect_equal(nrow(res$metrics), 2)
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
  expect_equal(res$value[res$metric == "RMSE"], 0)
  expect_equal(res$value[res$metric == "MAE"],  0)
  expect_equal(res$value[res$metric == "NSE"],  1)
  expect_equal(res$value[res$metric == "R2"],   1)
})

test_that("register_metric extends the registry", {
  aligned <- data.frame(
    time  = as.POSIXct("2023-01-01", tz="UTC"),
    model = 1,
    obvs  = 1
  )
  register_metric("CUSTOM", function(dat) 999)
  res <- compute_metrics(aligned, c("CUSTOM"))
  expect_equal(res$value[res$metric == "CUSTOM"], 999)
})

test_that("align_by_time matches exact timestamps", {
  aligned <- align_by_time(model_df, obs_df)
  expect_equal(nrow(aligned), 4)
  expect_true(all(c("model", "obvs", "time") %in% names(aligned)))
  expect_equal(aligned$model, c(1, 2, 3, 4))
  expect_equal(aligned$obvs, c(1.1, 1.9, 3.2, 3.9))
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
