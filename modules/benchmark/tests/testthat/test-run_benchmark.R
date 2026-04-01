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
  bad_df <- data.frame(time = c("2023-01-01"), value = c(1.0))
  expect_error(bm_validate(bad_df, obs_df), "POSIXct")
})

test_that("compute_metrics returns correct values", {
  aligned <- data.frame(
    time  = model_df$time,
    model = c(1, 2, 3, 4),
    obs   = c(1, 2, 3, 4)
  )
  res <- compute_metrics(aligned, c("RMSE", "MAE"))
  expect_equal(res$value[res$metric == "RMSE"], 0)
  expect_equal(res$value[res$metric == "MAE"],  0)
})

test_that("align_by_time matches exact timestamps", {
  aligned <- align_by_time(model_df, obs_df)
  expect_equal(nrow(aligned), 4)
  expect_true(all(c("time", "model", "obs") %in% names(aligned)))
})
