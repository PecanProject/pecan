library(testthat)

test_that("run_benchmark basic works", {
  model <- data.frame(
    time  = as.POSIXct(seq(0, 3600*3, by = 3600), origin = "1970-01-01", tz = "UTC"),
    value = c(1, 2, 3, 4)
  )
  obs <- data.frame(
    time  = as.POSIXct(seq(0, 3600*3, by = 3600), origin = "1970-01-01", tz = "UTC"),
    value = c(1.1, 1.9, 3.2, 3.9)
  )
  tmp1 <- tempfile(fileext = ".csv")
  tmp2 <- tempfile(fileext = ".csv")
  write.csv(model, tmp1, row.names = FALSE)
  write.csv(obs,   tmp2, row.names = FALSE)

  res <- run_benchmark(tmp1, tmp2, metrics = c("RMSE", "MAE"))
  expect_true("metrics" %in% names(res))
  expect_true("aligned" %in% names(res))
  expect_true(nrow(res$metrics) == 2)
})
