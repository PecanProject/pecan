# Test file for BIAS metric (Mean Bias Error)

test_that("metric_Bias calculates zero bias when model equals obvs", {
  dat <- data.frame(model = c(10, 20, 30), obvs = c(10, 20, 30))
  score <- metric_Bias(dat)
  expect_equal(score, 0)
})

test_that("metric_Bias calculates positive bias when model overestimates", {
  dat <- data.frame(model = c(12, 22, 32), obvs = c(10, 20, 30))
  score <- metric_Bias(dat)
  expect_equal(score, 2)
})

test_that("metric_Bias calculates negative bias when model underestimates", {
  dat <- data.frame(model = c(8, 18, 28), obvs = c(10, 20, 30))
  score <- metric_Bias(dat)
  expect_equal(score, -2)
})

test_that("metric_Bias is registered in pecan_metric_registry", {
  expect_true(exists("BIAS", envir = pecan_metric_registry))
})
