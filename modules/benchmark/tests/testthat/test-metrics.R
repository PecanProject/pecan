test_that("metric_RMSE returns 0 for perfect predictions", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_RMSE(dat), 0)
})

test_that("metric_RMSE handles NA values", {
  dat <- data.frame(model = c(1, NA, 3), obvs = c(1, 2, 3))
  expect_equal(metric_RMSE(dat), 0)
})

test_that("metric_RMSE returns numeric", {
  dat <- data.frame(model = c(2, 4), obvs = c(1, 3))
  expect_equal(metric_RMSE(dat), 1)
})

test_that("metric_MAE returns 0 for perfect predictions", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_MAE(dat), 0)
})

test_that("metric_MAE returns correct value", {
  dat <- data.frame(model = c(3, 3), obvs = c(1, 1))
  expect_equal(metric_MAE(dat), 2)
})

test_that("metric_cor returns 1 for perfect linear relationship", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_cor(dat), 1)
})

test_that("metric_R2 returns 1 for perfect predictions", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_R2(dat), 1)
})

test_that("metric_R2 returns NA for constant model output", {
  dat <- data.frame(model = c(2, 2, 2), obvs = c(1, 2, 3))
  expect_warning(
    result <- metric_R2(dat),
    "the standard deviation is zero"
  )
  expect_equal(result, NA_real_)
})
