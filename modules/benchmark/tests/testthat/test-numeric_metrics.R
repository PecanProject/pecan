# Test file for remaining numeric metrics (Issue #4027)

library(testthat)

# 1. metric_MSE
test_that("metric_MSE returns 0 for perfect predictions", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_MSE(dat), 0)
})

test_that("metric_MSE handles NA values correctly", {
  # na.rm = TRUE means the NA row is ignored
  dat <- data.frame(model = c(1, NA, 3), obvs = c(1, 2, 3))
  # remaining errors are 0 and 0. Mean squared error is 0.
  expect_equal(metric_MSE(dat), 0)
})

test_that("metric_MSE returns correct known value", {
  dat <- data.frame(model = c(2, 4), obvs = c(1, 2))
  # Errors: -1, -2. Squared errors: 1, 4. Mean: 2.5
  expect_equal(metric_MSE(dat), 2.5)
})

# 2. metric_AME
test_that("metric_AME returns 0 for perfect predictions", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_AME(dat), 0)
})

test_that("metric_AME handles NA values correctly", {
  dat <- data.frame(model = c(1, NA, 10), obvs = c(2, 100, 4))
  # absolute errors: 1, NA, 6. Max is 6.
  expect_equal(metric_AME(dat), 6)
})

test_that("metric_AME returns correct known value", {
  dat <- data.frame(model = c(2, 10), obvs = c(1, 2))
  # absolute errors: 1, 8. Max is 8.
  expect_equal(metric_AME(dat), 8)
})

# 3. metric_PPMC
test_that("metric_PPMC returns 1 for perfect linear relationship", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_PPMC(dat), 1)
})

test_that("metric_PPMC handles NA values correctly", {
  dat <- data.frame(model = c(1, 2, NA, 4), obvs = c(2, 4, 100, 8))
  # Uses pairwise.complete.obs, so the NA row is ignored. Remaining is perfectly linear.
  expect_equal(metric_PPMC(dat), 1)
})

test_that("metric_PPMC returns correct known value", {
  # simple known correlation case
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 3, 2))
  # Cor(c(1,2,3), c(1,3,2)) = 0.5
  expect_equal(metric_PPMC(dat), 0.5)
})

# 4. metric_RAE
test_that("metric_RAE returns 0 for perfect predictions", {
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_RAE(dat), 0)
})

test_that("metric_RAE handles NA values correctly", {
  dat <- data.frame(model = c(1, NA, 3, 4), obvs = c(1, 100, 3, 4))
  # Uses na.omit internally, remaining data is perfect prediction
  expect_equal(metric_RAE(dat), 0)
})

test_that("metric_RAE returns correct known value", {
  dat <- data.frame(model = c(2, 4, 6), obvs = c(1, 2, 3))
  # obvs mean = 2
  # abs(obvs - mean(obvs)) = c(1, 0, 1), mean = 2/3
  # abs(obvs - model) = c(1, 2, 3), mean = 6/3 = 2
  # RAE = 2 / (2/3) = 3
  expect_equal(metric_RAE(dat), 3)
})

# 5. metric_Frechet
test_that("metric_Frechet returns 0 for perfect predictions", {
  skip_if_not_installed("SimilarityMeasures")
  dat <- data.frame(model = c(1, 2, 3), obvs = c(1, 2, 3))
  expect_equal(metric_Frechet(dat), 0)
})

test_that("metric_Frechet handles NA values correctly", {
  skip_if_not_installed("SimilarityMeasures")
  dat <- data.frame(model = c(1, NA, 3), obvs = c(1, 100, 3))
  # Uses na.omit internally, remaining data is perfect
  expect_equal(metric_Frechet(dat), 0)
})

test_that("metric_Frechet returns correct known value", {
  skip_if_not_installed("SimilarityMeasures")
  dat <- data.frame(model = c(1, 2), obvs = c(1, 3))
  # Frechet distance between matrix(c(1,3)) and matrix(c(1,2))
  # Distance is max(|1-1|, |3-2|) = max(0, 1) = 1
  expect_equal(metric_Frechet(dat), 1)
})
