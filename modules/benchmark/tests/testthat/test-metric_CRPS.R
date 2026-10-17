# Test file for CRPS metric (Continuous Ranked Probability Score)

library(testthat)

test_that("metric_CRPS calculates perfect score for point mass ensemble matching obs", {
  # When all ensemble members equal observation, CRPS is 0
  ens <- matrix(c(10, 10, 10,
                  20, 20, 20), nrow = 2, byrow = TRUE)
  dat <- data.frame(obvs = c(10, 20))
  attr(dat, "ensemble_matrix") <- ens

  score <- metric_CRPS(dat)
  expect_equal(score, 0)
})

test_that("metric_CRPS calculates correct score with wide member columns", {
  dat <- data.frame(
    obvs = c(10, 20),
    member_1 = c(8, 18),
    member_2 = c(12, 22)
  )

  score <- metric_CRPS(dat)
  expect_true(is.numeric(score))
  expect_true(score > 0)
})

test_that("metric_CRPS calculates expected score for known sample spread", {
  # Obs = 10. Samples = c(8, 12).
  # E|X - y| = mean(|8-10|, |12-10|) = 2.
  # E|X - X'| = mean(|8-8|, |8-12|, |12-8|, |12-12|) = (0 + 4 + 4 + 0) / 4 = 2.
  # CRPS = 2 - 0.5 * 2 = 1.0.
  ens <- matrix(c(8, 12), nrow = 1)
  dat <- data.frame(obvs = 10)
  attr(dat, "ensemble_matrix") <- ens

  score <- metric_CRPS(dat)
  expect_equal(score, 1.0)
})

test_that("metric_CRPS is registered in pecan_metric_registry", {
  ens <- matrix(c(8, 12), nrow = 1)
  dat <- data.frame(obvs = 10, model = 10)
  attr(dat, "ensemble_matrix") <- ens

  crps_fn <- get("CRPS", envir = pecan_metric_registry)
  expect_true(is.function(crps_fn))
  expect_equal(crps_fn(dat), 1.0)
})
