# test-check_consistent.R
# Tests for check_consistent() — the prior-data consistency checker
#
# check_consistent() is a pure function with no side effects or DB dependency,
# making it ideal for unit testing. It is called twice inside
# meta_analysis_standalone(): once to check data vs prior, and again to check
# posterior vs prior.

# Note: We use ::: to access internal (non-exported) functions.
# This is standard practice for testing internal functions in R packages.
# See: https://r-pkgs.org/testing-basics.html

test_that("check_consistent returns no_error=TRUE, no_warning=TRUE when point is well within prior", {
  # A normal(0, 10) prior — point at the mean should be perfectly consistent

  prior <- data.frame(distn = "norm", parama = 0, paramb = 10,
                      stringsAsFactors = FALSE)
  result <- PEcAn.MA:::check_consistent(point = 0, prior = prior)

  expect_type(result, "logical")
  expect_named(result, c("no_error", "no_warning"))
  expect_true(result[["no_error"]])
  expect_true(result[["no_warning"]])
})

test_that("check_consistent returns warning (but not error) for moderately extreme points", {

  # normal(0, 1): p(2.1) ≈ 0.982 which is > 0.975 (warning threshold)

  # but < 0.9995 (error threshold)
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1,
                      stringsAsFactors = FALSE)
  result <- PEcAn.MA:::check_consistent(point = 2.1, prior = prior)

  expect_true(result[["no_error"]])
  expect_false(result[["no_warning"]])
})

test_that("check_consistent returns error for extremely inconsistent points", {
  # normal(0, 1): p(5) ≈ 1.0 which exceeds both thresholds
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1,
                      stringsAsFactors = FALSE)
  result <- PEcAn.MA:::check_consistent(point = 5, prior = prior)

  expect_false(result[["no_error"]])
  expect_false(result[["no_warning"]])
})

test_that("check_consistent works symmetrically for low-tail extremes", {
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1,
                      stringsAsFactors = FALSE)
  result <- PEcAn.MA:::check_consistent(point = -5, prior = prior)

  expect_false(result[["no_error"]])
  expect_false(result[["no_warning"]])
})

test_that("check_consistent respects custom p_error and p_warning thresholds", {
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1,
                      stringsAsFactors = FALSE)

  # With very permissive thresholds, even extreme points pass
  result <- PEcAn.MA:::check_consistent(
    point = 3, prior = prior,
    p_error = 1e-10, p_warning = 1e-5
  )
  expect_true(result[["no_error"]])
  expect_true(result[["no_warning"]])
})

test_that("check_consistent validates that p_warning >= p_error", {
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1,
                      stringsAsFactors = FALSE)
  expect_error(
    PEcAn.MA:::check_consistent(point = 0, prior = prior,
                                p_error = 0.05, p_warning = 0.01)
  )
})

test_that("check_consistent works with non-normal prior distributions", {
  # Gamma distribution: dgamma with shape=2, rate=1
  # Median of gamma(2,1) ≈ 1.678
  prior_gamma <- data.frame(distn = "gamma", parama = 2, paramb = 1,
                            stringsAsFactors = FALSE)

  # Point near the mode should be consistent
  result <- PEcAn.MA:::check_consistent(point = 1.5, prior = prior_gamma)
  expect_true(result[["no_error"]])
  expect_true(result[["no_warning"]])

  # Point far in the tail should trigger error
  result_extreme <- PEcAn.MA:::check_consistent(point = 50, prior = prior_gamma)
  expect_false(result_extreme[["no_error"]])
})
