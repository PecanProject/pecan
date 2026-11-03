# test-p.point.in.prior.R
# Unit tests for PEcAn.MA:::p.point.in.prior()

# ---------------------------------------------------------------------------
# p.point.in.prior (helper used throughout the pipeline)
# ---------------------------------------------------------------------------



test_that("p.point.in.prior returns correct quantile for normal distribution", {
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1)
  result <- PEcAn.MA:::p.point.in.prior(point = 0, prior = prior)
  expect_equal(result, 0.5)
})

test_that("p.point.in.prior returns correct quantile for extreme values", {
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1)
  result_low <- PEcAn.MA:::p.point.in.prior(point = -5, prior = prior)
  expect_true(result_low < 0.001)
  result_high <- PEcAn.MA:::p.point.in.prior(point = 5, prior = prior)
  expect_true(result_high > 0.999)
})

test_that("p.point.in.prior works with gamma distribution", {
  prior <- data.frame(distn = "gamma", parama = 2, paramb = 1)
  result <- PEcAn.MA:::p.point.in.prior(point = 2, prior = prior)
  expected <- pgamma(2, shape = 2, rate = 1)
  expect_equal(result, expected)
})

test_that("p.point.in.prior returns numeric of length 1", {
  prior <- data.frame(distn = "norm", parama = 0, paramb = 1)
  result <- PEcAn.MA:::p.point.in.prior(point = 1.5, prior = prior)
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(result >= 0 && result <= 1)
})
