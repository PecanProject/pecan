# test-get.parameter.samples.R
# Documents the invisible(NULL) return value problem in get.parameter.samples()
#
# get.parameter.samples() ends with save() and has no return() statement,
# so it returns invisible(NULL). This means callers cannot programmatically
# access results without loading the saved file.
# The GSoC project "Refactoring the PEcAn trait meta-analysis workflow"
# aims to fix this by returning a named list instead.



test_that("get.parameter.samples returns invisible(NULL) — documenting the problem", {
  skip(paste0(
    "Requires full PEcAn settings + database connection. ",
    "Current function ends with save() and no return(), ",
    "so it returns invisible(NULL). ",
    "GSoC refactoring will make it return a named list."
  ))
})

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
