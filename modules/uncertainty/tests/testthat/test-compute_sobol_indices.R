# Tests for compute_sobol_indices

make_sobol_design <- function(n = 50) {
  sensitivity::soboljansen(
    model = NULL,
    X1 = data.frame(a = stats::runif(n), b = stats::runif(n)),
    X2 = data.frame(a = stats::runif(n), b = stats::runif(n))
  )
}

# same layout get.results() saves: one value per run, in run order
save_ensemble_output <- function(y) {
  ensemble.output <- stats::setNames(as.list(y), seq_along(y))
  path <- withr::local_tempfile(fileext = ".Rdata", .local_envir = parent.frame())
  save(ensemble.output, file = path)
  path
}

test_that("tell() gets the saved values in run order", {
  set.seed(1)
  design <- make_sobol_design()
  y <- design$X$a + 2 * design$X$b
  path <- save_ensemble_output(y)

  told <- compute_sobol_indices(path, design)

  expect_equal(told$y, y)
})

test_that("nboot gives intervals for a design built without them", {
  set.seed(1)
  design <- make_sobol_design()
  y <- design$X$a + 2 * design$X$b
  path <- save_ensemble_output(y)
  booted <- sensitivity::soboljansen(
    model = NULL, X1 = design$X1, X2 = design$X2, nboot = 100
  )

  set.seed(2)
  told <- compute_sobol_indices(path, design, nboot = 100)
  set.seed(2)
  expected <- sensitivity::tell(booted, y)

  expect_equal(told$S, expected$S)
  expect_equal(told$T, expected$T)
})

test_that("output that does not match the design fails", {
  set.seed(1)
  design <- make_sobol_design()
  path <- save_ensemble_output(stats::rnorm(nrow(design$X) - 1))

  expect_error(compute_sobol_indices(path, design), "length(y) == nrow(sobol_design$X)", fixed = TRUE)
})
