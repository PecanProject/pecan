# Tests for compute_sobol_indices

make_sobol_run <- function(n = 50) {
  outdir <- withr::local_tempdir(.local_envir = parent.frame())
  settings <- PEcAn.settings::as.Settings(list(
    outdir = outdir,
    ensemble = list(ensemble.id = "E1", start.year = 2001, end.year = 2002)
  ))
  sobol_obj <- sensitivity::soboljansen(
    model = NULL,
    X1 = data.frame(a = stats::runif(n), b = stats::runif(n)),
    X2 = data.frame(a = stats::runif(n), b = stats::runif(n))
  )
  list(settings = settings, sobol_obj = sobol_obj)
}

# same layout get.results() writes: one mean per run, in run order
save_ensemble_output <- function(settings, y, variable = "NEE") {
  ensemble.output <- stats::setNames(as.list(y), seq_along(y))
  save(ensemble.output, file = ensemble.filename(
    settings, "ensemble.output", "Rdata", all.var.yr = FALSE,
    variable = variable, start.year = 2001, end.year = 2002
  ))
}

test_that("indices come from the ensemble output get.results() saved", {
  set.seed(1)
  run <- make_sobol_run()
  y <- run$sobol_obj$X$a + 2 * run$sobol_obj$X$b
  save_ensemble_output(run$settings, y)

  told <- compute_sobol_indices(run$settings, run$sobol_obj, "NEE")

  expect_true(inherits(told, "soboljansen"))
  expect_equal(told$S, sensitivity::tell(run$sobol_obj, y)$S)
  expect_equal(told$T, sensitivity::tell(run$sobol_obj, y)$T)
})

test_that("nboot gives intervals for a design built without them", {
  set.seed(1)
  run <- make_sobol_run()
  y <- run$sobol_obj$X$a + 2 * run$sobol_obj$X$b
  save_ensemble_output(run$settings, y)
  booted <- sensitivity::soboljansen(
    model = NULL, X1 = run$sobol_obj$X1, X2 = run$sobol_obj$X2, nboot = 100
  )

  set.seed(2)
  told <- compute_sobol_indices(run$settings, run$sobol_obj, "NEE", nboot = 100)
  set.seed(2)
  expected <- sensitivity::tell(booted, y)

  expect_true(all(c("min. c.i.", "max. c.i.") %in% colnames(told$S)))
  expect_equal(told$S, expected$S)
  expect_equal(told$T, expected$T)
})

test_that("output that does not match the design fails", {
  set.seed(1)
  run <- make_sobol_run()
  save_ensemble_output(run$settings, stats::rnorm(nrow(run$sobol_obj$X) - 1))

  expect_error(compute_sobol_indices(run$settings, run$sobol_obj, "NEE"), "rows")
})

test_that("missing output fails", {
  set.seed(1)
  run <- make_sobol_run()

  expect_error(compute_sobol_indices(run$settings, run$sobol_obj, "NEE"), "get.results")
})

test_that("multisite settings are refused", {
  set.seed(1)
  run <- make_sobol_run()
  multi <- PEcAn.settings::MultiSettings(run$settings, run$settings)

  expect_error(compute_sobol_indices(multi, run$sobol_obj, "NEE"), "once per site")
})

test_that("a run with no output fails instead of giving NA indices", {
  set.seed(1)
  run <- make_sobol_run()
  y <- run$sobol_obj$X$a + 2 * run$sobol_obj$X$b
  y[3] <- NA
  save_ensemble_output(run$settings, y)

  expect_error(compute_sobol_indices(run$settings, run$sobol_obj, "NEE"), "runs with no NEE output: 1")
})

test_that("a derived variable is read under its left-hand side", {
  set.seed(1)
  run <- make_sobol_run()
  y <- run$sobol_obj$X$a + 2 * run$sobol_obj$X$b
  save_ensemble_output(run$settings, y, variable = "Diff")

  told <- compute_sobol_indices(run$settings, run$sobol_obj, "Diff=GPP-NEE")

  expect_equal(told$S, sensitivity::tell(run$sobol_obj, y)$S)
})

test_that("settings without ensemble years find the output get.results() saved", {
  set.seed(1)
  run <- make_sobol_run()
  run$settings$ensemble$start.year <- NULL
  run$settings$ensemble$end.year <- NULL
  y <- run$sobol_obj$X$a + 2 * run$sobol_obj$X$b
  ensemble.output <- stats::setNames(as.list(y), seq_along(y))
  save(ensemble.output, file = ensemble.filename(
    run$settings, "ensemble.output", "Rdata", all.var.yr = FALSE,
    variable = "NEE", start.year = NA, end.year = NA
  ))

  told <- compute_sobol_indices(run$settings, run$sobol_obj, "NEE")

  expect_equal(told$S, sensitivity::tell(run$sobol_obj, y)$S)
})
