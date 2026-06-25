# ============================================================
# Tests for the get.parameter.samples() wrapper (Layer 2)
#
# These exercise the WRAPPER's behaviour, not the pure
# get_parameter_samples() (covered in test-get_parameter_samples.R).
#
# Focus: the `outdir` argument added in GSoC 2026 Week 3, which
# replaced the `save_to_disk` flag from #3860:
#   * default (outdir = settings$outdir) -> writes samples.Rdata as before
#   * explicit outdir                    -> writes there instead
#   * outdir = NULL                      -> no save
#   * the saved file keeps the 5-object name contract
#   * the result list is still returned invisibly
#   * the loaded posteriors are still delegated to the pure function
#
# load.posteriors() and get_parameter_samples() are stubbed with
# mockery so these run with no database and no real MCMC objects.
# ============================================================

skip_if_not_installed("mockery")
skip_if_not_installed("PEcAn.logger")
skip_if_not_installed("withr")

# ---- Helper: minimal single-PFT settings (no $database -> no DB calls) ----
make_test_settings <- function(outdir) {
  list(
    pfts = list(list(name = "temperate.deciduous", outdir = outdir)),
    outdir = outdir
  )
}

# ---- Helper: canned load.posteriors() return ----
fake_posterior <- function() {
  list(
    prior.distns = data.frame(
      distn = "norm", parama = 20, paramb = 5, n = 50,
      row.names = "SLA", stringsAsFactors = FALSE
    ),
    trait.mcmc = NULL,
    is.joint = FALSE
  )
}

# ---- Helper: canned get_parameter_samples() return ----
fake_samples_result <- function() {
  list(
    trait.samples    = list(),
    sa.samples       = list(),
    ensemble.samples = list(),
    runs.samples     = list(),
    env.samples      = list()
  )
}


# Save location is driven by `outdir`

test_that("samples.Rdata is written to settings$outdir by default", {
  tmp <- withr::local_tempdir()
  settings <- make_test_settings(tmp)

  mockery::stub(get.parameter.samples, "load.posteriors", fake_posterior())
  mockery::stub(get.parameter.samples, "get_parameter_samples", fake_samples_result())

  suppressWarnings(get.parameter.samples(settings))

  expect_true(file.exists(file.path(tmp, "samples.Rdata")))
})


test_that("explicit outdir overrides settings$outdir for the save", {
  tmp_settings <- withr::local_tempdir()
  tmp_explicit <- withr::local_tempdir()
  settings <- make_test_settings(tmp_settings)

  mockery::stub(get.parameter.samples, "load.posteriors", fake_posterior())
  mockery::stub(get.parameter.samples, "get_parameter_samples", fake_samples_result())

  suppressWarnings(get.parameter.samples(settings, outdir = tmp_explicit))

  expect_true(file.exists(file.path(tmp_explicit, "samples.Rdata")))
  expect_false(file.exists(file.path(tmp_settings, "samples.Rdata")))
})


test_that("outdir = NULL skips the save entirely", {
  tmp <- withr::local_tempdir()
  settings <- make_test_settings(tmp)

  mockery::stub(get.parameter.samples, "load.posteriors", fake_posterior())
  mockery::stub(get.parameter.samples, "get_parameter_samples", fake_samples_result())

  suppressWarnings(get.parameter.samples(settings, outdir = NULL))

  expect_false(file.exists(file.path(tmp, "samples.Rdata")))
})


# Saved file keeps the downstream name contract

test_that("samples.Rdata bundles the 5 expected objects", {
  # Downstream consumers (run.write.configs, get.results,
  # run.sensitivity.analysis, read.ensemble.ts) read these exact names,
  # so the save contract must not drift.
  tmp <- withr::local_tempdir()
  settings <- make_test_settings(tmp)

  mockery::stub(get.parameter.samples, "load.posteriors", fake_posterior())
  mockery::stub(get.parameter.samples, "get_parameter_samples", fake_samples_result())

  suppressWarnings(get.parameter.samples(settings))

  e <- new.env()
  load(file.path(tmp, "samples.Rdata"), envir = e)
  expect_setequal(
    ls(e),
    c("ensemble.samples", "trait.samples", "sa.samples",
      "runs.samples", "env.samples")
  )
})


# Return contract

test_that("returns the get_parameter_samples result invisibly", {
  tmp <- withr::local_tempdir()
  settings <- make_test_settings(tmp)
  fake <- fake_samples_result()

  mockery::stub(get.parameter.samples, "load.posteriors", fake_posterior())
  mockery::stub(get.parameter.samples, "get_parameter_samples", fake)

  # withVisible must sit INSIDE suppressWarnings, so it captures the
  # wrapper's own visibility rather than suppressWarnings()'s.
  vis <- suppressWarnings(
    withVisible(get.parameter.samples(settings, outdir = NULL))
  )

  expect_identical(vis$value, fake)
  expect_false(vis$visible)
})

# Delegation still wired correctly after the edit

test_that("loaded posteriors are delegated to get_parameter_samples", {
  tmp <- withr::local_tempdir()
  settings <- make_test_settings(tmp)
  post <- fake_posterior()

  mockery::stub(get.parameter.samples, "load.posteriors", post)
  mock_gps <- mockery::mock(fake_samples_result())
  mockery::stub(get.parameter.samples, "get_parameter_samples", mock_gps)

  suppressWarnings(get.parameter.samples(settings, outdir = NULL))

  mockery::expect_called(mock_gps, 1)
  args <- mockery::mock_args(mock_gps)[[1]]
  expect_identical(args$prior_distns_list[[1]], post$prior.distns)
  expect_true(args$independent)
})