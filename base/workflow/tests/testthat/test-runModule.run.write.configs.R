# Tests for .prepare_input_designs (internal helper of
# runModule.run.write.configs). Focus: now that the generator samples in
# memory, this helper samples once, writes samples.Rdata for the downstream
# analysis steps that still read it, and hands the same samples to the
# generator so nothing resamples.

skip_if_not_installed("mockery")
skip_if_not_installed("withr")

make_prep_settings <- function(outdir) {
  list(
    outdir   = outdir,
    pfts     = list(list(name = "temperate.deciduous")),
    ensemble = list(
      size = 3,
      samplingspace = list(parameters = list(method = "uniform"))
    )
  )
}

fake_loaded <- function() {
  list(
    pft_names         = list("temperate.deciduous"),
    prior_distns_list = list(NULL),
    trait_mcmc_list   = list(NULL),
    independent       = TRUE
  )
}

fake_bundle <- function() {
  list(
    trait.samples    = list(temperate.deciduous = list(SLA = 1:3)),
    sa.samples       = list(),
    ensemble.samples = list(temperate.deciduous = data.frame(SLA = 1:3)),
    runs.samples     = list(),
    env.samples      = list()
  )
}


test_that(".prepare_input_designs writes samples.Rdata with the 5-object contract", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::load_pft_posteriors",
                function(...) fake_loaded())
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::get_parameter_samples",
                function(...) fake_bundle())
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design",
                function(...) list(X = data.frame(param = 1:3)))

  .prepare_input_designs(settings, input_design = NULL)

  samples_file <- file.path(tmp, "samples.Rdata")
  expect_true(file.exists(samples_file))

  e <- new.env()
  load(samples_file, envir = e)
  expect_setequal(
    ls(e),
    c("ensemble.samples", "trait.samples", "sa.samples",
      "runs.samples", "env.samples")
  )
})


test_that(".prepare_input_designs hands the sampled bundle to the generator", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  bundle <- fake_bundle()
  gen <- mockery::mock(list(X = data.frame(param = 1:3)))
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::load_pft_posteriors",
                function(...) fake_loaded())
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::get_parameter_samples",
                function(...) bundle)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design", gen)

  designs <- .prepare_input_designs(settings, input_design = NULL)

  mockery::expect_called(gen, 1)
  passed <- mockery::mock_args(gen)[[1]]
  expect_identical(passed$samples, bundle)
  expect_equal(designs$ensemble, data.frame(param = 1:3))
  # the bundle is also threaded onto designs for run.write.configs to receive
  expect_identical(designs$samples, bundle)
})


test_that(".prepare_input_designs returns an input_design list unchanged", {
  gen <- mockery::mock()
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design", gen)

  normalized <- list(ensemble = data.frame(param = 1:2), sensitivity = NULL)
  result <- .prepare_input_designs(list(outdir = "."), input_design = normalized)

  expect_identical(result, normalized)
  mockery::expect_called(gen, 0)
})
