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

# contract added when the design must carry its samples  .

test_that(".prepare_input_designs rejects a bare design supplied without its samples", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  # A design's `param` column only indexes into the samples it was drawn with,
  # so a design supplied without those samples must error rather than be paired
  # with a fresh, mismatched resample.
  expect_error(
    .prepare_input_designs(settings, input_design = data.frame(param = 1:3)),
    "without its parameter samples"
  )
})


test_that(".prepare_input_designs reuses a supplied list(X, samples) and does not resample", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  bundle   <- fake_bundle()
  supplied <- list(X = data.frame(param = 1:3, met = c(2, 1, 2)), samples = bundle)

  loader <- mockery::mock()   # posteriors must NOT be loaded
  gps    <- mockery::mock()   # parameters must NOT be resampled
  gen    <- mockery::mock()   # the design must NOT be regenerated
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::load_pft_posteriors", loader)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::get_parameter_samples", gps)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design", gen)

  designs <- .prepare_input_designs(settings, input_design = supplied)

  mockery::expect_called(loader, 0)
  mockery::expect_called(gps, 0)
  mockery::expect_called(gen, 0)
  expect_identical(designs$ensemble, supplied$X)
  expect_identical(designs$samples, bundle)
  # samples.Rdata is still written for the downstream analysis steps.
  expect_true(file.exists(file.path(tmp, "samples.Rdata")))
})


test_that(".prepare_input_designs threads SA samples into the OAT generator", {
  tmp <- withr::local_tempdir()
  settings <- list(
    outdir = tmp,
    pfts   = list(list(name = "temperate.deciduous")),
    sensitivity.analysis = list(quantiles = c(0.025, 0.5, 0.975))
  )

  sa_bundle <- list(
    trait.samples    = list(temperate.deciduous = list(SLA = 1:3)),
    sa.samples       = list(temperate.deciduous = matrix(1:6, 3, 2)),
    ensemble.samples = list(),
    runs.samples     = list(),
    env.samples      = list()
  )

  gps <- mockery::mock(sa_bundle)
  oat <- mockery::mock(list(X = data.frame(param = 1:4)))
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::load_pft_posteriors",
                function(...) fake_loaded())
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::get_parameter_samples", gps)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_OAT_SA_design", oat)

  designs <- .prepare_input_designs(settings, input_design = NULL)

  # SA-only run: parameters are sampled without an ensemble
  gps_args <- mockery::mock_args(gps)[[1]]
  expect_false(gps_args$do_ensemble)

  # the OAT generator receives the full sample bundle directly (no disk
  # re-read), and uses sa.samples from it
  oat_args <- mockery::mock_args(oat)[[1]]
  # generate_OAT_SA_design(settings, samples = designs$samples): settings is
  # positional (first), the bundle comes through named `samples`
  # OAT is handed the resolved bundle itself (so it never re-reads samples.Rdata)
  expect_equal(oat_args$samples, designs$samples)

  expect_equal(designs$sensitivity, data.frame(param = 1:4))
  expect_true(file.exists(file.path(tmp, "samples.Rdata")))
})


test_that(".prepare_input_designs rejects a design whose samples are NULL", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  loader <- mockery::mock()
  gps    <- mockery::mock()
  gen    <- mockery::mock()
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::load_pft_posteriors", loader)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::get_parameter_samples", gps)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design", gen)

  # with the loader stubbed out too, the only thing that can raise here is the guard
  expect_error(
    .prepare_input_designs(settings, list(X = data.frame(param = 1:3), samples = NULL)),
    "Unrecognized input_design format"
  )
  mockery::expect_called(loader, 0)
  mockery::expect_called(gps, 0)
  mockery::expect_called(gen, 0)
})

test_that(".prepare_input_designs warns when it auto-generates a design", {
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

  msgs <- capture.output(
    invisible(.prepare_input_designs(settings, input_design = NULL)),
    type = "message"
  )
  expect_match(paste(msgs, collapse = "\n"), "deprecated", all = FALSE)
})

test_that(".prepare_input_designs does not warn when a design is supplied", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  supplied <- list(X = data.frame(param = 1:3), samples = fake_bundle())

  msgs <- capture.output(
    invisible(.prepare_input_designs(settings, input_design = supplied)),
    type = "message"
  )
  expect_false(any(grepl("deprecated", msgs)))
})

test_that(".prepare_input_designs accepts a design supplied as design_matrix", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  bundle   <- fake_bundle()
  supplied <- list(design_matrix = data.frame(param = 1:3), samples = bundle)

  gen <- mockery::mock()
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design", gen)

  designs <- .prepare_input_designs(settings, input_design = supplied)

  mockery::expect_called(gen, 0)
  expect_identical(designs$ensemble, supplied$design_matrix)
  expect_identical(designs$samples, bundle)
})

test_that(".prepare_input_designs still accepts a design supplied as X", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)

  bundle   <- fake_bundle()
  supplied <- list(X = data.frame(param = 1:3), samples = bundle)

  gen <- mockery::mock()
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design", gen)

  designs <- .prepare_input_designs(settings, input_design = supplied)

  mockery::expect_called(gen, 0)
  expect_identical(designs$ensemble, supplied$X)
})

test_that(".prepare_input_designs warns only about the design it generates itself", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)
  settings$sensitivity.analysis <- list(quantiles = c(0.025, 0.5, 0.975))

  supplied <- list(design_matrix = data.frame(param = 1:3), samples = fake_bundle())

  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_OAT_SA_design",
                function(...) list(design_matrix = data.frame(param = 1:4)))

  msgs <- capture.output(
    invisible(.prepare_input_designs(settings, input_design = supplied)),
    type = "message"
  )
  joined <- paste(msgs, collapse = "\n")

  # the ensemble design came from the caller, so nothing to warn about there;
  # the SA design is still generated here, and now that one can be supplied
  # too, that generation is worth warning about
  expect_false(grepl("ensemble design internally", joined))
  expect_match(joined, "sensitivity analysis design", all = FALSE)
})

# -- routing: an SA design can now be supplied, one call per run ----

make_sa_settings <- function(outdir) {
  list(
    outdir = outdir,
    pfts   = list(list(name = "temperate.deciduous")),
    sensitivity.analysis = list(quantiles = c(0.025, 0.5, 0.975))
  )
}

test_that(".prepare_input_designs takes a supplied design as the SA design when the settings are SA only", {
  tmp <- withr::local_tempdir()
  settings <- make_sa_settings(tmp)

  bundle   <- fake_bundle()
  supplied <- list(design_matrix = data.frame(param = 1:7), samples = bundle)

  loader <- mockery::mock()
  gps    <- mockery::mock()
  oat    <- mockery::mock()
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::load_pft_posteriors", loader)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::get_parameter_samples", gps)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_OAT_SA_design", oat)

  designs <- .prepare_input_designs(settings, input_design = supplied)

  expect_identical(designs$sensitivity, supplied$design_matrix)
  expect_null(designs$ensemble)
  mockery::expect_called(oat, 0)
  mockery::expect_called(loader, 0)
  mockery::expect_called(gps, 0)
  # the bundle still reaches disk for the downstream analysis steps
  expect_true(file.exists(file.path(tmp, "samples.Rdata")))
})


test_that(".prepare_input_designs still takes a supplied design as the ensemble design when the settings have an ensemble", {
  tmp <- withr::local_tempdir()
  settings <- make_prep_settings(tmp)
  settings$sensitivity.analysis <- list(quantiles = c(0.025, 0.5, 0.975))

  supplied <- list(design_matrix = data.frame(param = 1:3), samples = fake_bundle())

  gen <- mockery::mock()
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_joint_ensemble_design", gen)
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_OAT_SA_design",
                function(...) list(design_matrix = data.frame(param = 1:7)))

  designs <- .prepare_input_designs(settings, input_design = supplied)

  # with an ensemble in the settings the supplied design is the ensemble one,
  # and the SA design is still generated, as before
  expect_identical(designs$ensemble, supplied$design_matrix)
  expect_equal(designs$sensitivity, data.frame(param = 1:7))
  mockery::expect_called(gen, 0)
})


test_that(".prepare_input_designs warns when it generates the SA design itself", {
  tmp <- withr::local_tempdir()
  settings <- make_sa_settings(tmp)

  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::load_pft_posteriors",
                function(...) fake_loaded())
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::get_parameter_samples",
                function(...) fake_bundle())
  mockery::stub(.prepare_input_designs,
                "PEcAn.uncertainty::generate_OAT_SA_design",
                function(...) list(design_matrix = data.frame(param = 1:7)))

  msgs <- capture.output(
    invisible(.prepare_input_designs(settings, input_design = NULL)),
    type = "message"
  )
  expect_match(paste(msgs, collapse = "\n"), "sensitivity analysis design", all = FALSE)
})


test_that(".prepare_input_designs does not warn when the SA design was supplied", {
  tmp <- withr::local_tempdir()
  settings <- make_sa_settings(tmp)

  supplied <- list(design_matrix = data.frame(param = 1:7), samples = fake_bundle())

  msgs <- capture.output(
    invisible(.prepare_input_designs(settings, input_design = supplied)),
    type = "message"
  )
  expect_false(any(grepl("deprecated", msgs)))
})