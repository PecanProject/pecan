# Tests for generate_OAT_SA_design
# Shared fixtures (make_test_settings, mock_sa_samples) in helper-input_design.R

# Canned return shapes for the in-memory sampling path, so the migration tests
# run without real priors, MCMC, or disk I/O (mirrors the ensemble generator
# tests). The bundle carries a populated sa.samples so a design can be built.
oat_loaded_posteriors <- function() {
  list(
    pft_names         = list("pft1"),
    prior_distns_list = list(NULL),
    trait_mcmc_list   = list(NULL),
    independent       = TRUE
  )
}

oat_parameter_samples <- function() {
  list(
    trait.samples    = list(pft1 = list(trait1 = 1:3)),
    sa.samples       = mock_sa_samples,
    ensemble.samples = list(),
    runs.samples     = list(),
    env.samples      = list()
  )
}


test_that("generate_OAT_SA_design returns correct structure and run count", {
  settings <- make_test_settings()

  result <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))

  # 1 median + 3 traits * 2 non-median quantiles = 7
  expect_equal(nrow(result$X), 7)
  expect_true("param" %in% names(result$X))
  expect_true(is.data.frame(result$X))
})


test_that("generate_OAT_SA_design keeps param sequential and non-param constant at 1", {
  settings <- make_test_settings()

  result <- generate_OAT_SA_design(settings, samples = list(sa.samples = mock_sa_samples))

  expect_equal(result$X$param, seq_len(nrow(result$X)))

  non_param_cols <- setdiff(names(result$X), "param")
  for (col in non_param_cols) {
    expect_true(all(result$X[[col]] == 1))
  }
})


test_that("supplied samples are returned alongside the design", {
  settings <- make_test_settings()
  bundle <- oat_parameter_samples()

  result <- generate_OAT_SA_design(settings, samples = bundle)

  expect_true("samples" %in% names(result))
  expect_identical(result$samples, bundle)
})


test_that("parameters are sampled in memory when no samples are supplied", {
  settings <- make_test_settings()
  settings$sensitivity.analysis <- list(quantiles = c(0.25, 0.5, 0.75))

  loader  <- mockery::mock(oat_loaded_posteriors())
  sampler <- mockery::mock(oat_parameter_samples())
  mockery::stub(generate_OAT_SA_design, "load_pft_posteriors", loader)
  mockery::stub(generate_OAT_SA_design, "get_parameter_samples", sampler)

  result <- generate_OAT_SA_design(settings, samples = NULL)

  mockery::expect_called(loader, 1)
  mockery::expect_called(sampler, 1)

  # SA sampling draws quantiles, not an ensemble
  args <- mockery::mock_args(sampler)[[1]]
  expect_false(args$do_ensemble)
  expect_identical(args$sa_quantiles, c(0.25, 0.5, 0.75))

  # design is built from the freshly sampled sa.samples, and returned with them
  expect_equal(nrow(result$X), 7)
  expect_false(is.null(result$samples$sa.samples))
})


test_that("passed-in samples skip the loader and sampler", {
  settings <- make_test_settings()

  loader  <- mockery::mock()
  sampler <- mockery::mock()
  mockery::stub(generate_OAT_SA_design, "load_pft_posteriors", loader)
  mockery::stub(generate_OAT_SA_design, "get_parameter_samples", sampler)

  established <- oat_parameter_samples()
  result <- generate_OAT_SA_design(settings, samples = established)

  mockery::expect_called(loader, 0)
  mockery::expect_called(sampler, 0)
  expect_identical(result$samples, established)
})


test_that("a bundle with empty sa.samples is rejected", {
  settings <- make_test_settings()

  expect_error(
    generate_OAT_SA_design(settings, samples = list(sa.samples = list())),
    "sa.samples"
  )
})


#------------------ tests: OAT design with write.sa.configs -------------------
# verifies the design still produces output compatible with SA postprocessing
# after the migration (write.sa.configs is untouched by this PR)

test_that("OAT design integrates with write.sa.configs for SA postprocessing", {
  # mock model config writer
  assign("write.config.FAKE", function(defaults, trait.values, settings, run.id) {
    invisible(NULL)
  }, envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv))

  workflow_root <- withr::local_tempdir()
  rundir <- file.path(workflow_root, "run")
  modeloutdir <- file.path(workflow_root, "out")
  dir.create(rundir, recursive = TRUE)
  dir.create(modeloutdir, recursive = TRUE)

  met_paths <- c("met_2010.nc", "met_2011.nc", "met_2012.nc")

  settings <- list(
    outdir = workflow_root,
    rundir = rundir,
    modeloutdir = modeloutdir,
    host = list(name = "localhost", rundir = rundir, outdir = modeloutdir),
    run = list(
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      site = list(id = "1", name = "Test Site"),
      inputs = list(met = list(path = met_paths)),
      outdir = modeloutdir
    ),
    model = list(id = 99, type = "FAKE"),
    pfts = list(list(name = "pft1", posteriorid = NULL, constants = list())),
    sensitivity.analysis = list(ensemble.id = "SA-TEST"),
    workflow = list(id = 1),
    database = NULL,
    ensemble = list(
      samplingspace = list(
        parameters = list(method = "uniform"),
        met = list(method = "sampling")
      )
    )
  )

  sa_samples <- list(
    pft1 = matrix(
      c(1, 2, 3, 4, 5, 6),
      nrow = 3, ncol = 2,
      dimnames = list(c("25", "50", "75"), c("Vcmax", "SLA"))
    )
  )

  design_result <- generate_OAT_SA_design(settings, samples = list(sa.samples = sa_samples))
  input_design <- design_result$X

  result <- PEcAn.uncertainty::write.sa.configs(
    defaults = settings$pfts,
    quantile.samples = sa_samples,
    settings = settings,
    model = "FAKE",
    write.to.db = FALSE,
    input_design = input_design
  )

  # verify write.sa.configs output structure (required for SA postprocessing)
  expect_true("runs" %in% names(result))
  expect_true("ensemble.id" %in% names(result))
  expect_true("pft1" %in% names(result$runs))

  # verify runs matrix structure matches sa_samples (required by run.sensitivity.analysis)
  runs_matrix <- result$runs$pft1
  expect_equal(rownames(runs_matrix), rownames(sa_samples$pft1))
  expect_equal(colnames(runs_matrix), colnames(sa_samples$pft1))

  # verify runs.txt created with correct count
  runs_file <- file.path(rundir, "runs.txt")
  expect_true(file.exists(runs_file))
  run_ids <- readLines(runs_file)
  expect_equal(length(run_ids), nrow(input_design))

  # verify run directories created (required for model output reading)
  for (run_id in run_ids) {
    expect_true(dir.exists(file.path(rundir, run_id)))
  }
})