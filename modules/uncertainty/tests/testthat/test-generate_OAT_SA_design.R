# Tests for generate_OAT_SA_design.R
# Shared fixtures in helper-input_design.R

test_that("generate_OAT_SA_design returns correct structure and run count", {
  settings <- make_test_settings()
  
  result <- generate_OAT_SA_design(settings, sa_samples = mock_sa_samples)
  
  # 1 median + 3 traits * 2 non-median quantiles = 7
  expect_equal(nrow(result$X), 7)
  expect_true("param" %in% names(result$X))
  expect_true(is.data.frame(result$X))
  })

  test_that("generate_OAT_SA_design keeps param sequential and non-param constant at 1", {
  settings <- make_test_settings()
  result <- generate_OAT_SA_design(settings, sa_samples = mock_sa_samples)
  
  expect_equal(result$X$param, seq_len(nrow(result$X)))

  non_param_cols <- setdiff(names(result$X), "param")
  for (col in non_param_cols) {
    expect_true(all(result$X[[col]] == 1))
  }
})

#------------------ tests: OAT design with write.sa.configs -------------------
# verifies design produces output compatible with SA postprocessing

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
  
  design_result <- generate_OAT_SA_design(settings, sa_samples = sa_samples)
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