# Tests for write.sa.configs
#
# Key behaviors tested:
# - Run ID generation format: SA-{pft}-{trait}-{quantile}-{site_id}
# - Median run sharing across all traits at quantile 50
# - Manifest structure and content
# - runs.txt and directory creation

test_that("write.sa.configs generates correct run IDs and manifest", {
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv))

  rundir <- withr::local_tempdir(pattern = "sa-rundir-")
  modeloutdir <- withr::local_tempdir(pattern = "sa-modelout-")

  settings <- list(
    rundir = rundir,
    modeloutdir = modeloutdir,
    host = list(name = "localhost", rundir = rundir, outdir = modeloutdir),
    run = list(
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      site = list(id = "1", name = "Test Site", site.pft = list("pftA")),
      inputs = list(met = list(path = "met.nc")),
      outdir = modeloutdir
    ),
    model = list(id = 99, type = "FAKE"),
    pfts = list(list(name = "pftA", posteriorid = 10, constants = list())),
    sensitivity.analysis = list(ensemble.id = "E-TEST"),
    workflow = list(id = 42),
    database = NULL
  )

  # SA samples: 2 quantiles * 2 traits
  quantile.samples <- list(
    pftA = matrix(
      c(1.0, 1.5, 2.0, 2.5),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("50", "95"), c("trait1", "trait2"))
    )
  )

  result <- PEcAn.uncertainty::write.sa.configs(
    defaults = settings$pfts,
    quantile.samples = quantile.samples,
    settings = settings,
    model = "FAKE",
    write.to.db = FALSE
  )

  # verify return structure
  expect_equal(result$ensemble.id, "E-TEST")
  expect_true("pftA" %in% names(result$runs))

  # verify median run ID format: SA-median-{site_id}
  median_run_id <- "SA-median-1"
  
  # both traits at q50 share the same median run
  expect_equal(as.character(result$runs$pftA["50", "trait1"]), median_run_id)
  expect_equal(as.character(result$runs$pftA["50", "trait2"]), median_run_id)

  # non-median runs: SA-{pft}-{trait}-{quantile}-{site_id}
  # quantile "95" --> 95/100 = 0.95
  expect_equal(as.character(result$runs$pftA["95", "trait1"]), "SA-pftA-trait1-0.95-1")
  expect_equal(as.character(result$runs$pftA["95", "trait2"]), "SA-pftA-trait2-0.95-1")

  # verify runs.txt: 1 median + 2 non-median trait runs = 3 entries
  runs_written <- readLines(file.path(rundir, "runs.txt"))
  expect_length(runs_written, 3)

  # verify directories created
  expect_true(dir.exists(file.path(rundir, median_run_id)))
  expect_true(dir.exists(file.path(rundir, "SA-pftA-trait1-0.95-1")))
  expect_true(dir.exists(file.path(rundir, "SA-pftA-trait2-0.95-1")))

  # verify manifest structure
  manifest <- result$manifest
  expected_cols <- c("run_id", "site_id", "pft_name", "trait", "quantile", "type")
  expect_true(all(expected_cols %in% names(manifest)))
  expect_true(all(manifest$type == "Sensitivity"))
  # 1 median + 2 non-median = 3 manifest entries
  expect_equal(nrow(manifest), 3)
})


test_that("write.sa.configs applies input_design for met file selection", {
  met_used <- list()
  assign("write.config.FAKE", function(defaults, trait.values, settings, run.id) {
    met_used[[run.id]] <<- settings$run$inputs$met$path
    invisible(NULL)
  }, envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv))

  rundir <- withr::local_tempdir(pattern = "sa-rundir-")
  modeloutdir <- withr::local_tempdir(pattern = "sa-modelout-")

  met_paths <- c("met_A.nc", "met_B.nc", "met_C.nc")
  
  settings <- list(
    rundir = rundir,
    modeloutdir = modeloutdir,
    host = list(name = "localhost", rundir = rundir, outdir = modeloutdir),
    run = list(
      start.date = "2000-01-01",
      end.date = "2000-12-31",
      site = list(id = "1", name = "Test Site", site.pft = list("pftA")),
      inputs = list(met = list(path = met_paths)),
      outdir = modeloutdir
    ),
    model = list(id = 99, type = "FAKE"),
    pfts = list(list(name = "pftA", posteriorid = 10, constants = list())),
    sensitivity.analysis = list(ensemble.id = "E-TEST"),
    workflow = list(id = 42),
    database = NULL
  )

  # simple: 1 trait, 2 quantiles --> 1 median + 1 SA run = 2 runs
  quantile.samples <- list(
    pftA = matrix(c(1.0, 1.5), nrow = 2, ncol = 1,
                  dimnames = list(c("50", "95"), c("Vcmax")))
  )

  # OAT design: hold met constant at index 1
  # median uses row 1 (hardcoded), SA runs use rows starting at run_index=1
  input_design <- data.frame(
    param = c(1, 1),
    met = c(1, 1)
  )

  PEcAn.uncertainty::write.sa.configs(
    defaults = settings$pfts,
    quantile.samples = quantile.samples,
    settings = settings,
    model = "FAKE",
    write.to.db = FALSE,
    input_design = input_design
  )

  # all runs should use met_A.nc (index 1)
  expect_true(length(met_used) > 0)
  for (run_id in names(met_used)) {
    expect_equal(met_used[[run_id]], "met_A.nc")
  }
})