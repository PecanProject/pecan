test_that("run.sensitivity.analysis returns NULL when sensitivity.analysis is missing", {
  expect_null(run.sensitivity.analysis(list()))
})

test_that("run.sensitivity.analysis loads samples once before variables loop and preserves loop variable", {
  tmpdir <- withr::local_tempdir()

  # Create mock samples.Rdata
  trait.samples <- list(pft1 = list(sla = c(10, 20)))
  sa.samples <- list(pft1 = matrix(c(10, 20), nrow = 2, dimnames = list(c("25", "75"), "sla")))
  runs.samples <- list(sa = c("run1", "run2"))
  save(trait.samples, sa.samples, runs.samples, file = file.path(tmpdir, "samples.Rdata"))

  settings <- list(
    outdir = tmpdir,
    sensitivity.analysis = list(
      variable = c("NPP", "GPP"),
      start.year = 2000,
      end.year = 2001,
      ensemble.id = "ENS-TEST"
    ),
    pfts = list(list(name = "pft1", outdir = tmpdir))
  )

  # Mock sensitivity output file for both variables
  for (var in c("NPP", "GPP")) {
    sens_out_file <- sensitivity.filename(
      settings, "sensitivity.output", "Rdata",
      all.var.yr = FALSE,
      ensemble.id = "ENS-TEST",
      variable = var,
      start.year = 2000,
      end.year = 2001
    )
    sensitivity.output <- list(pft1 = data.frame(sla = c(1.0, 2.0)))
    dir.create(dirname(sens_out_file), showWarnings = FALSE, recursive = TRUE)
    save(sensitivity.output, file = sens_out_file)
  }

  # Mock trait.lookup, convert.expr, sensitivity.analysis
  mockery::stub(run.sensitivity.analysis, "PEcAn.utils::trait.lookup", function(...) data.frame(units = "m2/kg"))
  mockery::stub(run.sensitivity.analysis, "PEcAn.utils::convert.expr", function(var) list(variable.drv = var))
  mockery::stub(run.sensitivity.analysis, "sensitivity.analysis", function(...) list(
    variance.decomposition.output = list(),
    sensitivity.output = list()
  ))

  # Track load calls to verify samples.Rdata is only loaded once
  load_count <- 0
  mock_load <- function(file, envir) {
    if (grepl("samples\\.Rdata$", file)) {
      load_count <<- load_count + 1
    }
    base::load(file, envir = envir)
  }
  mockery::stub(run.sensitivity.analysis, "load", mock_load)

  run.sensitivity.analysis(settings, plot = FALSE)

  # samples.Rdata should be loaded exactly once despite multiple variables
  expect_equal(load_count, 1)

  # Both variable results should be generated
  for (var in c("NPP", "GPP")) {
    res_file <- sensitivity.filename(
      settings, "sensitivity.results", "Rdata",
      all.var.yr = FALSE,
      pft = NULL,
      ensemble.id = "ENS-TEST",
      variable = var,
      start.year = 2000,
      end.year = 2001
    )
    expect_true(file.exists(res_file))
  }
})
