# Regression tests pinning the run.write.configs() behaviour the
# config-writer refactor will move: the input_design/ensemble.size guard,
# the param-column rebuild of ensemble.samples from trait.samples, and the
# object-name contract of the ensemble/sensitivity .samples.<id>.Rdata saves.
# write.*.configs(), the *.filename() helpers and load.modelpkg() are stubbed.
# DB-skip is covered in test.run.write.configs.R, manifest handling in
# test_run.write.configs_multisite.R.

skip_if_not_installed("mockery")
skip_if_not_installed("withr")

# --------------------
# Helpers
# --------------------

# Minimal single-PFT settings; database NULL + write = FALSE keep BETY out,
# model "FAKE" keeps us off a real model package once load.modelpkg() is stubbed.
make_rwc_settings <- function(outdir,
                              with_ensemble = FALSE,
                              with_sa = FALSE) {
  settings <- list(
    outdir   = outdir,
    rundir   = outdir,
    database = NULL,
    host     = list(name = "localhost"),
    model    = list(type = "FAKE"),
    pfts     = list(list(name = "temperate.deciduous", posteriorid = NULL))
  )
  if (with_ensemble) {
    settings$ensemble <- list(size = 3)
  }
  if (with_sa) {
    settings$sensitivity.analysis <- list(quantiles = c(0.025, 0.5, 0.975))
  }
  settings
}

# Writes the samples.Rdata bundle run.write.configs() loads; only the objects
# each test needs are set, the rest stay empty lists.
write_samples_file <- function(outdir,
                               trait.samples,
                               ensemble.samples = list(),
                               sa.samples = list()) {
  runs.samples <- list()
  env.samples  <- list()
  save(trait.samples, ensemble.samples, sa.samples, runs.samples, env.samples,
       file = file.path(outdir, "samples.Rdata"))
}

# Throwaway write.config.FAKE so the exists() check in run.write.configs()
# passes; removed when the calling test finishes.
local_fake_model_writer <- function(env = parent.frame()) {
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv),
               envir = env, priority = "first")
}

# ------------------------------------
# input_design / ensemble.size guard
# ------------------------------------

test_that("run.write.configs errors when input_design rows != ensemble.size", {
  tmp <- withr::local_tempdir()
  settings <- make_rwc_settings(tmp, with_ensemble = TRUE)

  # 3 design rows against an ensemble.size of 5: the run and design counts
  # disagree, so the function must stop before writing anything.
  expect_error(
    PEcAn.workflow::run.write.configs(
      settings,
      ensemble.size = 5,
      input_design  = data.frame(param = c(1, 2, 3)),
      write = FALSE
    ),
    "input_design has 3 rows, but ensemble.size is 5"
  )
})

# -----------------------------------------------------------
# ensemble.samples sourcing (param column vs samples.Rdata)
# -----------------------------------------------------------

test_that("ensemble.samples are rebuilt from trait.samples via input_design$param", {
  tmp <- withr::local_tempdir()
  local_fake_model_writer()
  settings <- make_rwc_settings(tmp, with_ensemble = TRUE)

  trait.samples <- list(
    temperate.deciduous = list(
      Vcmax = c(10, 20, 30, 40, 50),
      SLA   = c(1,  2,  3,  4,  5)
    )
  )
  # ensemble.samples on disk is deliberately wrong, so this fails if the
  # param-column rebuild is skipped and the disk copy is used instead.
  write_samples_file(
    tmp, trait.samples,
    ensemble.samples = list(temperate.deciduous = data.frame(Vcmax = -1, SLA = -1))
  )

  input_design <- data.frame(param = c(2, 4, 5))

  wec <- mockery::mock(
    list(runs        = c("r1", "r2", "r3"),
         ensemble.id = "ENS9",
         manifest    = data.frame(run_id = c("r1", "r2", "r3"),
                                  stringsAsFactors = FALSE))
  )

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg",
                function(...) invisible(NULL))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::write.ensemble.configs", wec)
  mockery::stub(run_write_configs, "PEcAn.uncertainty::ensemble.filename",
                function(...) file.path(tmp, "ensemble.samples.ENS9.Rdata"))

  run_write_configs(settings, ensemble.size = 3,
                    input_design = input_design, write = FALSE)

  mockery::expect_called(wec, 1)
  passed <- mockery::mock_args(wec)[[1]]$ensemble.samples
  expect_equal(passed$temperate.deciduous$Vcmax, c(20, 40, 50))
  expect_equal(passed$temperate.deciduous$SLA,   c(2, 4, 5))
})

test_that("without a param column ensemble.samples come straight from samples.Rdata", {
  tmp <- withr::local_tempdir()
  local_fake_model_writer()
  settings <- make_rwc_settings(tmp, with_ensemble = TRUE)

  trait.samples    <- list(temperate.deciduous = list(Vcmax = c(10, 20, 30)))
  ensemble.samples <- list(temperate.deciduous = data.frame(Vcmax = c(7, 8, 9)))
  write_samples_file(tmp, trait.samples, ensemble.samples = ensemble.samples)

  # design carries an input column but no param column, so the pre-generated
  # ensemble.samples are used as-is.
  input_design <- data.frame(met = c(1, 1, 1))

  wec <- mockery::mock(
    list(runs        = c("r1", "r2", "r3"),
         ensemble.id = "ENS9",
         manifest    = data.frame(run_id = c("r1", "r2", "r3"),
                                  stringsAsFactors = FALSE))
  )

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg",
                function(...) invisible(NULL))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::write.ensemble.configs", wec)
  mockery::stub(run_write_configs, "PEcAn.uncertainty::ensemble.filename",
                function(...) file.path(tmp, "ensemble.samples.ENS9.Rdata"))

  run_write_configs(settings, ensemble.size = 3,
                    input_design = input_design, write = FALSE)

  mockery::expect_called(wec, 1)
  passed <- mockery::mock_args(wec)[[1]]$ensemble.samples
  expect_equal(passed$temperate.deciduous$Vcmax, c(7, 8, 9))
})

# ------------------------
# on-disk save contracts
# ------------------------

test_that("ensemble.samples.<id>.Rdata keeps its 5-object save contract", {
  tmp <- withr::local_tempdir()
  local_fake_model_writer()
  settings <- make_rwc_settings(tmp, with_ensemble = TRUE)

  trait.samples <- list(temperate.deciduous = list(Vcmax = c(10, 20, 30)))
  write_samples_file(tmp, trait.samples)

  input_design <- data.frame(param = c(1, 2, 3))
  ens_file <- file.path(tmp, "ensemble.samples.ENS9.Rdata")

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg",
                function(...) invisible(NULL))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::write.ensemble.configs",
                function(...) list(
                  runs        = c("r1", "r2", "r3"),
                  ensemble.id = "ENS9",
                  manifest    = data.frame(run_id = c("r1", "r2", "r3"),
                                           stringsAsFactors = FALSE)
                ))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::ensemble.filename",
                function(...) ens_file)

  run_write_configs(settings, ensemble.size = 3,
                    input_design = input_design, write = FALSE)

  expect_true(file.exists(ens_file))
  e <- new.env()
  load(ens_file, envir = e)
  expect_setequal(
    ls(e),
    c("ens.run.ids", "ens.ensemble.id", "ens.samples", "pft.names", "trait.names")
  )
  expect_equal(e$ens.run.ids, c("r1", "r2", "r3"))
  expect_equal(e$ens.ensemble.id, "ENS9")
  expect_equal(e$ens.samples$temperate.deciduous$Vcmax, c(10, 20, 30))
  expect_equal(e$pft.names, "temperate.deciduous")
  expect_equal(e$trait.names, list(temperate.deciduous = "Vcmax"))
})

test_that("sensitivity.samples.<id>.Rdata keeps its 5-object save contract", {
  tmp <- withr::local_tempdir()
  local_fake_model_writer()
  settings <- make_rwc_settings(tmp, with_sa = TRUE)

  trait.samples <- list(temperate.deciduous = list(Vcmax = c(10, 20, 30)))
  sa.samples <- list(
    temperate.deciduous = matrix(
      c(12, 20, 28), nrow = 3, ncol = 1,
      dimnames = list(c("2.5", "50", "97.5"), "Vcmax")
    )
  )
  write_samples_file(tmp, trait.samples, sa.samples = sa.samples)

  input_design <- data.frame(param = c(1, 2, 3))
  sa_file <- file.path(tmp, "sensitivity.samples.SA7.Rdata")

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg",
                function(...) invisible(NULL))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::write.sa.configs",
                function(...) list(
                  runs        = list(temperate.deciduous = matrix(c("r1", "r2", "r3"), nrow = 3)),
                  ensemble.id = "SA7",
                  manifest    = data.frame(run_id = c("r1", "r2", "r3"),
                                           stringsAsFactors = FALSE)
                ))
  mockery::stub(run_write_configs, "PEcAn.uncertainty::sensitivity.filename",
                function(...) sa_file)

  run_write_configs(settings, ensemble.size = 1,
                    input_design = input_design, write = FALSE)

  expect_true(file.exists(sa_file))
  e <- new.env()
  load(sa_file, envir = e)
  expect_setequal(
    ls(e),
    c("sa.run.ids", "sa.ensemble.id", "sa.samples", "pft.names", "trait.names")
  )
  expect_equal(e$sa.ensemble.id, "SA7")
  expect_identical(e$sa.samples, sa.samples)
  expect_equal(e$pft.names, "temperate.deciduous")
  expect_equal(e$trait.names, list(temperate.deciduous = "Vcmax"))
})