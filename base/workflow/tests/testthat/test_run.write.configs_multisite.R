# Tests for run.write.configs manifest handling
#
# These tests verify the manifest file behavior that enables multisite workflows.
# When runModule.run.write.configs processes MultiSettings with overwrite=TRUE, it
# clears the manifest once, then runs each site with overwrite=FALSE, so every
# site's run.write.configs call appends to it.


make_test_env <- function() {
  workflow_root <- withr::local_tempdir(.local_envir = parent.frame())
  rundir <- file.path(workflow_root, "run")
  modeloutdir <- file.path(workflow_root, "out")
  dir.create(rundir, recursive = TRUE, showWarnings = FALSE)
  dir.create(modeloutdir, recursive = TRUE, showWarnings = FALSE)

  # create minimal samples.Rdata
  trait.samples <- list(pftA = list(Vcmax = c(40, 45)))
  sa.samples <- list(
    pftA = matrix(c(42, 48), nrow = 2, ncol = 1,
                  dimnames = list(c("50", "95"), "Vcmax"))
  )
  runs.samples <- list()
  pft.names <- names(trait.samples)
  trait.names <- lapply(trait.samples, names)
  save(trait.samples, sa.samples, runs.samples, pft.names, trait.names,
       file = file.path(workflow_root, "samples.Rdata"))

  list(
    workflow_root = workflow_root,
    rundir = rundir,
    modeloutdir = modeloutdir,
    manifest_file = file.path(workflow_root, "runs_manifest.csv")
  )
}

make_settings <- function(env, site_id) {
  list(
    outdir = env$workflow_root,
    rundir = env$rundir,
    modeloutdir = env$modeloutdir,
    database = NULL,
    host = list(name = "localhost", rundir = env$rundir, outdir = env$modeloutdir),
    model = list(type = "FAKE", id = 123),
    run = list(
      start.date = "2001-01-01",
      end.date = "2001-12-31",
      outdir = env$modeloutdir,
      site = list(id = site_id, name = paste("Site", site_id), site.pft = list("pftA")),
      inputs = list(met = list(path = list("met.nc")))
    ),
    pfts = list(list(name = "pftA", constants = list(), posteriorid = NULL)),
    sensitivity.analysis = list(quantiles = c(0.5, 0.95)),
    workflow = list(id = 42)
  )
}


test_that("run.write.configs writes manifest with expected structure", {
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv), priority = "first")

  env <- make_test_env()
  settings <- make_settings(env, "1001")
  # the design says what each run is: the median run, then Vcmax at its 95th
  input_design <- data.frame(
    param       = c(1, 2),
    met         = c(1, 1),
    sa_pft      = c(NA, "pftA"),
    sa_trait    = c(NA, "Vcmax"),
    sa_quantile = c("50", "95"),
    stringsAsFactors = FALSE
  )

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg", function(...) invisible(NULL))

  run_write_configs(
    settings = settings,
    ensemble.size = 1,
    input_design = input_design,
    write = FALSE,
    overwrite = TRUE
  )

  expect_true(file.exists(env$manifest_file))

  manifest <- utils::read.csv(env$manifest_file, stringsAsFactors = FALSE)

  # verify manifest structure matches what read.sa.output expects
  expected_cols <- c("run_id", "site_id", "pft_name", "trait", "quantile", "type")
  expect_true(all(expected_cols %in% names(manifest)))
  expect_true(nrow(manifest) > 0)
  expect_true(all(manifest$type == "Sensitivity"))
  expect_equal(unique(as.character(manifest$site_id)), "1001")
})


test_that("rerunning a multisite write with overwrite = TRUE replaces the manifest", {
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv), priority = "first")

  env <- make_test_env()
  settings <- PEcAn.settings::MultiSettings(
    PEcAn.settings::as.Settings(make_settings(env, "1001")),
    PEcAn.settings::as.Settings(make_settings(env, "1002"))
  )
  input_design <- list(sensitivity = data.frame(
    param       = c(1, 2),
    met         = c(1, 1),
    sa_pft      = c(NA, "pftA"),
    sa_trait    = c(NA, "Vcmax"),
    sa_quantile = c("50", "95"),
    stringsAsFactors = FALSE
  ))

  # existing run directories and the appended manifest header both warn here
  suppressWarnings(runModule.run.write.configs(settings, overwrite = TRUE, input_design = input_design))
  first_count <- nrow(utils::read.csv(env$manifest_file))
  suppressWarnings(runModule.run.write.configs(settings, overwrite = TRUE, input_design = input_design))

  expect_equal(nrow(utils::read.csv(env$manifest_file)), first_count)
})


test_that("run.write.configs appends to manifest when overwrite = FALSE", {
  # this is the key multisite behavior: papply calls with overwrite=FALSE
  # after the first site, so manifests from all sites accumulate
  
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv), priority = "first")

  env <- make_test_env()
  # the design says what each run is: the median run, then Vcmax at its 95th
  input_design <- data.frame(
    param       = c(1, 2),
    met         = c(1, 1),
    sa_pft      = c(NA, "pftA"),
    sa_trait    = c(NA, "Vcmax"),
    sa_quantile = c("50", "95"),
    stringsAsFactors = FALSE
  )

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg", function(...) invisible(NULL))

  # first site: overwrite=TRUE (creates fresh manifest)
  run_write_configs(
    settings = make_settings(env, "1001"),
    ensemble.size = 1,
    input_design = input_design,
    write = FALSE,
    overwrite = TRUE
  )

  first_manifest <- utils::read.csv(env$manifest_file, stringsAsFactors = FALSE)
  first_count <- nrow(first_manifest)
  expect_gt(first_count, 0)

 # second site: overwrite=FALSE (appends to manifest)
  run_write_configs(
    settings = make_settings(env, "1002"),
    ensemble.size = 1,
    input_design = input_design,
    write = FALSE,
    overwrite = FALSE
  )

  merged_manifest <- utils::read.csv(env$manifest_file, stringsAsFactors = FALSE)

  # both sites should be present
  expect_setequal(as.character(unique(merged_manifest$site_id)), c("1001", "1002"))
  expect_gt(nrow(merged_manifest), first_count)
  
  # no duplicate column headers from append
  expect_false(any(merged_manifest$run_id == "run_id"))
})


test_that("run.write.configs preserves later PFTs and an explicit design subset", {
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv), priority = "first")

  env <- make_test_env()
  # pftA is initial vegetation; pftB may be planted in a later segment
  trait.samples <- list(pftA = list(Vcmax = c(40, 45)), pftB = list(Vcmax = c(60, 65)))
  sa.samples <- lapply(c(pftA = 42, pftB = 62), function(v) {
    matrix(c(v, v + 6), nrow = 2, ncol = 1, dimnames = list(c("50", "95"), "Vcmax"))
  })
  runs.samples <- list()
  pft.names <- names(trait.samples)
  trait.names <- lapply(trait.samples, names)
  save(trait.samples, sa.samples, runs.samples, pft.names, trait.names,
       file = file.path(env$workflow_root, "samples.Rdata"))
  settings <- make_settings(env, "1001")
  settings$pfts <- list(list(name = "pftA"), list(name = "pftB"))

  input_design <- data.frame(
    param       = c(1, 2, 3),
    met         = c(1, 1, 1),
    sa_pft      = c(NA, "pftA", "pftB"),
    sa_trait    = c(NA, "Vcmax", "Vcmax"),
    sa_quantile = c("50", "95", "95"),
    stringsAsFactors = FALSE
  )

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(run_write_configs, "PEcAn.utils::load.modelpkg", function(...) invisible(NULL))

  run_write_configs(
    settings = settings,
    ensemble.size = 1,
    input_design = input_design,
    write = FALSE,
    overwrite = TRUE
  )

  expect_length(readLines(file.path(env$rundir, "runs.txt")), 3)
  manifest <- utils::read.csv(env$manifest_file, stringsAsFactors = FALSE)
  expect_setequal(manifest$pft_name, c("pftA", "pftB"))
  expect_equal(manifest$ens_num, c(2, 3, 1, 1))

  settings$run$site$id <- "1002"
  run_write_configs(
    settings = settings,
    ensemble.size = 1,
    input_design = input_design[1:2, ],
    write = FALSE,
    overwrite = TRUE
  )
  manifest <- utils::read.csv(env$manifest_file, stringsAsFactors = FALSE)
  expect_setequal(manifest$pft_name, "pftA")
})


test_that("run.write.configs handles a site with no varying traits", {
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv), priority = "first")

  env <- make_test_env()
  settings <- make_settings(env, "1001")
  settings$pfts <- list(
    list(name = "pftA", constants = list(Vcmax = 42)),
    list(name = "pftB", constants = list())
  )

  samples <- list(
    trait.samples = list(
      pftA = list(),
      pftB = list(Vcmax = c(60, 65))
    ),
    sa.samples = list(
      pftA = matrix(
        numeric(),
        nrow = 2,
        ncol = 0,
        dimnames = list(c("50", "95"), character())
      ),
      pftB = matrix(
        c(62, 68),
        nrow = 2,
        ncol = 1,
        dimnames = list(c("50", "95"), "Vcmax")
      )
    ),
    ensemble.samples = list(),
    runs.samples = list(),
    env.samples = list()
  )
  design <- PEcAn.uncertainty::generate_OAT_SA_design(
    settings,
    samples = samples
  )$design_matrix
  design <- design[is.na(design$sa_pft), , drop = FALSE]

  run_write_configs <- PEcAn.workflow::run.write.configs
  mockery::stub(
    run_write_configs,
    "PEcAn.utils::load.modelpkg",
    function(...) invisible(NULL)
  )

  run_write_configs(
    settings = settings,
    ensemble.size = 1,
    input_design = design,
    write = FALSE,
    overwrite = TRUE,
    samples = samples
  )

  expect_length(readLines(file.path(env$rundir, "runs.txt")), 1)

  manifest <- utils::read.csv(env$manifest_file, stringsAsFactors = FALSE)
  expect_equal(nrow(manifest), 1)
  expect_true(is.na(manifest$pft_name))
  expect_equal(manifest$quantile, 50)
  expect_equal(manifest$type, "Sensitivity")
  expect_named(
    manifest,
    c("run_id", "site_id", "ens_num", "pft_name", "trait", "quantile", "type")
  )

  samples_file <- list.files(
    env$workflow_root,
    pattern = "^sensitivity\\.samples\\..*\\.Rdata$",
    full.names = TRUE
  )
  expect_length(samples_file, 1)

  saved <- new.env(parent = emptyenv())
  load(samples_file, envir = saved)
  expect_identical(saved$sa.run.ids, list())
})
