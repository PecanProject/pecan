# Tests for run.write.configs manifest handling
#
# These tests verify the manifest file behavior that enables multisite workflows.
# When runModule.run.write.configs processes MultiSettings, it calls run.write.configs
# with overwrite=TRUE for the first site and overwrite=FALSE for subsequent sites.


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
  input_design <- list(sensitivity = data.frame(param = c(1, 1), met = c(1, 1)))

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


test_that("run.write.configs appends to manifest when overwrite = FALSE", {
  # this is the key multisite behavior: papply calls with overwrite=FALSE
  # after the first site, so manifests from all sites accumulate
  
  assign("write.config.FAKE", function(...) invisible(NULL), envir = .GlobalEnv)
  withr::defer(rm("write.config.FAKE", envir = .GlobalEnv), priority = "first")

  env <- make_test_env()
  input_design <- list(sensitivity = data.frame(param = c(1, 1), met = c(1, 1)))

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