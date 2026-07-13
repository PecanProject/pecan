context("run.write.configs")

test_that("run.write.configs correctly skips database connection when bety is missing", {
  # Mock settings with a database section but missing 'bety' details
  # This mimics the issue where dbfiles triggers the database presence check, but no connection details exist.
  settings <- list(
    database = list(
      dbfiles = tempdir()
    ),
    outdir = tempdir(),
    run = list(
      host = list(
        name = "localhost"
      )
    ),
    model = list(
      type = "ALMA"
    )
  )
  
  # create a dummy samples.Rdata so it does not crash saying it requires it
  samples.file <- file.path(settings$outdir, "samples.Rdata")
  # Mock the internal elements expected by run.write.configs
  trait.samples <- list()
  sa.samples <- list()
  runs.samples <- list()
  env.samples <- list()
  ensemble.samples <- list()
  
  save(
    trait.samples, 
    ensemble.samples,
    sa.samples, 
    runs.samples, 
    env.samples, 
    file = samples.file
  )
  
  # When write is FALSE and bety is missing, the code should skip opening a DB connection
  # and not throw an error from attempting to pass NULL to db.open()
  
  expect_no_error({
    runwrite_log <- capture.output(
      PEcAn.workflow::run.write.configs(
        settings,
        write = FALSE,
        input_design = data.frame(param = 1),
        ensemble.size = 1,
        overwrite = FALSE
      ),
      type = "message"
    )
  })
  expect_match(runwrite_log, "Not writing this run to database", all = FALSE)
})

test_that("run.write.configs uses the in-memory samples= bundle and skips the disk read", {
  # outdir has NO samples.Rdata: if the disk path were taken, run.write.configs
  # would hit its "not found" error. Passing samples= must bypass that.
  empty_outdir <- tempfile("no_samples_")
  dir.create(empty_outdir)

  settings <- list(
    database = list(dbfiles = tempdir()),
    outdir = empty_outdir,
    rundir = empty_outdir,
    run = list(host = list(name = "localhost")),
    model = list(type = "ALMA")
  )

  samples <- list(
    trait.samples    = list(),
    sa.samples       = list(),
    ensemble.samples = list(),
    runs.samples     = list(),
    env.samples      = list()
  )

  expect_false(file.exists(file.path(empty_outdir, "samples.Rdata")))

  expect_no_error({
    runwrite_log <- capture.output(
      PEcAn.workflow::run.write.configs(
        settings,
        write = FALSE,
        input_design = data.frame(param = 1),
        ensemble.size = 1,
        overwrite = TRUE,
        samples = samples
      ),
      type = "message"
    )
  })
  # confirms it got past sample resolution to the config-writing stage
  expect_match(runwrite_log, "Finished writing model run config files", all = FALSE)
})
