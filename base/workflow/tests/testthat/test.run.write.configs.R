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
