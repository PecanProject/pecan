test_that("write_job_sh creates job.sh file", {
  # Create a temporary directory for testing
  tmpdir <- tempfile()
  dir.create(tmpdir)
  run.id <- "test_run_001"
  dir.create(file.path(tmpdir, run.id))

  # Define simple job script content
  jobsh <- c(
    "#!/bin/bash",
    "echo 'running model'"
  )

  # Call the helper function
  result <- write_job_sh(tmpdir, run.id, jobsh)

  # Test 1 - file exists
  expect_true(file.exists(file.path(tmpdir, run.id, "job.sh")))

  # Test 2 - file content is correct
  written <- readLines(file.path(tmpdir, run.id, "job.sh"))
  expect_equal(written, jobsh)

  # Test 3 - function returns the path invisibly
  expect_equal(result, file.path(tmpdir, run.id, "job.sh"))

  # Cleanup
  unlink(tmpdir, recursive = TRUE)
})

test_that("write_job_sh chmod parameter works", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  run.id <- "test_run_002"
  dir.create(file.path(tmpdir, run.id))

  jobsh <- c("#!/bin/bash", "./model")

  # Test with chmod = FALSE
  write_job_sh(tmpdir, run.id, jobsh, chmod = FALSE)
  expect_true(file.exists(file.path(tmpdir, run.id, "job.sh")))

  # Cleanup
  unlink(tmpdir, recursive = TRUE)
})