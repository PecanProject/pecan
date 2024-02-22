test_that("`start_model_runs` throws a warning if runs.txt not provided", {
  withr::with_tempdir({
    PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
    on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE))
    settings <- list(rundir = getwd())
    expect_output(start_model_runs(settings), "runs.txt not found")
  })
})

test_that("`start_model_runs` throws a warning if runs.txt is empty", {
  withr::with_tempdir({
    PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
    on.exit(PEcAn.logger::logger.setUseConsole(TRUE, TRUE))
    settings <- list(rundir = getwd())
    file_path <- file.path(getwd(), "runs.txt")
    file.create(file_path)
    expect_output(start_model_runs(settings), "runs.txt found, but is empty")
  })
})

