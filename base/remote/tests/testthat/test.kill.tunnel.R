test_that("`kill.tunnel()` able to read the correct files and log the correct messages to kill tunnel for exe and data", {
  withr::with_dir(tempdir(), {
    mockery::stub(kill.tunnel, 'tools::pskill', TRUE)
    mockery::stub(kill.tunnel, 'dirname', getwd())

    # Kill tunnel to executable
    settings <- list(host = list(tunnel = getwd()))
    file_path <- file.path(getwd(), "pid")
    file.create(file_path)
    writeLines("1234", file_path)
    expect_output(kill.tunnel(settings), "Killing tunnel with PID 1234")

    # Kill tunnel to data
    settings <- list(host = list(data_tunnel = getwd()))
    file_path <- file.path(getwd(), "pid")
    file.create(file_path)
    writeLines("3456", file_path)
    expect_output(kill.tunnel(settings), "Killing tunnel with PID 3456")
  })
})