test_that("`do_conversions` able to return settings from pecan.METProcess.xml if it already exists", {
  withr::with_tempdir({
    settings <- list(host = list(name = 'test', folder = 'test'), outdir = getwd())
    file_path <- file.path(getwd(), "pecan.METProcess.xml")
    file.create(file_path)
    writeLines(
      "<pecan>
        <outdir>test</outdir>
      </pecan>",
      file_path
    )
    ret <- do_conversions(settings)
    expect_equal(ret$outdir, "test")
  })
})

test_that("`do_conversions` able to call met.process if the input tag has met, update the met path and save settings to pecan.METProcess.xml", {
  withr::with_tempdir({
    mocked_res <- mockery::mock(list(path = 'test'))
    mockery::stub(do_conversions, 'PEcAn.data.atmosphere::met.process', mocked_res)
    settings <- list(
      host = list(name = 'test', folder = 'test'),
      outdir = getwd(),
      run = list(site = list(id = 0), inputs = list(met = list(id = 1)))
    )
    res <- do_conversions(settings)
    mockery::expect_called(mocked_res, 1)
    expect_equal(res$run$inputs$met$path, 'test')
    expect_true(file.exists(file.path(getwd(), "pecan.METProcess.xml")))
  })
})

test_that("`do_conversions` works for settings with no database section", {
  withr::with_tempdir({
    settings <- list(
      host = list(name = "localhost"),
      outdir = getwd(),
      run = list(
        site = list(id = 0),
        inputs = list(met = list(path = "existing.clim"))
      )
    )
    expect_null(settings$database)
    res <- expect_no_error(do_conversions(settings))
    # input already has a path, so it is left alone
    expect_equal(res$run$inputs$met$path, "existing.clim")
  })
})

test_that("`do_conversions` uses the host folder for dbfiles when running on a remote host", {
  withr::with_tempdir({
    mockery::stub(do_conversions, "PEcAn.remote::is.localhost", FALSE)
    settings <- list(
      host = list(name = "remote.example.org", folder = "/remote/folder"),
      outdir = getwd(),
      database = list(dbfiles = "/local/dbfiles"),
      run = list(
        site = list(id = 0),
        inputs = list(met = list(path = "existing.clim"))
      )
    )
    expect_no_error(do_conversions(settings))
  })
})
