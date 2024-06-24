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
