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

test_that("`do_conversions` silently converts character inputs to NULL", {
  withr::with_tempdir({
    settings <- list(
      host = list(name = "localhost"),
      outdir = getwd(),
      database = list(dbfiles = getwd()),
      run = list(inputs = "some_legacy_string")
    )
    # Should not error, and inputs should be treated as NULL (loop skipped)
    ret <- do_conversions(settings)
    # needsave stays FALSE, no METProcess.xml written, settings returned as-is
    expect_false(file.exists(file.path(getwd(), "pecan.METProcess.xml")))
  })
})

test_that("`do_conversions` skips input when path exists and force is NULL", {
  withr::with_tempdir({
    mocked_met <- mockery::mock(list(path = "should_not_be_called"))
    mockery::stub(do_conversions, "PEcAn.data.atmosphere::met.process", mocked_met)
    settings <- list(
      host = list(name = "localhost"),
      database = list(dbfiles = getwd()),
      outdir = getwd(),
      run = list(
        site = list(id = 0),
        inputs = list(met = list(path = "/existing/path"))  # path exists, force is NULL
      )
    )
    ret <- do_conversions(settings)
    # met.process should NOT have been called
    mockery::expect_called(mocked_met, 0)
    # path should be unchanged
    expect_equal(ret$run$inputs$met$path, "/existing/path")
    # needsave FALSE so no xml written
    expect_false(file.exists(file.path(getwd(), "pecan.METProcess.xml")))
  })
})

test_that("`do_conversions` does NOT skip input when path exists but force is set", {
  withr::with_tempdir({
    mocked_met <- mockery::mock(list(path = "new_path"))
    mockery::stub(do_conversions, "PEcAn.data.atmosphere::met.process", mocked_met)
    mockery::stub(do_conversions, "PEcAn.utils::status.check", mockery::mock(0))
    settings <- list(
      host = list(name = "localhost"),
      database = list(dbfiles = getwd()),
      outdir = getwd(),
      run = list(
        site = list(id = 0),
        inputs = list(met = list(path = "/existing/path", force = TRUE))
      )
    )
    ret <- do_conversions(settings)
    # met.process SHOULD have been called because force is set
    mockery::expect_called(mocked_met, 1)
    expect_equal(ret$run$inputs$met$path, "new_path")
  })
})

test_that("`do_conversions` returns settings unchanged when inputs is empty list", {
  withr::with_tempdir({
    settings <- list(
      host = list(name = "localhost"),
      outdir = getwd(),
      database = list(dbfiles = getwd()),
      run = list(inputs = list())
    )
    ret <- do_conversions(settings)
    # Empty inputs — needsave stays FALSE, no xml written
    expect_false(file.exists(file.path(getwd(), "pecan.METProcess.xml")))
    expect_equal(ret$run$inputs, list())
  })
})

test_that("`do_conversions` reads pecan.METProcess.xml when needsave is FALSE and file exists", {
  withr::with_tempdir({
    # Write a settings xml with a sentinel value
    xml_path <- file.path(getwd(), "pecan.METProcess.xml")
    writeLines(
      "<pecan>
        <outdir>sentinel_value</outdir>
      </pecan>",
      xml_path
    )
    settings <- list(
      host = list(name = "localhost"),
      outdir = getwd(),
      database = list(dbfiles = getwd()),
      run = list(inputs = list())  # empty inputs → needsave stays FALSE
    )
    ret <- do_conversions(settings)
    # Should have read from xml and returned the sentinel value
    expect_equal(ret$outdir, "sentinel_value")
  })
})
