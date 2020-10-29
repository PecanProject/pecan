context("Checking GFDL download")

tmpdir <- tempfile(pattern = "GFDLtest")
dir.create(tmpdir, showWarnings = FALSE)
teardown(unlink(tmpdir, recursive = TRUE))

test_that("GFDL server is reachable", {
  skip_on_ci()

  test_url <- paste0("http://nomads.gfdl.noaa.gov:9192/opendap/",
                     "CMIP5/output1/NOAA-GFDL/GFDL-CM3/rcp45/3hr/",
                     "atmos/3hr/r1i1p1/v20110601/tas/",
                     "tas_3hr_GFDL-CM3_rcp45_r1i1p1_2006010100-2010123123.nc")
  test_nc <- tryCatch(
    ncdf4::nc_open(test_url, suppress_dimvals = TRUE),
    error = function(e) {
      # NB this skips whole file-- that's why other tests are nested inside this one.
      skip("Skipping test because unable to reach NOAA GFDL download server.")
    }
  )
  teardown(ncdf4::nc_close(test_nc))
  expect_s3_class(test_nc, "ncdf4")

  test_that("download works and returns a valid CF file", {
    # Download is too slow for Travis -- please run locally before committing!
    skip_on_ci()


    PEcAn.logger::logger.setLevel("WARN")

    result <- download.GFDL(
      outfolder = tmpdir,
      start_date = "2007-01-01",
      end_date = "2007-12-31",
      site_id = 753,
      lat.in = 40,
      lon.in = -88
    )
    cf <- ncdf4::nc_open(result$file)
    teardown(ncdf4::nc_close(cf))

    # Expect that reference times are present and set to start date
    cf_units <- cf$dim$time$units
    expect_equal(cf_units, "seconds since 2007-01-01 00:00:00")

    expect_equal(cf$dim$latitude$len, 1)
    expect_equal(cf$dim$longitude$len, 1)
    expect_equal(cf$dim$time$len, 365*(24/3)) # one year at 3-hr interval, leap days ignored
    expect_equal(cf$nvar, 8)

    # Expect that overwrite argument is respected
    expect_log(
      download.GFDL(
        outfolder = tmpdir,
        start_date = "2007-01-01",
        end_date = "2007-12-31",
        site_id = 753,
        lat.in = 40,
        lon.in = -88,
        overwrite = FALSE),
      "already exists. Skipping")
  })
})
