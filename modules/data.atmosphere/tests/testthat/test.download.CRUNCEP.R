context("Checking CRUNCEP download")

tmpdir <- tempfile(pattern = "CRUNCEPtest")
dir.create(tmpdir)
teardown(unlink(tmpdir, recursive = TRUE))

test_that("download works and returns a valid CF file", {
  # download is slow and was causing lots of Travis timeouts
  skip_on_ci()

  PEcAn.logger::logger.setLevel("WARN")

  result <- download.CRUNCEP(outfolder = tmpdir,
                             start_date = "2000-01-01",
                             end_date = "2000-12-31",
                             lat.in = 40,
                             lon.in = -88)
  cf <- ncdf4::nc_open(result$file)
  cf_units <- cf$dim$time$units
  ncdf4::nc_close(cf)

  # Expect that reference times are present and set to start date
  expect_equal(cf_units, "days since 2000-01-01T00:00:00Z")

  expect_log(
    download.CRUNCEP(
      outfolder = tmpdir,
      start_date = "2000-01-01",
      end_date = "2000-12-31",
      lat.in = 40,
      lon.in = -88,
      overwrite = FALSE),
    "already exists. Skipping")
})
