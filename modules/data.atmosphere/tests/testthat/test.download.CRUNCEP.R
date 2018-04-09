context("Checking CRUNCEP download")


test_that("download works and returns a valid CF file", {
  # download is slow and was causing lots of Travis timeouts
  skip_on_travis()

  PEcAn.logger::logger.setLevel("WARN")

  tmpdir <- tempdir()
  on.exit(unlink(tmpdir, recursive = TRUE))

  result <- download.CRUNCEP(outfolder = tmpdir,
                             start_date = "2000-01-01",
                             end_date = "2000-12-31",
                             site_id = 753,
                             lat.in = 40,
                             lon.in = -88)
  cf <- ncdf4::nc_open(result$file)
  cf_units <- cf$dim$time$units
  ncdf4::nc_close(cf)

  # Expect that reference times are present and set to start date
  expect_equal(cf_units, "days since 2000-01-01T00:00:00Z")

  # Expect that overwrite argument is respected
  # The skip message comes fromPEcAn.logger::logger.error,
  # which writes to stderr but does not use message().
  # If it did, this test would reduce to expect_message(download.CRUNCEP(...), "foo")
  msg <- capture.output(download.CRUNCEP(outfolder = tmpdir,
                                         start_date = "2000-01-01",
                                         end_date = "2000-12-31",
                                         site_id = 753,
                                         lat.in = 40,
                                         lon.in = -88,
                                         overwrite = FALSE),
                        type = "message")
  expect_match(paste(msg, collapse="\n"), "already exists. Skipping")
})
