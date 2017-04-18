context("Checking CRUNCEP download")

logger.setLevel("WARN")

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


test_that("reference times are present", {
  expect_equal(cf_units, "days since 2000-01-01T00:00:00Z")
})

test_that("overwrite argument is respected", {
  # Skip message comes from logger.error,
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