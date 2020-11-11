context("Download MERRA")

outdir <- tempdir()
setup(dir.create(outdir, showWarnings = FALSE, recursive = TRUE))
teardown(unlink(outdir, recursive = TRUE))

test_that("MERRA download works", {
  start_date <- "2009-06-01"
  end_date <- "2009-06-04"
  dat <- download.MERRA(outdir, start_date, end_date,
                        lat.in = 45.3, lon.in = -85.3, overwrite = TRUE)
  expect_true(file.exists(dat$file[[1]]))
  nc <- ncdf4::nc_open(dat$file[[1]])
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_timeseq <- seq(lubridate::as_datetime(start_date),
                        lubridate::as_datetime(paste(end_date, "23:59:59")),
                        by = "1 hour", tz = "UTC")
  time <- lubridate::as_datetime("2009-01-01") +
    as.difftime(ncdf4::ncvar_get(nc, "time"), units = "days")
  expect_equal(time, expect_timeseq)
  temp_k <- ncdf4::ncvar_get(nc, "air_temperature")
  # June temperatures here should always be greater than 0 degC
  expect_true(all(temp_k > 273.15))
  # ...and temperatures anywhere should be less than 60 degC
  expect_true(all(temp_k < 323.15))
})
