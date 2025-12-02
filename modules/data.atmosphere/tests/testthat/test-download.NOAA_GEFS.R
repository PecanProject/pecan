# Verify that helpers are called with appropriate arguments
# (We test the actual download and conversion in the helper test file)
test_that("GEFS interface, helpers mocked out", {
  local_edition(3)
  out <- withr::local_tempdir()
  start_date <- Sys.Date() - lubridate::days(1)

  # Two full days
  with_mocked_bindings(
    {
      res <- download.NOAA_GEFS(
        site_id = "test_site",
        lat.in = 40,
        lon.in = -88,
        start_date = start_date,
        end_date = start_date + lubridate::days(2),
        outfolder = out
      )
    },
    noaa_grid_download = function(...) {
      args <- list(...)
      expect_equal(args$forecast_date, start_date)
      expect_equal(args$forecast_time, 0)
      expect_equal(args$end_hr, 48)

      NULL
    },
    process_gridded_noaa_download = function(...) NULL
  )


  # Nonzero start hour
  with_mocked_bindings(
    {
      res <- download.NOAA_GEFS(
        site_id = "test_site",
        lat.in = 32,
        lon.in = -115,
        start_date = start_date + lubridate::hours(8),
        end_date = start_date + lubridate::hours(22),
        outfolder = out
      )
    },
    noaa_grid_download = function(...) {
      args <- list(...)
      expect_equal(args$forecast_date, start_date)
      expect_equal(args$forecast_time, 6)
      expect_equal(args$end_hr, 12)

      NULL
    },
    process_gridded_noaa_download = function(...) NULL
  )
})


# Verify responses from live GEFS server
# (This downloads >200 grib files. They're small, but it still takes time)
test_that("GEFS live server (slow!)", {
  skip_on_ci()
  out <- withr::local_tempdir()
  start_date <- Sys.Date() - lubridate::days(1)
  end_date <- start_date + lubridate::hours(18)

  res <- download.NOAA_GEFS(
    site_id = "test_site",
    lat.in = 40,
    lon.in = -88,
    start_date = start_date,
    end_date = end_date,
    outfolder = out
  )

  expect_files(
    file.path(out, "NOAAGEFS_raw", start_date, "00"),
    c("gec00.t00z.pgrb2a.0p50.f000.grib", "gep30.t00z.pgrb2a.0p50.f018.grib")
  )

  ensemble_members <- sprintf("%02i", 0:30)
  outnames <- paste(
    "NOAA_GEFS_test_site", ensemble_members,
    format(start_date, "%Y-%m-%dT%H:%M"), format(end_date, "%Y-%m-%dT%H:%M"),
    sep = "_"
  )
  expect_files(
    out,
    paste0(outnames, "/", outnames, ".nc"),
    recursive = TRUE
  )
})
