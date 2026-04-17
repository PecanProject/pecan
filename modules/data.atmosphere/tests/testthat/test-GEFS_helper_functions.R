test_that("noaa_grid_download end times", {
  local_edition(3)
  local_mocked_bindings(
    download_file_shim = function(...) {
      dl_call_count <<- dl_call_count + 1
    }
  )
  out <- withr::local_tempdir()

  # wrapper for end-time tests
  dl_hrs <- function(end_hr, start_time = 0) {
    noaa_grid_download(
      lat_list = 0,
      lon_list = 0,
      forecast_time = start_time,
      forecast_date = Sys.Date(),
      model_name_raw = "geftest_raw",
      output_directory = out,
      end_hr = end_hr
    )
  }

  # Two timepoints (t0, 3-hr forecast)
  # Expect calls for 31 ensemble members per time
  dl_call_count <- 0
  dl_hrs(3)
  expect_equal(dl_call_count, 62)
  expect_files(out, "geftest_raw")

  # full 35 days
  # (t0 + 10 days 3-hrly + 25 days 6-hrly = 181 timepoints)
  dl_call_count <- 0
  dl_hrs(840)
  expect_equal(dl_call_count, 31 * 181)

  # more than 35 days requested -> only 35 returned
  dl_call_count <- 0
  dl_hrs(1200)
  expect_equal(dl_call_count, 31 * 181)

  # nonzero forecast time -> 16 days available
  dl_call_count <- 0
  dl_hrs(1200, 12)
  expect_equal(dl_call_count, 31 * 105)
})


test_that("noaa_grid_download bounding box", {
  local_edition(3)
  local_mocked_bindings(
    download_file_shim = function(...) {
      args <- list(...)
      called_urls <<- append(called_urls, args[[1]])
      return(0)
    }
  )
  out <- withr::local_tempdir()

  dl_loc <- function(lats = 40, lons = -88) {
    noaa_grid_download(
      lat_list = lats,
      lon_list = lons,
      forecast_time = 0,
      forecast_date = Sys.Date(),
      model_name_raw = "geftest_raw",
      output_directory = out,
      end_hr = 0
    )
  }

  # one location
  called_urls <- c()
  dl_loc()
  expect_match(
    called_urls,
    "leftlon=-88&rightlon=-88&toplat=40&bottomlat=40",
    fixed = TRUE,
    all = TRUE
  )

  # multiple locations
  called_urls <- c()
  dl_loc(c(35.5, 40, 42.9), c(-88, -89.5, -87))
  expect_match(
    called_urls,
    "leftlon=-90&rightlon=-87&toplat=43&bottomlat=35",
    fixed = TRUE,
    all = TRUE
  )
})


# TODO now that `download_grid` is a separate function,
# it would be cleaner to test skipping in download_grid directly
# and focus for noaa_grid_download on whether it correctly constructs/passes
# each chunk of the query string.
test_that("noaa_grid_download skips on error", {
  local_edition(3)
  local_mocked_bindings(
    download_file_shim = function(...) stop("NOPE!")
  )
  out <- withr::local_tempdir()

  noaa_grid_download(
    lat_list = 0,
    lon_list = 0,
    forecast_time = 0,
    forecast_date = Sys.Date(),
    model_name_raw = "geftest_raw",
    output_directory = out,
    end_hr = 3
  ) |>
    expect_warning("NOPE! skipping gec00.*f000") |>
    expect_warning("NOPE! skipping gep30.*f003") |>
    suppressWarnings() # no need to check all 62 warnings
})


# test_that("download_grid", {
#   TODO
# })


# test_that("process_gridded_noaa_download", {
#   TODO
# })


# test_that("write_noaa_gefs_netcdf", {
#   TODO
# })
