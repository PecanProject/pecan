context("loading data from PEcAn-CF met drivers")

daily.nc <- ncdf4::nc_open("data/urbana_daily_test.nc")
on.exit(ncdf4::nc_close(daily.nc))
daily.cf <- load.cfmet(met.nc = daily.nc, lat = 39.75, lon = -87.25,
                       start.date = "1951-01-02", end.date = "1951-06-01")
subdaily.nc <- ncdf4::nc_open("data/urbana_subdaily_test.nc")
on.exit(ncdf4::nc_close(subdaily.nc), add=TRUE)

test_that("data extracted from test pecan-cf met files is valid",{
  expect_is(daily.cf, "data.frame")
  expect_is(daily.cf, "data.table")

  expect_is(daily.cf$date, "POSIXct")
  expect_is(daily.cf$date, "POSIXt")
  expect_is(daily.cf$index, "integer")
  expect_is(daily.cf$doy, "numeric")
  expect_is(daily.cf$air_temperature, "numeric")
  expect_is(daily.cf$relative_humidity, "numeric")

  expect_true(all(daily.cf$year == 1951))
  expect_true(all(daily.cf$day %in% 1:31))
  expect_true(all(diff(daily.cf$doy) > 0)) ## test dataset is for 1/2 year so day of year should be increasing
  expect_true(all(c("index", "date", "doy", "year", "month", "day", "hour",
                    "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air",
                    "air_temperature", "air_temperature_max", "air_temperature_min",
                    "northward_wind", "eastward_wind", "relative_humidity")
                  %in% colnames(daily.cf)))
})

test_that("load.cfmet respects start/end date",{
  expect_equal(strftime(min(daily.cf$date), "%F"), "1951-01-01")
  expect_equal(strftime(max(daily.cf$date), "%F"), "1951-06-01")
  expect_equal(nrow(daily.cf), 152)
})

test_that("load.cfmet throws error if start/end date out of range",{
  PEcAn.utils::logger.setLevel("OFF")
  expect_error(load.cfmet(met.nc = subdaily.nc, lat = 39, lon = -88,
                          start.date = "9999-01-01", end.date = "9999-02-02"))
  expect_error(load.cfmet(met.nc = subdaily.nc, lat = 39, lon = -88,
                          start.date = "0000-01-01", end.date = "0000-02-02"))
  expect_error(load.cfmet(met.nc = daily.nc, lat = 39, lon = -88,
                          start.date = "1950-12-31", end.date = "1951-12-31"),
               "run start date .* before met data starts")
  expect_error(load.cfmet(met.nc = daily.nc, lat = 39, lon = -88,
                          start.date = "1951-01-01", end.date = "1952-01-01"),
               "run start date .* after met data ends")
})

test_that("load.cfmet enforces lat/lon matching",{
  expect_is(load.cfmet(met.nc = daily.nc, lat = 39, lon = -88,
                       start.date = "1951-01-01", end.date = "1951-01-07"),
            "data.frame")
  expect_error(load.cfmet(met.nc = daily.nc, lat = 39, lon = 20,
                          start.date = "1951-01-01", end.date = "1951-01-07"),
               "lat / lon .* outside range of met file")
  expect_error(load.cfmet(met.nc = daily.nc, lat = 9, lon = -88,
                          start.date = "1951-01-01", end.date = "1951-01-07"),
               "lat / lon .* outside range of met file")

})
