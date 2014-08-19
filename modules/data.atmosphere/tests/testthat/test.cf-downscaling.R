context("loading data from PEcAn-CF met drivers")

daily.nc <- nc_open(system.file("extdata/urbana_daily_test.nc", package = "PEcAn.data.atmosphere"))
daily.cf <- load.cfmet(met.nc = daily.nc, lat = 39.75, lon = -87.25, start.date = "1951-01-02", end.date = "1951-06-01")

subdaily.nc <- nc_open(system.file("extdata/urbana_subdaily_test.nc", package = "PEcAn.data.atmosphere"))
subdaily.cf <- load.cfmet(met.nc = subdaily.nc, lat = 39.75, lon = -87.25, start.date = "1979-01-02", end.date = "1979-06-01")

test_that("data extracted from test pecan-cf met files is valid",{
  
  expect_is(daily.cf, "data.frame")    
  expect_is(daily.cf, "data.table")
  
  expect_is(daily.cf$date, "POSIXct")
  expect_is(daily.cf$date, "POSIXt")
  
  expect_true(all(daily.cf$year == 1951))
  expect_true(all(daily.cf$day %in% 1:31))
  expect_true(all(diff(daily.cf$doy) > 0)) ## test dataset is for 1/2 year so day of year should be increasing
  expect_true(all(c("index", "date", "doy", "year", "month", "day", "hour", "surface_downwelling_longwave_flux_in_air", 
                    "surface_downwelling_shortwave_flux_in_air", "air_temperature", 
                    "air_temperature_max", "air_temperature_min", "northward_wind", 
                    "eastward_wind", "relative_humidity") %in% 
                    colnames(daily.cf)))
})


test_that("load.cfmet throws error if start/end date out of range",{
  PEcAn.utils::logger.setLevel("OFF")
  expect_error(load.cfmet(met.nc = subdaily.nc, lat = 39, lon = -88, start.date = "9999-01-01", end.date = "9999-02-02"))
  expect_error(load.cfmet(met.nc = subdaily.nc, lat = 39, lon = -88, start.date = "0000-01-01", end.date = "0000-02-02"))
})

context("downscaling")

test_that(
  paste("cfmet.downscale.time works\n",
        "these are capturing the current state of the downscale algorithms;\n", 
        "actual values will need to be revised if (when) algorithms change"), 
{
  b <- cfmet.downscale.time(cfmet = daily.cf, lat = 40)
  expect_equal(b[,unique(year)], 1951)
  expect_equal(b[,range(doy)], c(3,151))
  expect_equal(b[,unique(hour)], 0:23)
  expect_equal(b[,round(range(downwelling_photosynthetic_photon_flux))], c(0, 2061))
  expect_equal(b[,round(range(air_temperature))], c(-22, 31))
  #  expect_equal(b[,round(range(relative_humidity))], c(0.30569194491299, 1))
  expect_equal(b[,signif(range(precipitation_flux), 3)], c(0.00, 1.45))
  expect_equal(b[,signif(range(wind), 2)], c(0.066, 6.60))
})


test_that("get.ncvector works",{
  run.dates <- data.table(index = 1:2, date = c(ymd("1951-01-01 UTC"), ymd("1951-01-02 UTC")))
  c <- get.ncvector("air_temperature", lati = 1, loni = 1, run.dates, met.nc = daily.nc)
})
