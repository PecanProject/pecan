context("testing functions used to load and downscaling PEcAn-CF met drivers")
library(data.table)
test_that("load.cfmet works as expected", {
  met.nc <- nc_open(system.file("extdata/urbana_daily_test.nc", package = "PEcAn.data.atmosphere"))
  a <- load.cfmet(met.nc = met.nc, lat = 39.75, lon = -87.25, start.date = "1951-01-01", end.date = "1951-06-01")
  
  expect_is(a, "data.frame")
  expect_is(a, "data.table")
  
  expect_is(a$date, "POSIXct")
  expect_is(a$date, "POSIXt")
  
  expect_true(all(a$year == 1951))
  expect_true(all(a$day %in% 1:31))
  expect_true(all(diff(a$doy) > 0)) ## test dataset is for 1/2 year so day of year should be increasing
  expect_true(all(c("index", "date", "doy", "year", "month", "day", "hour", "surface_downwelling_longwave_flux_in_air", 
                    "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", 
                    "surface_pressure", "wind", "air_temperature") %in% 
                    colnames(a)))
})
