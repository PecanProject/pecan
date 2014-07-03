context("testing functions used to load and downscaling PEcAn-CF met drivers")

daily.nc <- nc_open(system.file("extdata/urbana_daily_test.nc", package = "PEcAn.data.atmosphere"))
daily.cf <- load.cfmet(met.nc = daily.nc, lat = 39.75, lon = -87.25, start.date = "1951-01-01", end.date = "1951-06-01")

#subdaily.nc <- nc_open(system.file("extdata/urbana_subdaily_narr_test.nc", package = "PEcAn.data.atmosphere"))
#subdaily.cf <- load.cfmet(met.nc = subdaily.nc, lat = 39.75, lon = -87.25, start.date = "1981-01-01", end.date = "1981-06-01")


test_that("load.cfmet works as expected", {

  expect_is(daily.cf, "data.frame")    
  expect_is(daily.cf, "data.table")
    
  expect_is(daily.cf$date, "POSIXct")
  expect_is(daily.cf$date, "POSIXt")
  
  expect_true(all(daily.cf$year == 1951))
  expect_true(all(daily.cf$day %in% 1:31))
  expect_true(all(diff(daily.cf$doy) > 0)) ## test dataset is for 1/2 year so day of year should be increasing
  expect_true(all(c("index", "date", "doy", "year", "month", "day", "hour", "surface_downwelling_longwave_flux_in_air", 
                    "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", 
                    "surface_pressure", "wind", "air_temperature") %in% 
                    colnames(daily.cf)))
})

test_that(
  paste("cfmet.downscale.time works\n",
        "these are capturing the current state of the downscale algorithms;\n", 
        "actual values will need to be revised if (when) algorithms change"), 
{
  b <- cfmet.downscale.time(cfmet = daily.cf, lat = 40)
  expect_equal(b[,unique(year)], 1951)
  expect_equal(b[,range(doy)], c(2,151))
  expect_equal(b[,unique(hour)], 0:23)
  expect_equal(b[,range(downwelling_photosynthetic_photon_flux)], c(0, 2057.82915482773))
  expect_equal(b[,range(air_temperature)], c(-20.1685943603516, 31.2723083496094))
  expect_equal(b[,range(relative_humidity)], c(0.30569194491299, 1))
  expect_equal(b[,range(precipitation_flux)], c(0, 1.40816308557987))
  expect_equal(b[,range(wind)], c(0.315869278266082, 6.86749991864085))
})


test_that("get.ncvector works",{
  run.dates <- data.table(index = 1:2, date = c(ymd("1951-01-01 UTC"), ymd("1951-01-02 UTC")))
  c <- get.ncvector("air_temperature", lati = 1, loni = 1, run.dates, met.nc = daily.nc)
})
