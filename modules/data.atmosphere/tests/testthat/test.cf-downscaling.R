context("downscaling")

daily.nc <- ncdf4::nc_open("data/urbana_daily_test.nc")
on.exit(ncdf4::nc_close(daily.nc))
daily.cf <- load.cfmet(met.nc = daily.nc, lat = 39.75, lon = -87.25,
                       start.date = "1951-01-02", end.date = "1951-06-01")

test_that(
  paste("cfmet.downscale.time works\n",
        "these are capturing the current state of the downscale algorithms;\n", 
        "actual values will need to be revised if (when) algorithms change"), 
{
  skip("Broken test #1343")
  b <- cfmet.downscale.time(cfmet = daily.cf, lat = 40)
  expect_equal(b[,unique(year)], 1951)
  expect_equal(b[,range(doy)], c(2,151))
  expect_equal(b[,unique(hour)], 0:23)
  expect_equal(b[,round(range(downwelling_photosynthetic_photon_flux))], c(0, 2061))
  expect_equal(b[,round(range(air_temperature))], c(-22, 31))
  #  expect_equal(b[,round(range(relative_humidity))], c(0.30569194491299, 1))
  expect_equal(b[,signif(range(precipitation_flux), 3)], c(0, 1.67e-05))
  expect_equal(b[,signif(range(wind), 2)], c(0.066, 6.60))
})


test_that("get.ncvector works",{
  run.dates <- data.table::data.table(index = 1:2, date = c(lubridate::ymd("1951-01-01 UTC"), lubridate::ymd("1951-01-02 UTC")))
  c <- get.ncvector("air_temperature", lati = 1, loni = 1, run.dates, met.nc = daily.nc)
})
