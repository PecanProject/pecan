context("downscaling")

daily.nc <- ncdf4::nc_open("data/urbana_daily_test.nc")
on.exit(ncdf4::nc_close(daily.nc), add = TRUE)
daily.cf <- load.cfmet(met.nc = daily.nc, lat = 39.75, lon = -87.25,
                       start.date = "1951-01-02", end.date = "1951-05-31")

test_that(
  paste("cfmet.downscale.time works\n",
        "these are capturing the current state of the downscale algorithms;\n", 
        "actual values will need to be revised if (when) algorithms change"), 
{
  b <- cfmet.downscale.time(cfmet = daily.cf, lat = 40)
  expect_equal(unique(b$year), 1951)
  expect_equal(range(b$doy), c(2,151))
  expect_equal(unique(b$hour), 0:23)
  expect_equal(round(range(b$downwelling_photosynthetic_photon_flux)), c(0, 2061))
  expect_equal(round(range(b$air_temperature)), c(-22, 31))
  #  expect_equal(round(range(b$relative_humidity)), c(0.30569194491299, 1))
  expect_equal(signif(range(b$precipitation_flux), 3), c(0, 1.67e-05))
  expect_equal(signif(range(b$wind), 2), c(0.066, 6.60))
})

test_that("downscaling with timestep", {
  df <- data.frame(
    year = 2020, doy = 100,
    air_temperature_min = 293.15, air_temperature_max = 303.15, air_temperature = 298.15,
    surface_downwelling_shortwave_flux_in_air = 1000,
    air_pressure = 1030,
    wind_speed = 0,
    relative_humidity = 0.5,
    precipitation_flux = 2 / (60 * 60)) # units: mm/sec

  r1 <- cfmet.downscale.daily(df, output.dt = 1, lat = 40)
  r6 <- cfmet.downscale.daily(df, output.dt = 6, lat = 40)
  r12 <- cfmet.downscale.daily(df, output.dt = 12, lat = 40)

  expect_equal(nrow(r1), 24)
  expect_equal(nrow(r6), 4)
  expect_equal(nrow(r12), 2)

  list(r1, r6,r12) %>%
  purrr::walk(~{
    expect_equal(mean(.$air_temperature), (df$air_temperature - 273.15)) # input is K, output is C
    expect_equal(sum(.$precipitation_flux), df$precipitation_flux)
    expect_true(all(.$wind == df$wind_speed))
  })

})

test_that("output for a given day not affected by adjacent days", {
  df <- data.frame(
    year = 2020, doy = 100:101,
    air_temperature_min = 293.15 + c(0, 10),
    air_temperature_max = 303.15 + c(0, 10),
    air_temperature = 298.15 + c(0, 10),
    surface_downwelling_shortwave_flux_in_air = c(1000, 2000),
    air_pressure = 1030,
    wind_speed = 0,
    relative_humidity = 0.5,
    precipitation_flux = c(0, 2 / (60 * 60)))

  # print(cfmet.downscale.daily(df[2,], 6, 40))
  # print(cfmet.downscale.daily(df, 6, 40))
  expect_equal(cfmet.downscale.daily(df[1,], 6, 40), cfmet.downscale.daily(df, 6, 40)[1:4,])
  expect_equal(cfmet.downscale.daily(df[2,], 6, 40), cfmet.downscale.daily(df, 6, 40)[5:8,])
})

test_that("get.ncvector works",{
  run.dates <- data.frame(index = 1:2, date = c(lubridate::ymd("1951-01-01 UTC"), lubridate::ymd("1951-01-02 UTC")))
  res <- get.ncvector("air_temperature", lati = 1, loni = 1, run.dates, met.nc = daily.nc)
  expect_type(res, "double")
  expect_equal(length(res), nrow(run.dates))
})
