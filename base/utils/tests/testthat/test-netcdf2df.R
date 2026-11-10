test_that("one dim", {
  local_edition(3)

  res <- c("a", "b") |>
    example_netcdf(file_path = withr::local_tempfile()) |>
    netcdf2df()

  expect_equal(dim(res), c(365, 3))
  expect_equal(colnames(res), c("time", "a", "b"))
  expect_equal(
    attr(res, "units"),
    c(time = "days since 2001-01-01", a = "kg", b = "kg")
  )
  expect_equal(res$time, (0L:364L))
})



test_that("multiple dims, all but one scalar", {
  local_edition(3)

  # TODO does this work if pkg not installed (eg check time)?
  # I think testthat may shim system.file
  res <- netcdf2df(
    system.file("test-data/CRUNCEP.2000.nc", package = "PEcAn.utils")
  )

  expect_equal(dim(res), c(366 * 4, 11))
  expect_equal(
    colnames(res),
    c(
      "latitude", "longitude", "time",
      "air_temperature", "surface_downwelling_longwave_flux_in_air",
      "air_pressure", "surface_downwelling_shortwave_flux_in_air",
      "eastward_wind", "northward_wind",
      "specific_humidity", "precipitation_flux"
    )
  )

  # Check units. NB some of these are nonstandard;
  # Key point is they should match what the file reports
  expect_equal(
    attr(res, "units"),
    c(
      latitude = "degree_north",
      longitude = "degree_east",
      time = "days since 2000-01-01T00:00:00Z",
      air_temperature = "Kelvin",
      surface_downwelling_longwave_flux_in_air = "W/m2",
      air_pressure = "Pascal",
      surface_downwelling_shortwave_flux_in_air = "W/m2",
      eastward_wind = "m/s",
      northward_wind = "m/s",
      specific_humidity = "g/g",
      precipitation_flux = "kg/m2/s"
    )
  )
  # dims
  expect_equal(res$time, seq(0, 365 + 3 / 4, 1 / 4) + 1 / 8, tolerance = 1e-12)
  expect_equal(unique(res$latitude), 45.25, tolerance = 1e-6)
  expect_equal(unique(res$longitude), -84.75, tolerance = 1e-6)
  expect_equal(mean(res$air_temperature), 278.798636, tolerance = 1e-6)
})

# TODO test with more than one non-degenerate dimension
