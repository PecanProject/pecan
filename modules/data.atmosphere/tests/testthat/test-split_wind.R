expect_wind_components <- function(east, north, direction = NULL, units = NULL) {
  folder <- withr::local_tempdir()
  path <- file.path(folder, "2001.nc")
  dims <- list(
    ncdf4::ncdim_def("latitude", "degrees_north", 42),
    ncdf4::ncdim_def("longitude", "degrees_east", -72),
    ncdf4::ncdim_def("time", "hours since 2001-01-01 00:00:00", 0:1)
  )
  vars <- list(ncdf4::ncvar_def("wind_speed", "m s-1", dims, -9999))
  if (!is.null(direction)) {
    vars[[2]] <- ncdf4::ncvar_def("wind_direction", units, dims, -9999)
  }
  nc <- ncdf4::nc_create(path, vars)
  tryCatch({
    ncdf4::ncvar_put(nc, "wind_speed", c(3, 4))
    if (!is.null(direction)) {
      ncdf4::ncvar_put(nc, "wind_direction", direction)
    }
  }, finally = ncdf4::nc_close(nc))

  split_wind(folder, "", "2001-01-01", "2001-12-31")

  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expected <- list(eastward_wind = east, northward_wind = north)
  for (name in names(expected)) {
    actual <- as.numeric(ncdf4::ncvar_get(nc, name))
    expect_equal(actual, expected[[name]], tolerance = 1e-6, scale = 1)
    expect_equal(ncdf4::ncatt_get(nc, name, "units")$value, "m s-1")
  }
}

test_that("split_wind writes components for direction in degrees", {
  expect_wind_components(c(0, 4), c(3, 0), c(0, 90), "degrees")
})

test_that("split_wind writes components for direction in radians", {
  expect_wind_components(c(0, 4), c(3, 0), c(0, pi / 2), "radians")
})

test_that("split_wind uses eastward speed when direction is absent", {
  expect_wind_components(c(3, 4), c(0, 0))
})
