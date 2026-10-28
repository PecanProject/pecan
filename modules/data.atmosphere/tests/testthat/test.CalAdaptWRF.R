make_caladapt_source <- function() {
  path <- tempfile(fileext = ".nc")
  dims <- list(
    ncdf4::ncdim_def("x", "m", 500000 + 3000 * (0:2)),
    ncdf4::ncdim_def("y", "m", 4000000 + 3000 * (0:2)),
    ncdf4::ncdim_def("time", "hours since 2024-01-01", 0:(365 * 24 - 1))
  )
  defaults <- c(
    t2 = 285, psfc = 100000, prec = 1, q2 = .005,
    swdnb = 100, lwdnb = 300, u10 = 3, v10 = 4
  )
  units <- c("K", "Pa", "mm", "kg kg-1", "W m-2", "W m-2", "m s-1", "m s-1")
  vars <- lapply(seq_along(defaults), function(i) ncdf4::ncvar_def(names(defaults)[i], units[i], dims, -9999))
  nc <- ncdf4::nc_create(path, c(vars, list(ncdf4::ncvar_def("crs", "", list(), -9999))))
  for (v in names(defaults)) {
    values <- array(defaults[v], c(3, 3, 8760))
    if (v == "t2") values[, , 3] <- NA_real_
    ncdf4::ncvar_put(nc, v, values)
    ncdf4::ncatt_put(nc, v, "grid_mapping", "crs")
  }
  ncdf4::ncatt_put(nc, "crs", "spatial_ref", sf::st_crs(32611)$wkt)
  ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
  ncdf4::nc_close(nc)
  xy <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(503000, 4003000)), crs = 32611), 4326))
  list(path = path, lat = xy[2], lon = xy[1], variables = names(defaults))
}

test_that("download and CF conversion remain separate and preserve source data", {
  skip_if_not_installed("caladaptaer")
  skip_if_not("cae_stage_zarr" %in% getNamespaceExports("caladaptaer"), "Requires the caladaptaer staging stack")
  source <- make_caladapt_source()
  withr::defer(unlink(source$path))
  out <- withr::local_tempdir()
  calls <- list()
  testthat::local_mocked_bindings(
    cae_check_variables = function(...) data.frame(variable = source$variables, available = TRUE),
    cae_stage_zarr = function(variable, model, scenario, points, cache_dir, ...) {
      calls[[length(calls) + 1L]] <<- list(variable = variable, site_id = points$site_id)
      source$path
    }, .package = "caladaptaer"
  )
  raw <- download.CalAdaptWRF(out, "2024-01-01", "2024-12-31", "001-field", source$lat, source$lon)
  expect_length(calls, 8L)
  expect_true(all(vapply(calls, function(x) identical(x$site_id, "001-field"), logical(1))))
  expect_identical(raw$formatname, "CalAdaptWRF source manifest")
  expect_length(list.files(out, pattern = "\\.nc$"), 0L)
  manifest <- jsonlite::read_json(raw$file, simplifyVector = TRUE)
  expect_identical(manifest$sites$site_id, "001-field")
  cf <- met2CF.CalAdaptWRF(out, raw$dbfile.name, file.path(out, "cf"), "2024-01-01", "2024-12-31")
  expect_identical(cf$dbfile.name, raw$dbfile.name)
  expect_identical(basename(cf$file), paste0(raw$dbfile.name, ".2024.nc"))
  nc <- ncdf4::nc_open(cf$file)
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "specific_humidity")), rep(.005 / 1.005, 8760), tolerance = 1e-8)
  expect_equal(sum(ncdf4::ncvar_get(nc, "precipitation_flux")) * 3600, 8760, tolerance = 1e-5)
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "wind_speed")), rep(5, 8760))
  expect_true(is.na(ncdf4::ncvar_get(nc, "air_temperature")[3]))
  expect_identical(ncdf4::ncatt_get(nc, "time", "calendar")$value, "365_day")
  expect_match(ncdf4::ncatt_get(nc, 0, "Conventions")$value, "CF-1.8")
  ncdf4::nc_close(nc)
  before <- file.info(c(raw$file, cf$file))$mtime
  download.CalAdaptWRF(out, "2024-01-01", "2024-12-31", "001-field", source$lat, source$lon)
  met2CF.CalAdaptWRF(out, raw$dbfile.name, file.path(out, "cf"), "2024-01-01", "2024-12-31")
  expect_equal(file.info(c(raw$file, cf$file))$mtime, before)
  writeLines("interrupted", cf$file)
  met2CF.CalAdaptWRF(out, raw$dbfile.name, file.path(out, "cf"), "2024-01-01", "2024-12-31")
  expect_equal(file.info(cf$file)$size, file.info(file.path(out, "cf", raw$dbfile.name, "001-field", "CESM2.ssp370.2024.nc"))$size)
  manifest$resolution <- "d01"
  jsonlite::write_json(manifest, raw$file, auto_unbox = TRUE, digits = NA)
  expect_error(met2CF.CalAdaptWRF(out, raw$dbfile.name, file.path(out, "cf"), "2024-01-01", "2024-12-31"), "identity or cache")
})

test_that("component precipitation stages the preceding source record", {
  skip_if_not_installed("caladaptaer")
  skip_if_not("cae_stage_zarr" %in% getNamespaceExports("caladaptaer"))
  out <- withr::local_tempdir()
  starts <- list()
  testthat::local_mocked_bindings(
    cae_check_variables = function(...) data.frame(variable = c("t2", "psfc", "q2", "swdnb", "lwdnb", "u10", "v10", "rainc", "rainnc"), available = TRUE),
    cae_stage_zarr = function(variable, model, scenario, points, cache_dir, start_time, ...) {
      starts[[variable]] <<- start_time
      out
    }, .package = "caladaptaer"
  )
  download.CalAdaptWRF(out, "2025-01-01", "2025-12-31", "field", 38, -121)
  expect_equal(starts$rainc, as.POSIXct("2024-12-31 23:00:00", tz = "UTC"))
  expect_equal(starts$rainnc, starts$rainc)
  expect_equal(starts$t2, as.POSIXct("2025-01-01", tz = "UTC"))
  expect_error(download.CalAdaptWRF(out, "2025-01-01", "2025-12-31", "../field", 38, -121), "safe")
  expect_error(download.CalAdaptWRF(out, "2025-12-31", "2025-01-01", "field", 38, -121), "ordered")
})

test_that("site registration uses the normal met2CF dispatcher without permutation", {
  convert <- mockery::mock(list(input.id = 20, dbfile.id = 21))
  mockery::stub(.met2cf.module, "PEcAn.DB::convert_input", convert)
  mockery::stub(.met2cf.module, "PEcAn.DB::query.format.vars", list())
  .met2cf.module(
    raw.id = list(input.id = 10), register = list(scale = "site", format = list(mimetype = "application/json")),
    met = "CalAdaptWRF", str_ns = "field", dir = tempdir(), site.id = 1, lat = 38, lon = -121,
    start_date = "2024-01-01", end_date = "2024-12-31", con = NULL, host = list(name = "localhost"), format.vars = list(), bety = NULL
  )
  mockery::expect_called(convert, 1)
  expect_identical(mockery::mock_args(convert)[[1]]$fcn, "met2CF.CalAdaptWRF")
})
