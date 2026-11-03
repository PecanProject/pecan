make_sm_cds_file <- function(
    directory,
    date,
    sm,
    prefix = "surface_soil_moisture"
) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(
    directory,
    paste(prefix, date, "nc", sep = ".")
  )
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(-1, 1))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(-1, 1))
  sm_var <- ncdf4::ncvar_def(
    "sm", "1", list(lon, lat), missval = -9999, prec = "double"
  )
  uncertainty_var <- ncdf4::ncvar_def(
    "sm_uncertainty", "1", list(lon, lat), missval = -9999, prec = "double"
  )
  nc <- ncdf4::nc_create(path, list(sm_var, uncertainty_var))
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncvar_put(nc, sm_var, matrix(sm, 2, 2))
  ncdf4::ncvar_put(nc, uncertainty_var, matrix(sm / 10, 2, 2))
  path
}

test_that("extract_SM_CDS selects files by date in a dotted directory", {
  skip_if_not_installed("raster")
  skip_if_not_installed("sp")
  directory <- file.path(withr::local_tempdir(), "cds.cache")
  make_sm_cds_file(directory, "1992-01-01", 0.92)
  make_sm_cds_file(directory, "1992-01-02", 0.93)
  make_sm_cds_file(directory, "2000-01-01", 0.20)
  make_sm_cds_file(directory, "2000-01-02", 0.21)

  result <- extract_SM_CDS(
    site_info = list(site_id = "synthetic", lon = 0, lat = 0),
    time.points = as.Date("2000-01-01"),
    in.path = directory,
    allow.download = FALSE,
    search_window = 2
  )

  expect_equal(result$sm.mean, 0.20)
  expect_equal(result$sm.uncertainty, 0.02)
})

test_that("extract_SM_CDS ignores unrelated files", {
  skip_if_not_installed("raster")
  skip_if_not_installed("sp")
  directory <- withr::local_tempdir()
  make_sm_cds_file(directory, "2000-01-01", 0.20)
  make_sm_cds_file(directory, "2000-01-02", 0.21)
  writeLines("not NetCDF data", file.path(directory, "notes.txt"))

  result <- extract_SM_CDS(
    site_info = list(site_id = "synthetic", lon = 0, lat = 0),
    time.points = as.Date("2000-01-01"),
    in.path = directory,
    allow.download = FALSE,
    search_window = 2
  )

  expect_equal(result$sm.mean, 0.20)
})

test_that("extract_SM_CDS ignores duplicate-looking dates in other files", {
  skip_if_not_installed("raster")
  skip_if_not_installed("sp")
  directory <- withr::local_tempdir()
  make_sm_cds_file(directory, "2000-01-01", 0.99, prefix = "aaa")
  make_sm_cds_file(directory, "2000-01-01", 0.20)
  make_sm_cds_file(directory, "2000-01-02", 0.21)

  result <- extract_SM_CDS(
    site_info = list(site_id = "synthetic", lon = 0, lat = 0),
    time.points = as.Date("2000-01-01"),
    in.path = directory,
    allow.download = FALSE,
    search_window = 2
  )

  expect_equal(result$sm.mean, 0.20)
})

test_that("extract_SM_CDS combines matching existing and downloaded dates", {
  skip_if_not_installed("raster")
  skip_if_not_installed("sp")
  directory <- withr::local_tempdir()
  make_sm_cds_file(directory, "1992-01-01", 0.92)
  make_sm_cds_file(directory, "2000-01-01", 0.20)
  downloaded_dates <- character()
  local_mocked_bindings(
    download.SM_CDS = function(outfolder, time.points, ...) {
      downloaded_dates <<- as.character(time.points)
      vapply(
        time.points,
        function(date) make_sm_cds_file(outfolder, as.character(date), 0.21),
        character(1)
      )
    },
    .package = "PEcAn.data.land"
  )

  result <- extract_SM_CDS(
    site_info = list(site_id = "synthetic", lon = 0, lat = 0),
    time.points = as.Date("2000-01-01"),
    in.path = directory,
    allow.download = TRUE,
    search_window = 2
  )

  expect_equal(downloaded_dates, "2000-01-02")
  expect_equal(result$sm.mean, 0.20)
  expect_equal(result$sm.uncertainty, 0.02)
})
