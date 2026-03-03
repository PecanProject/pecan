# No fallback required → primary copied unchanged
test_that("metgapfill_with_fallback returns primary file unchanged when no fallback needed", {
  skip_if_not_installed("ncdf4")

  tmp <- tempdir()

  primary <- file.path(tmp, "primary_full.nc")
  fallback <- file.path(tmp, "fallback_dummy.nc")
  out <- file.path(tmp, "out_full.nc")

  dim_time <- ncdf4::ncdim_def(
    name  = "time",
    units = "days since 2000-01-01",
    vals  = 0:2
  )

  var_tair <- ncdf4::ncvar_def(
    name    = "air_temperature",
    units   = "K",
    dim     = dim_time,
    missval = NA_real_,
    prec    = "double"
  )

  # Primary: no missing values
  nc_p <- ncdf4::nc_create(primary, vars = var_tair)
  ncdf4::ncvar_put(nc_p, "air_temperature", c(280, 281, 282))
  ncdf4::nc_close(nc_p)

  # Fallback: not used
  nc_f <- ncdf4::nc_create(fallback, vars = var_tair)
  ncdf4::ncvar_put(nc_f, "air_temperature", c(279, 279, 279))
  ncdf4::nc_close(nc_f)

  result <- metgapfill_with_fallback(
    primary_cf  = primary,
    vars        = "air_temperature",
    fallback_cf = fallback,
    out_file    = out
  )

  expect_identical(result, out)
  expect_true(file.exists(out))

  nc_o <- ncdf4::nc_open(out)
  vals <- ncdf4::ncvar_get(nc_o, "air_temperature")
  ncdf4::nc_close(nc_o)

  expect_equal(as.numeric(vals), c(280, 281, 282))
})


#  Missing values filled from fallback
test_that("fills missing values when fallback is required", {
  skip_if_not_installed("ncdf4")

  tmp <- tempdir()

  primary <- file.path(tmp, "primary_fill.nc")
  fallback <- file.path(tmp, "fallback_fill.nc")
  out <- file.path(tmp, "out_fill.nc")

  dim_time <- ncdf4::ncdim_def(
    name  = "time",
    units = "days since 2000-01-01",
    vals  = 0:2
  )

  var_tair <- ncdf4::ncvar_def(
    name    = "air_temperature",
    units   = "K",
    dim     = dim_time,
    missval = NA_real_,
    prec    = "double"
  )

  # Primary contains NA
  nc_p <- ncdf4::nc_create(primary, vars = var_tair)
  ncdf4::ncvar_put(nc_p, "air_temperature", c(280, NA, 282))
  ncdf4::nc_close(nc_p)

  # Fallback provides value
  nc_f <- ncdf4::nc_create(fallback, vars = var_tair)
  ncdf4::ncvar_put(nc_f, "air_temperature", c(279, 281, 279))
  ncdf4::nc_close(nc_f)

  result <- metgapfill_with_fallback(
    primary_cf  = primary,
    vars        = "air_temperature",
    fallback_cf = fallback,
    out_file    = out
  )

  expect_identical(result, out)
  expect_true(file.exists(out))

  nc_o <- ncdf4::nc_open(out)
  vals <- ncdf4::ncvar_get(nc_o, "air_temperature")
  ncdf4::nc_close(nc_o)

  expect_equal(as.numeric(vals), c(280, 281, 282))
})

# Mismatched time dimensions → error
test_that("errors for mismatched time dimensions", {
  skip_if_not_installed("ncdf4")

  primary <- system.file(
    "testthat/data/urbana_subdaily_test.nc",
    package = "PEcAn.data.atmosphere"
  )

  fallback <- system.file(
    "testthat/data/urbana_daily_test.nc",
    package = "PEcAn.data.atmosphere"
  )

  out_tmp <- tempfile(fileext = ".nc")

  expect_error(
    metgapfill_with_fallback(
      primary_cf  = primary,
      vars        = "surface_downwelling_shortwave_flux_in_air",
      fallback_cf = fallback,
      out_file    = out_tmp
    )
  )
})
