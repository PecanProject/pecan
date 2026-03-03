test_that("coalesce_na_cf_met fills NA values only", {
  skip_if_not_installed("ncdf4")

  tmp <- tempdir()

  primary <- file.path(tmp, "primary.nc")
  secondary <- file.path(tmp, "secondary.nc")
  out <- file.path(tmp, "out.nc")

  # ---- define CF-compliant time dimension
  dim_t <- ncdf4::ncdim_def(
    name  = "time",
    units = "days since 2000-01-01",
    vals  = 0:2
  )

  var_tair <- ncdf4::ncvar_def(
    name     = "air_temperature",
    units    = "K",
    dim      = dim_t,
    missval  = NA_real_,
    prec     = "double"
  )

  # ---- create primary file (contains NA)
  nc_primary <- ncdf4::nc_create(primary, vars = var_tair)
  ncdf4::ncvar_put(nc_primary, "air_temperature", c(280, NA, 282))
  ncdf4::ncatt_put(nc_primary, "air_temperature", "missing_value", NA_real_)
  ncdf4::nc_close(nc_primary)

  # ---- create secondary file (no NA)
  nc_secondary <- ncdf4::nc_create(secondary, vars = var_tair)
  ncdf4::ncvar_put(nc_secondary, "air_temperature", c(281, 281, 281))
  ncdf4::ncatt_put(nc_secondary, "air_temperature", "missing_value", NA_real_)
  ncdf4::nc_close(nc_secondary)

  # ---- run coalesce
  result <- coalesce_na_cf_met(
    primary_cf   = primary,
    secondary_cf = secondary,
    vars         = "air_temperature",
    out_file     = out
  )

  # ---- verify output path returned
  expect_identical(result, out)
  expect_true(file.exists(out))

  # ---- verify only missing values were filled
  nc_out <- ncdf4::nc_open(out)
  vals <- ncdf4::ncvar_get(nc_out, "air_temperature")
  ncdf4::nc_close(nc_out)

  expect_equal(as.numeric(vals), c(280, 281, 282))
})

test_that("coalesce_na_cf_met errors on dimension mismatch", {
  skip_if_not_installed("ncdf4")

  tmp <- tempdir()

  primary <- file.path(tmp, "primary_dim.nc")
  secondary <- file.path(tmp, "secondary_dim.nc")
  out <- file.path(tmp, "out_dim.nc")

  dim_t1 <- ncdf4::ncdim_def(
    name  = "time",
    units = "days since 2000-01-01",
    vals  = 0:2
  )

  dim_t2 <- ncdf4::ncdim_def(
    name  = "time",
    units = "days since 2000-01-01",
    vals  = 0:3
  )

  var1 <- ncdf4::ncvar_def(
    name     = "air_temperature",
    units    = "K",
    dim      = dim_t1,
    missval  = NA_real_,
    prec     = "double"
  )

  var2 <- ncdf4::ncvar_def(
    name     = "air_temperature",
    units    = "K",
    dim      = dim_t2,
    missval  = NA_real_,
    prec     = "double"
  )

  nc_primary <- ncdf4::nc_create(primary, vars = var1)
  ncdf4::ncvar_put(nc_primary, "air_temperature", c(280, NA, 282))
  ncdf4::nc_close(nc_primary)

  nc_secondary <- ncdf4::nc_create(secondary, vars = var2)
  ncdf4::ncvar_put(nc_secondary, "air_temperature", c(281, 281, 281, 281))
  ncdf4::nc_close(nc_secondary)

  expect_error(
    coalesce_na_cf_met(
      primary_cf   = primary,
      secondary_cf = secondary,
      vars         = "air_temperature",
      out_file     = out
    )
  )
})
