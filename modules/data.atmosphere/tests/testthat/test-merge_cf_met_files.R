context("merge_cf_met_files")

test_that("merge_cf_met_files fills NA values only", {

  skip_if_not_installed("ncdf4")

  tmp <- tempdir()

  primary <- file.path(tmp, "primary.nc")
  secondary <- file.path(tmp, "secondary.nc")
  out <- file.path(tmp, "out.nc")

  # ---- define dimensions and variable
  dim_t <- ncdf4::ncdim_def(
    name  = "time",
    units = "days",
    vals  = 1:3
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
  ncdf4::ncvar_put(
    nc_primary,
    varid = "air_temperature",
    vals  = c(280, NA, 282)
  )
  ncdf4::nc_close(nc_primary)

  # ---- create secondary file (no NA)
  nc_secondary <- ncdf4::nc_create(secondary, vars = var_tair)
  ncdf4::ncvar_put(
    nc_secondary,
    varid = "air_temperature",
    vals  = c(281, 281, 281)
  )
  ncdf4::nc_close(nc_secondary)

  # ---- run merge
  result <- merge_cf_met_files(
    primary_cf   = primary,
    secondary_cf = secondary,
    vars         = "air_temperature",
    out_file     = out
  )


  # ---- verify output path returned
  expect_identical(result, out)
  expect_true(file.exists(out))

  # ---- verify merged values
  nc_out <- ncdf4::nc_open(out)
  vals <- ncdf4::ncvar_get(nc_out, "air_temperature")
  ncdf4::nc_close(nc_out)

  expect_equal(as.numeric(vals), c(280, 281, 282))
})
