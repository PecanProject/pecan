test_that(
  "metgapfill_with_fallback returns primary file when coverage is sufficient",
  {

    skip_if_not_installed("ncdf4")

    tmp <- tempdir()

    primary  <- file.path(tmp, "primary.nc")
    fallback <- file.path(tmp, "fallback.nc")
    out      <- file.path(tmp, "out.nc")

    # ---- define CF time dimension
    dim_time <- ncdf4::ncdim_def(
      name  = "time",
      units = "days since 2000-01-01",
      vals  = 0:2
    )

    var_tair <- ncdf4::ncvar_def(
      name     = "air_temperature",
      units    = "K",
      dim      = dim_time,
      missval  = NA_real_,
      prec     = "double"
    )

    # ---- primary file (full coverage, no NA)
    nc_p <- ncdf4::nc_create(primary, vars = var_tair)
    ncdf4::ncvar_put(nc_p, "air_temperature", c(280, 281, 282))
    ncdf4::ncatt_put(nc_p, "air_temperature", "missing_value", NA_real_)
    ncdf4::nc_close(nc_p)

    # ---- fallback file (dummy, should not be used)
    nc_f <- ncdf4::nc_create(fallback, vars = var_tair)
    ncdf4::ncvar_put(nc_f, "air_temperature", c(279, 279, 279))
    ncdf4::ncatt_put(nc_f, "air_temperature", "missing_value", NA_real_)
    ncdf4::nc_close(nc_f)

    # ---- run gap-fill logic
    result <- metgapfill_with_fallback(
      primary_cf   = primary,
      vars         = "air_temperature",
      fallback_cf  = fallback,
      out_file     = out
    )

    # Note: this checks for identical _paths_ and doesn't compare file contents
    expect_identical(result, out)
    expect_true(file.exists(out))

    nc_o <- ncdf4::nc_open(out)
    vals <- ncdf4::ncvar_get(nc_o, "air_temperature")
    ncdf4::nc_close(nc_o)

    expect_equal(as.numeric(vals), c(280, 281, 282))
  }
)

test_that(
  "metgapfill_with_fallback fills missing values when fallback is required",
  {
    skip_if_not_installed("ncdf4")

    tmp <- tempdir()

    primary  <- file.path(tmp, "primary_fill.nc")
    fallback <- file.path(tmp, "fallback_fill.nc")
    out      <- file.path(tmp, "out_fill.nc")

    # ---- define CF time dimension
    dim_time <- ncdf4::ncdim_def(
      name  = "time",
      units = "days since 2000-01-01",
      vals  = 0:2
    )

    var_tair <- ncdf4::ncvar_def(
      name     = "air_temperature",
      units    = "K",
      dim      = dim_time,
      missval  = NA_real_,
      prec     = "double"
    )

    # ---- primary file (contains NA → should be filled)
    nc_p <- ncdf4::nc_create(primary, vars = var_tair)
    ncdf4::ncvar_put(nc_p, "air_temperature", c(280, NA, 282))
    ncdf4::ncatt_put(nc_p, "air_temperature", "missing_value", NA_real_)
    ncdf4::nc_close(nc_p)

    # ---- fallback file (provides value for missing slot)
    nc_f <- ncdf4::nc_create(fallback, vars = var_tair)
    ncdf4::ncvar_put(nc_f, "air_temperature", c(279, 281, 279))
    ncdf4::ncatt_put(nc_f, "air_temperature", "missing_value", NA_real_)
    ncdf4::nc_close(nc_f)

    # ---- run gap-fill logic
    result <- metgapfill_with_fallback(
      primary_cf   = primary,
      vars         = "air_temperature",
      fallback_cf  = fallback,
      out_file     = out
    )

    # ---- verify output file created
    expect_identical(result, out)
    expect_true(file.exists(out))

    # ---- verify missing value filled correctly
    nc_o <- ncdf4::nc_open(out)
    vals <- ncdf4::ncvar_get(nc_o, "air_temperature")
    ncdf4::nc_close(nc_o)

    expect_equal(as.numeric(vals), c(280, 281, 282))
  }
)
