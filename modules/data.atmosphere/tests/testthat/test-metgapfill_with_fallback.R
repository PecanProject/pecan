context("metgapfill_with_fallback")

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
    ncdf4::nc_close(nc_p)

    # ---- fallback file (dummy, should not be used)
    nc_f <- ncdf4::nc_create(fallback, vars = var_tair)
    ncdf4::ncvar_put(nc_f, "air_temperature", c(279, 279, 279))
    ncdf4::nc_close(nc_f)

    # ---- run gap-fill logic
    result <- metgapfill_with_fallback(
      primary_cf   = primary,
      vars         = "air_temperature",
      fallback_cf  = fallback,
      out_file     = out
    )

    # ---- verify behavior
    expect_identical(result, primary)
    expect_false(file.exists(out))
  }
)
