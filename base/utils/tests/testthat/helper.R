# Used by test-read.output
times_to_netcdf <- function(times, timeunit, outdir, filename) {
  dims <- list(time = ncdf4::ncdim_def(
    name = "time",
    units = timeunit,
    vals = times))
  vars <- list(Y = ncdf4::ncvar_def(
    name = "Y",
    units  = "kg",
    dim = dims,
    missval = NA))
  nc <- ncdf4::nc_create(
    filename = file.path(outdir, filename),
    vars = vars)
  ncdf4::ncatt_put(nc, 0, "description", "strictly for testing")
  ncdf4::ncvar_put(nc, varid = vars[["Y"]], vals = rnorm(length(times)))
  ncdf4::nc_close(nc)
}

#' Create an example NetCDF file for use in test-read.output
#'
#' Target file contains one year's worth of daily data. The data are
#' random normal draws ([stats::rnorm()]) with the specified variable
#' names. The dimensions are "time" with units as "days since
#' 2001-01-01". There is one variable per item in `varnames`.
#' @param varnames Character vector of variable names.
#' @param file_path Full path to target NetCDF file.
#' @return `file_path`, invisibly
#' @author Alexey Shiklomanov
example_netcdf <- function(varnames, file_path) {
  n <- 365
  dims <- list(x = ncdf4::ncdim_def(
    name = "time",
    units = "days since 2001-01-01",
    vals = seq_len(n) - 1
  ))
  vars <- lapply(varnames, ncdf4::ncvar_def,
                 units = "kg", dim = dims, missval = NA)
  names(vars) <- varnames
  nc <- ncdf4::nc_create(filename = file_path, vars = vars)
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncatt_put(nc, 0, "description", "strictly for testing")
  for (v in varnames) {
    ncdf4::ncvar_put(nc, varid = vars[[v]], vals = rnorm(n))
  }
  invisible(file_path)
}
