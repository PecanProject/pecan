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
