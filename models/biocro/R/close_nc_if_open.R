# close file if open, but don't complain if it's already closed.
# Used by met2model.BIOCRO to allow closing files cleanly on exit
# without keeping them open longer than needed while looping over other years
close_nc_if_open <- function(ncfile) {

  if (!inherits(ncfile, "ncdf4")) {
    PEcAn.logger::logger.error(
      substitute(ncfile),
      " is not an NCDF file object. Don't know how to close it.")
    return(invisible())
  }

  # Key assumption here: as long as it's an ncdf4 object to start with,
  # "invalid ID" means the file has been closed already...
  # or at least that we can't do anything else about it
  already_closed_msg <- "Error in R_nc4_close: NetCDF: Not a valid ID"

  res <- utils::capture.output(ncdf4::nc_close(ncfile))
  if (length(res) == 0 || res == already_closed_msg) {
    # OK, it's closed now
    return(invisible())
  } else {
    PEcAn.logger::logger.error(
      "Closing NCDF file", ncfile$filename, "failed with message", res)
  }
}
