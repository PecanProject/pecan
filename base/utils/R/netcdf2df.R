#' Read all variables from a netCDF into a single data frame
#'
#' Reads all dimensions and variables from a netCDF and returns them as
#'  a single data frame with one row per cell of the source file's
#'  dimension array.
#' Units are also read and are attached to the result as attribute "units"
#'
#' Written mostly for files where all dimensions are 1d vectors,
#'  e.g. single-site soil or met files.
#' Many files with more complex dimensions should work
#'  (at least for cases where it's clear how to rectangle them),
#'  but they are not yet well tested.
#'
#' @param path path to a netcdf file
#'
#' @return data frame with columns for each dim and var of the input file.
#'  Units for all of these are attached as an attribute.
#'
#' @author Chris Black
#' @export
#'
netcdf2df <- function(path) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  dim_vals <- nc$dim |>
    sapply(\(x) c(x[["vals"]]), simplify = FALSE) |>
    # Load-bearing assumption:
    # Dimension listed first in nc$dim is fastest-varying (as for expand.grid)
    # TODO verify whether this is reliably true.
    expand.grid()

  var_vals <- nc$var |>
    names() |>
    sapply(\(v) c(ncdf4::ncvar_get(nc, v)), simplify = FALSE)

  if (any(lengths(var_vals) != nrow(dim_vals))) {
    PEcAn.logger::logger.error("Not all variables have same length")
  }

  res <- cbind(dim_vals, as.data.frame(var_vals))
  attr(res, "units") <- c(
    sapply(nc$dim, `[[`, "units"),
    sapply(nc$var, `[[`, "units")
  )

  res
}
