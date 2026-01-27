#' Merge CF-compliant meteorological NetCDF files
#'
#' Creates a new CF-compliant NetCDF file by merging selected variables
#' from a secondary CF NetCDF file into a primary CF NetCDF file.
#' Input files are never modified.
#'
#' @param primary_cf character. Path to primary CF NetCDF file
#' @param secondary_cf character. Path to secondary CF NetCDF file
#' @param vars character vector. CF variable names to merge
#' @param out_file character. Path to output NetCDF file
#' @param align_time logical. Whether to align time axes before merging
#'
#' @return character. Path to the newly created NetCDF file
#'
#' @noRd

  merge_cf_met_files <- function(
    primary_cf,
    secondary_cf,
    vars,
    out_file,
    align_time = FALSE
  ) {

  # TODO(#3605): align CF time axes using PEcAn.utils::cf2datetime()
  # TODO(#3605): error on non-overlapping time axes
  # TODO(#3605): consider aggregation/repeat logic in future PR

  # ---- open inputs (read-only)
  nc_primary <- ncdf4::nc_open(primary_cf)
  on.exit(ncdf4::nc_close(nc_primary), add = TRUE)

  nc_secondary <- ncdf4::nc_open(secondary_cf)
  on.exit(ncdf4::nc_close(nc_secondary), add = TRUE)

    # ---- extract and align CF time axes (minimal, non-resampling)
  if (align_time) {

    # ---- dependency guard (CI-safe)
    if (!requireNamespace("PEcAn.utils", quietly = TRUE)) {
      stop(
        "align_time = TRUE requires the PEcAn.utils package, which is not installed"
      )
    }

    # ---- validate CF time variable existence
    if (!("time" %in% names(nc_primary$var))) {
      stop("Primary CF file does not contain a 'time' variable")
    }

    if (!("time" %in% names(nc_secondary$var))) {
      stop("Secondary CF file does not contain a 'time' variable")
    }

    # Extract CF time values + units
    primary_time_vals <- ncdf4::ncvar_get(nc_primary, "time")
    primary_time_unit <- ncdf4::ncatt_get(nc_primary, "time", "units")$value

    secondary_time_vals <- ncdf4::ncvar_get(nc_secondary, "time")
    secondary_time_unit <- ncdf4::ncatt_get(nc_secondary, "time", "units")$value

    # Convert to POSIXct using PEcAn.utils helper
    primary_time <- PEcAn.utils::cf2datetime(
      primary_time_vals,
      primary_time_unit
    )

    secondary_time <- PEcAn.utils::cf2datetime(
      secondary_time_vals,
      secondary_time_unit
    )

    # Find overlapping timestamps
    common_time <- intersect(primary_time, secondary_time)

    if (length(common_time) == 0) {
      stop("No overlapping CF time values between primary_cf and secondary_cf")
    }

    # Indices for subsetting variables
    primary_idx   <- match(common_time, primary_time)
    secondary_idx <- match(common_time, secondary_time)
  }


  # TODO (#3605): Replace copy-and-modify approach with explicit
  # CF-safe NetCDF construction. Output file is currently created by
  # copying the primary file and updating selected variables.
  # ---- create output file by copying base file
  file.copy(primary_cf, out_file, overwrite = TRUE)
  nc_out <- ncdf4::nc_open(out_file, write = TRUE)

  on.exit(ncdf4::nc_close(nc_out), add = TRUE)

  # ---- loop over variables to merge
  for (v in vars) {

    if (!(v %in% names(nc_primary$var)) || !(v %in% names(nc_secondary$var))) {
      next
    }

    primary_vals   <- ncdf4::ncvar_get(nc_primary, v)
    secondary_vals <- ncdf4::ncvar_get(nc_secondary, v)

    if (align_time) {
      primary_vals   <- primary_vals[primary_idx, drop = FALSE]
      secondary_vals <- secondary_vals[secondary_idx, drop = FALSE]
    }

    # only replace NA values
    replace_idx <- is.na(primary_vals) & !is.na(secondary_vals)

    if (any(replace_idx)) {
      primary_vals[replace_idx] <- secondary_vals[replace_idx]
      ncdf4::ncvar_put(nc_out, v, primary_vals)
    }
  }

  out_file
}
