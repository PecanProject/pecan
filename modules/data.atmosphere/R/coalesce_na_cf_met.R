#' Coalesce missing values in CF meteorological NetCDF files
#'
#' Creates a new CF-compliant NetCDF file by coalescing missing values in the
#' primary CF NetCDF file with corresponding values from the secondary CF NetCDF
#' file. Matching is determined by variable name and timestamp. Values in the
#' primary file are preserved unless they are missing (NA).
#'
#' Input files are never modified.
#'
#' @param primary_cf character. Path to the base CF NetCDF file whose missing
#'   values may be filled.
#' @param secondary_cf character. Path to the CF NetCDF file providing fallback
#'   values used only where the primary file contains missing values.
#' @param vars character vector. CF variable names to coalesce (must exist in
#'   both files with compatible units).
#' @param out_file character. Path to the output NetCDF file.
#' @param align_time logical. If TRUE, restricts both files to overlapping
#'   timestamps before filling (no resampling or interpolation is performed).
#'
#' @return character. Path to the newly created NetCDF file.
#'
#' NOTE: This helper assumes CF-compliant meteorological NetCDF files
#' produced by PEcAn workflows. Behavior with arbitrary NetCDF files,
#' differing temporal resolution, or non-time dimensions is undefined.
#' @noRd

coalesce_na_cf_met <- function(
  primary_cf,
  secondary_cf,
  vars,
  out_file,
  align_time = FALSE
) {

  # TODO(#3605): align CF time axes using PEcAn.utils::cf2datetime()
  # TODO(#3605): error on non-overlapping time axes
  # TODO(#3605): consider aggregation/repeat logic in future PR
  # NOTE (#3605): Current implementation assumes:
  # - Both files share identical temporal resolution
  # - Only overlapping timestamps are used when align_time = TRUE
  # - Variables are one-dimensional along time only
  # Handling differing timesteps, temporal extension, or multidimensional
  # variables is intentionally out of scope for this helper and may be
  # considered in future refactoring.

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
  if (!file.copy(primary_cf, out_file, overwrite = TRUE)) {
    stop("Failed to copy primary_cf to out_file")
  }
  nc_out <- ncdf4::nc_open(out_file, write = TRUE)

  on.exit(ncdf4::nc_close(nc_out), add = TRUE)

  # --- helper: get numeric fill attribute if present
  get_fill_attr <- function(nc, varname) {
    a <- ncdf4::ncatt_get(nc, varname, "_FillValue")
    if (!is.null(a) && !identical(a, list()) && !is.na(a$value)) return(a$value)

    a2 <- ncdf4::ncatt_get(nc, varname, "missing_value")
    if (!is.null(a2) && !identical(a2, list()) && !is.na(a2$value)) return(a2$value)
    
    return(NULL)
  }

  # ---- loop over variables to coalesce missing values
  for (v in vars) {

    if (!(v %in% names(nc_primary$var)) || !(v %in% names(nc_secondary$var))) {
      next
    }

   # read variable arrays from inputs
    primary_vals   <- ncdf4::ncvar_get(nc_primary, v)
    secondary_vals <- ncdf4::ncvar_get(nc_secondary, v)

    fill_primary_attr   <- get_fill_attr(nc_primary, v)
    fill_secondary_attr <- get_fill_attr(nc_secondary, v)

    is_missing_primary <- rep(FALSE, length(primary_vals))
    if (!is.null(fill_primary_attr)) {

      eq_fill <- (!is.na(primary_vals) & primary_vals == fill_primary_attr)
      is_missing_primary <- is_missing_primary | eq_fill
    }

    is_missing_primary <- is_missing_primary | is.na(primary_vals) | is.nan(primary_vals)

    is_valid_secondary <- rep(TRUE, length(secondary_vals))
    if (!is.null(fill_secondary_attr)) {
      is_valid_secondary <- (!is.na(secondary_vals) & secondary_vals != fill_secondary_attr)
    }

    is_valid_secondary <- is_valid_secondary & !is.na(secondary_vals) & !is.nan(secondary_vals)

    if (align_time) {

      overlap_primary   <- primary_vals[primary_idx]
      overlap_secondary <- secondary_vals[secondary_idx]


      is_missing_overlap_primary <- rep(FALSE, length(overlap_primary))
      if (!is.null(fill_primary_attr)) {
        eq_fill_o <- (!is.na(overlap_primary) & overlap_primary == fill_primary_attr)
        is_missing_overlap_primary <- is_missing_overlap_primary | eq_fill_o
      }
      is_missing_overlap_primary <- is_missing_overlap_primary | is.na(overlap_primary) | is.nan(overlap_primary)

      is_valid_overlap_secondary <- rep(TRUE, length(overlap_secondary))
      if (!is.null(fill_secondary_attr)) {
        is_valid_overlap_secondary <- (!is.na(overlap_secondary) & overlap_secondary != fill_secondary_attr)
      }
      is_valid_overlap_secondary <- is_valid_overlap_secondary & !is.na(overlap_secondary) & !is.nan(overlap_secondary)

      replace_idx <- is_missing_overlap_primary & is_valid_overlap_secondary

      if (any(replace_idx)) {
        overlap_primary[replace_idx] <- overlap_secondary[replace_idx]
        primary_vals[primary_idx] <- overlap_primary
      }

    } else {

      replace_idx <- is_missing_primary & is_valid_secondary

      if (any(replace_idx)) {
        primary_vals[replace_idx] <- secondary_vals[replace_idx]
      }
    }

    if (!is.null(fill_primary_attr) && !is.na(fill_primary_attr)) {
      ncdf4::ncatt_put(nc_out, v, "_FillValue", fill_primary_attr)
    }

    ncdf4::ncvar_put(nc_out, v, primary_vals)
  }

  out_file
}
