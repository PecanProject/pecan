#' Coalesce missing values in CF meteorological NetCDF files
#'
#' Creates a new CF-compliant NetCDF file by coalescing missing values in the
#' primary CF NetCDF file with corresponding values from a secondary CF
#' NetCDF file.
#'
#' Values in the primary file are preserved unless they are missing
#' (NA, NaN, or equal to a declared _FillValue/missing_value attribute).
#' Missing values are replaced with valid values from the secondary file.
#'
#' Matching behavior:
#' - If `align_time = FALSE` (default), variables are matched strictly
#'   by array index. Dimensions must be identical or an error is thrown.
#' - If `align_time = TRUE`, time axes are converted to POSIXct using
#'   `PEcAn.utils::cf2datetime()` and only overlapping timestamps are used.
#'   No interpolation or resampling is performed.
#'
#' Input files are never modified. The output file is created by copying
#' the primary file and updating selected variables.
#'
#' @param primary_cf character. Path to the base CF NetCDF file whose missing
#'   values may be filled.
#' @param secondary_cf character. Path to the CF NetCDF file providing fallback
#'   values used only where the primary file contains missing values.
#' @param vars character vector. CF variable names to coalesce (must exist in
#'   both files).
#' @param out_file character. Path to the output NetCDF file.
#' @param align_time logical. If TRUE, restricts filling to overlapping
#'   timestamps based on CF time metadata. Requires the `PEcAn.utils` package.
#'
#' @return character. Path to the newly created NetCDF file.
#'
#' @details
#' This helper assumes CF-compliant meteorological NetCDF files produced
#' by PEcAn workflows. Behavior with arbitrary NetCDF files, differing
#' temporal resolution, or multidimensional time structures is undefined.
#'
#' @noRd

coalesce_na_cf_met <- function(
  primary_cf,
  secondary_cf,
  vars,
  out_file,
  align_time = FALSE
) {
  # NOTE (#3605):
  # Current implementation:
  # - Assumes identical temporal resolution unless align_time = TRUE
  # - Uses cf2datetime() for alignment when requested
  # - Performs strict dimension validation when align_time = FALSE
  #

  # ---- open inputs (read-only)
  nc_primary <- ncdf4::nc_open(primary_cf)
  on.exit(ncdf4::nc_close(nc_primary), add = TRUE)

  nc_secondary <- ncdf4::nc_open(secondary_cf)
  on.exit(ncdf4::nc_close(nc_secondary), add = TRUE)

  # ---- optional time alignment
  if (align_time) {
    if (!requireNamespace("PEcAn.utils", quietly = TRUE)) {
      stop("align_time = TRUE requires the PEcAn.utils package")
    }

    if (!("time" %in% names(nc_primary$var))) {
      stop("Primary CF file does not contain a 'time' variable")
    }

    if (!("time" %in% names(nc_secondary$var))) {
      stop("Secondary CF file does not contain a 'time' variable")
    }

    primary_time_vals <- ncdf4::ncvar_get(nc_primary, "time")
    primary_time_unit <- ncdf4::ncatt_get(nc_primary, "time", "units")$value
    secondary_time_vals <- ncdf4::ncvar_get(nc_secondary, "time")
    secondary_time_unit <- ncdf4::ncatt_get(nc_secondary, "time", "units")$value

    primary_time <- PEcAn.utils::cf2datetime(primary_time_vals, primary_time_unit)
    secondary_time <- PEcAn.utils::cf2datetime(secondary_time_vals, secondary_time_unit)

    common_time <- intersect(primary_time, secondary_time)

    if (length(common_time) == 0) {
      stop("No overlapping CF time values between primary_cf and secondary_cf")
    }

    primary_idx <- match(common_time, primary_time)
    secondary_idx <- match(common_time, secondary_time)
  }

  # ---- copy primary file to output
  if (!file.copy(primary_cf, out_file, overwrite = TRUE)) {
    stop("Failed to copy primary_cf to out_file")
  }

  nc_out <- ncdf4::nc_open(out_file, write = TRUE)
  on.exit(ncdf4::nc_close(nc_out), add = TRUE)

  # ---- helper to extract fill attributes
  get_fill_attr <- function(nc, varname) {
    a <- ncdf4::ncatt_get(nc, varname, "_FillValue")
    if (!is.null(a$value) && !is.na(a$value)) {
      return(a$value)
    }

    a2 <- ncdf4::ncatt_get(nc, varname, "missing_value")
    if (!is.null(a2$value) && !is.na(a2$value)) {
      return(a2$value)
    }

    return(NULL)
  }

  # ---- loop over variables
  for (v in vars) {
    if (!(v %in% names(nc_primary$var)) ||
      !(v %in% names(nc_secondary$var))) {
      next
    }

    primary_vals <- ncdf4::ncvar_get(nc_primary, v)
    secondary_vals <- ncdf4::ncvar_get(nc_secondary, v)

    # ---- strict dimension validation when not aligning time
    if (!align_time) {
      if (!identical(dim(primary_vals), dim(secondary_vals))) {
        stop(
          sprintf(
            "Dimension mismatch for variable '%s': primary dim = %s, secondary dim = %s",
            v,
            paste(dim(primary_vals), collapse = "x"),
            paste(dim(secondary_vals), collapse = "x")
          )
        )
      }
    }

    fill_primary_attr <- get_fill_attr(nc_primary, v)
    fill_secondary_attr <- get_fill_attr(nc_secondary, v)

    # ---- array-safe logical masks
    is_missing_primary <- array(FALSE, dim(primary_vals))

    if (!is.null(fill_primary_attr)) {
      is_missing_primary <-
        is_missing_primary |
          (!is.na(primary_vals) & primary_vals == fill_primary_attr)
    }

    is_missing_primary <-
      is_missing_primary |
        is.na(primary_vals) |
        is.nan(primary_vals)

    is_valid_secondary <- array(TRUE, dim(secondary_vals))

    if (!is.null(fill_secondary_attr)) {
      is_valid_secondary <-
        (!is.na(secondary_vals) &
          secondary_vals != fill_secondary_attr)
    }

    is_valid_secondary <-
      is_valid_secondary &
        !is.na(secondary_vals) &
        !is.nan(secondary_vals)

    # ---- coalescing logic
    if (align_time) {
      overlap_primary <- primary_vals[primary_idx]
      overlap_secondary <- secondary_vals[secondary_idx]

      is_missing_overlap <-
        is.na(overlap_primary) | is.nan(overlap_primary)

      if (!is.null(fill_primary_attr)) {
        is_missing_overlap <-
          is_missing_overlap |
            overlap_primary == fill_primary_attr
      }

      is_valid_overlap <-
        !is.na(overlap_secondary) & !is.nan(overlap_secondary)

      if (!is.null(fill_secondary_attr)) {
        is_valid_overlap <-
          is_valid_overlap &
            overlap_secondary != fill_secondary_attr
      }

      replace_idx <- is_missing_overlap & is_valid_overlap

      if (any(replace_idx)) {
        overlap_primary[replace_idx] <-
          overlap_secondary[replace_idx]
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
