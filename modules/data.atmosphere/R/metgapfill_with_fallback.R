#' Gap-fill CF meteorological data using a fallback dataset
#'
#' Coalesces missing values in a primary CF NetCDF file using values
#' from a fallback CF NetCDF file.
#'
#' This function assumes:
#' - Coverage decisions have already been made upstream.
#' - `vars` contains only variables that require fallback.
#' - `fallback_cf` has already been prepared (if needed).
#'
#' This function:
#' - Never modifies input files in place.
#' - Copies the primary file unchanged if no fallback is required.
#'
#' @param primary_cf character. Path to primary CF NetCDF file.
#' @param vars character vector. CF variable names to gap-fill.
#' @param fallback_cf character or NULL. Path to fallback CF NetCDF file.
#' @param out_file character. Path to output CF NetCDF file.
#' @param align_time logical. If TRUE, restricts both CF files to overlapping
#'   timestamps before filling. No interpolation or resampling is performed.
#'
#' @return character. Path to gap-filled CF NetCDF file.
#'
#' @noRd

metgapfill_with_fallback <- function(
  primary_cf,
  vars,
  fallback_cf = NULL,
  out_file,
  align_time = FALSE
) {
  # Basic validation
  stopifnot(
    is.character(primary_cf),
    file.exists(primary_cf),
    is.character(out_file)
  )

  if (is.null(vars)) {
    vars <- character(0)
  }

  if (!is.character(vars)) {
    stop("vars must be a character vector")
  }

  # No fallback required
  if (length(vars) == 0 || is.null(fallback_cf)) {
    if (file.exists(out_file)) {
      file.remove(out_file)
    }

    if (!file.copy(primary_cf, out_file, overwrite = TRUE)) {
      stop("Failed to copy primary_cf to out_file")
    }

    return(out_file)
  }

  # Validate fallback file
  if (!file.exists(fallback_cf)) {
    stop("fallback_cf does not exist")
  }

  # Ensure clean output path
  if (file.exists(out_file)) {
    file.remove(out_file)
  }

  # Perform CF-level coalescing
  coalesce_na_cf_met(
    primary_cf   = primary_cf,
    secondary_cf = fallback_cf,
    vars         = vars,
    out_file     = out_file,
    align_time   = align_time
  )
}
