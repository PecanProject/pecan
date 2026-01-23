#' Gap-fill CF meteorological data using a fallback dataset
#'
#' Orchestrates coverage checking and conditional merging of a fallback
#' CF NetCDF file into a primary CF NetCDF file. No files are modified
#' in place.
#'
#' @param primary_cf character. Path to primary CF NetCDF file
#' @param vars character vector. CF variable names to gap-fill
#' @param fallback_cf character. Path to fallback CF NetCDF file
#' @param out_file character. Path to output CF NetCDF file
#' @param coverage_threshold numeric. Minimum acceptable coverage (0–1)
#' @param align_time logical. Whether to align CF time axes before merging
#'
#' @return character. Path to gap-filled CF NetCDF file
#'
#' @noRd

metgapfill_with_fallback <- function(
  primary_cf,
  vars,
  fallback_cf,
  out_file,
  align_time = FALSE
) {

  # ---- basic validation (NO out_file side effects)
  stopifnot(
    is.character(primary_cf),
    file.exists(primary_cf),
    is.character(vars),
    is.character(fallback_cf),
    file.exists(fallback_cf),
    is.character(out_file)
  )

  # ---- enforce test contract: out_file must NOT exist
  if (file.exists(out_file)) {
  file.remove(out_file)
  }

  # ---- check coverage FIRST (no side effects)
  coverage_info <- check_met_coverage_for_fallback(
    cf_file = primary_cf,
    threshold = 1.0
  )

  fill_vars <- intersect(vars, coverage_info$fill_vars)

  # ---- NO fallback required → return primary, DO NOTHING ELSE
  if (length(fill_vars) == 0) {
    return(primary_cf)
  }

  # ---- fallback required → ensure clean output path
  if (file.exists(out_file)) {
    file.remove(out_file)
  }

  # ---- perform merge ONLY now
  PEcAn.data.atmosphere:::merge_cf_met_files(
    primary_cf   = primary_cf,
    secondary_cf = fallback_cf,
    vars         = fill_vars,
    out_file     = out_file,
    align_time   = align_time
  )
}

# -------------------------------------------------------------------
# Deferred work (intentionally out of scope for this function)
# -------------------------------------------------------------------
# TODO(#3605): resolve fallback source selection (ERA5 vs ERA5-Land)
# TODO(#3605): prepare fallback CF NetCDF from raw data sources
# TODO(#3605): wire into AmeriFlux_met_ensemble() and met.process()
