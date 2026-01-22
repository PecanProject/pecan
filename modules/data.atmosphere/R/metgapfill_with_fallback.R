#' Gap-fill meteorological CF NetCDF using fallback sources
#'
#' High-level helper to gap-fill meteorological drivers using
#' one or more fallback datasets (e.g. ERA5, ERA5-Land).
#'
#' This function orchestrates coverage checking, fallback selection,
#' and CF-safe merging, but delegates implementation details to
#' lower-level helpers.
#'
#' @param cf_file character. Path to primary CF NetCDF file
#' @param vars character vector. Meteorological variables to gap-fill
#' @param fallback character vector. Fallback sources (e.g. "ERA5", "ERA5-Land")
#' @param out_file character. Path to output CF NetCDF file
#'
#' @return character. Path to the newly created CF NetCDF file
#'
#' @noRd
metgapfill_with_fallback <- function(
  cf_file,
  vars,
  fallback,
  out_file
) {

  # TODO(#3605): detect temporal coverage gaps
  #   - use check_met_coverage_for_fallback()
  #   - decide whether fallback is required

  # TODO(#3605): resolve fallback source(s)
  #   - ERA5 vs ERA5-Land
  #   - priority order

  # TODO(#3605): prepare fallback CF NetCDF
  #   - reuse existing ERA5 helpers
  #   - ensure CF compliance

  # TODO(#3605): merge fallback into primary
  #   - call merge_cf_met_files()

  cli::cli_abort(
    "metgapfill_with_fallback() is a design skeleton and not yet implemented"
  )
}
