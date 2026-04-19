#' Translate meteorological variable names between naming conventions
#'
#' Looks up variable names in one column of the PEcAn standard meteorology
#' table and returns the corresponding names from another column.
#'
#' @param vars Character vector of variable names to translate.
#' @param from Character scalar. Column in \code{table} containing the
#'   input naming convention.
#' @param to Character scalar. Column in \code{table} containing the
#'   output naming convention.
#' @param table Data frame used as the lookup source. Defaults to
#'   \code{pecan_standard_met_table}.
#'
#' @return Named character vector of translated variable names. Names are
#'   the input values; elements are the mapped values. Variables not found
#'   in \code{table[[from]]} return \code{NA} and emit a log warning.
#'   Returns \code{character(0)} for empty input.
#'
#' @details
#' Rows where \code{table[[from]]} is \code{NA} or empty string are excluded
#' from the lookup. Translation is not guaranteed to be one-to-one; ensure
#' uniqueness in \code{table[[to]]} if round-trip mapping is required.
#'
#' @examples
#' # ERA5 CDS name to CF standard name
#' translate_met_varnames(
#'   "surface_solar_radiation_downwards",
#'   from = "era5_cds",
#'   to   = "cf_standard_name"
#' )
#'
#' # CF standard name to ERA5 CDS name
#' translate_met_varnames(
#'   "surface_downwelling_shortwave_flux_in_air",
#'   from = "cf_standard_name",
#'   to   = "era5_cds"
#' )
#'
#' @export
translate_met_varnames <- function(vars,
                                   from,
                                   to,
                                   table = pecan_standard_met_table) {
  if (length(vars) == 0L) return(character(0))

  lookup <- table |>
    dplyr::select(dplyr::all_of(c(from, to))) |>
    dplyr::filter(!is.na(.data[[from]]), .data[[from]] != "")

  result        <- lookup[[to]]
  names(result) <- lookup[[from]]

  translated <- result[vars]

  unknown <- vars[is.na(translated)]
  if (length(unknown) > 0L) {
    PEcAn.logger::logger.warn(
      "translate_met_varnames: no", to, "mapping for", from,
      "variable(s):", paste(unknown, collapse = ", "),
      "-- returning NA for those entries"
    )
  }

  translated
}


#' Translate ERA5 CDS API names to CF standard names
#'
#' @param cds_vars Character vector of ERA5 CDS API variable names.
#' @return Named character vector of CF standard names.
#' @noRd
cds_to_cf_varnames <- function(cds_vars) {
  translate_met_varnames(cds_vars,
                         from = "era5_cds",
                         to   = "cf_standard_name")
}


#' Translate CF standard names to ERA5 CDS API names
#'
#' @param cf_vars Character vector of CF standard names.
#' @return Named character vector of ERA5 CDS variable names.
#' @noRd
cf_to_cds_varnames <- function(cf_vars) {
  translate_met_varnames(cf_vars,
                         from = "cf_standard_name",
                         to   = "era5_cds")
}