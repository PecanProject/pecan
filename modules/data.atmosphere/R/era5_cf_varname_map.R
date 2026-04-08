#' ERA5 CDS-to-CF variable name map
#'
#' Internal mapping between ERA5 CDS variable names and CF standard names
#' produced by extract.nc.ERA5().
#'
#' Only variables relevant to the AmeriFlux gap-fill fallback pipeline are
#' included. Variables not listed here are not handled by this pipeline.
#'
#' @noRd
era5_cds_to_cf_varnames <- c(
  "surface_solar_radiation_downwards" =
    "surface_downwelling_shortwave_flux_in_air",
  "volumetric_soil_water_layer_1" =
    "volume_fraction_of_condensed_water_in_soil"
)

#' Translate ERA5 CDS variable names to CF standard names
#'
#' @description
#' Translate ERA5 CDS variable names to CF standard names.
#'
#' @param cds_vars Character vector of ERA5 CDS API variable names.
#'
#' @return Named character vector the same length as `cds_vars`. Names are
#'   the input CDS names; values are the corresponding CF standard names as
#'   written by [extract.nc.ERA5()]. Any input name absent from the internal
#'   map produces a [PEcAn.logger::logger.warn()] and an `NA` at that
#'   position. Returns `character(0)` when `cds_vars` is empty.
#'
#' @details
#' Covers only the ERA5 variables used in the AmeriFlux gap-fill fallback
#' pipeline. Unknown names are warned and returned as `NA` rather than
#' silently dropped so that callers encounter the failure immediately instead
#' of producing a silent no-fill condition downstream.
#'
#' @seealso [cf_to_cds_varnames()]
#' @noRd
cds_to_cf_varnames <- function(cds_vars) {
  if (length(cds_vars) == 0L) {
    return(character(0))
  }

  result  <- era5_cds_to_cf_varnames[cds_vars]
  unknown <- cds_vars[is.na(result)]

  if (length(unknown) > 0L) {
    msg <- paste(
        "cds_to_cf_varnames: no CF mapping for CDS variable(s):",
        paste(unknown, collapse = ", "),
        "returning NA for those entries"
    )
    warning(msg, call. = FALSE)
    PEcAn.logger::logger.warn(msg)

  }

  result
}


#' Translate CF standard variable names to ERA5 CDS names
#'
#' @param cf_vars Character vector of CF standard variable names.
#'
#' @return Named character vector the same length as `cf_vars`. Names are
#'   the input CF names; values are the corresponding CDS API names. Any
#'   input name absent from the internal map produces a
#'   [PEcAn.logger::logger.warn()] and an `NA` at that position. Returns
#'   `character(0)` when `cf_vars` is empty.
#'
#' @details
#' Reverse of [cds_to_cf_varnames()]. Covers only the ERA5 variables used in
#' the AmeriFlux gap-fill fallback pipeline. Unknown names warn and return
#' `NA` rather than being silently dropped.
#'
#' @seealso [cds_to_cf_varnames()]
#' @noRd
cf_to_cds_varnames <- function(cf_vars) {
  if (length(cf_vars) == 0L) {
    return(character(0))
  }

  # Reverse map built at call time — one source of truth in
  # era5_cds_to_cf_varnames; no second hardcoded vector to keep in sync.
  reverse_map <- stats::setNames(
    names(era5_cds_to_cf_varnames),
    unname(era5_cds_to_cf_varnames)
  )

  result  <- reverse_map[cf_vars]
  unknown <- cf_vars[is.na(result)]

  if (length(unknown) > 0L) {
    msg <- paste(
        "cf_to_cds_varnames: no CDS mapping for CF variable(s):",
        paste(unknown, collapse = ", "),
        "- returning NA for those entries"
    )
    warning(msg, call. = FALSE)
    PEcAn.logger::logger.warn(msg)
  }

  result
}