#' Check AmeriFlux CF coverage for ERA5 fallback
#'
#' Determines whether radiation and/or soil moisture require ERA5 fallback
#' based on variable presence and data coverage.
#'
#' @param cf_file character. Path to CF-compliant NetCDF file
#' @param threshold numeric. Coverage threshold (0–1) for radiation fallback
#' @param verbose logical. Emit logger messages
#'
#' @return list with:
#' \itemize{
#'   \item fill_vars: ERA5 variables to request
#'   \item coverage: named list of coverage fractions
#' }
#'
#' @noRd
check_met_coverage_for_fallback <- function(cf_file,
                                            threshold = 0.5,
                                            verbose = FALSE) {

  if (verbose) {
    PEcAn.logger::logger.info("Checking data coverage for ERA5 fallback")
  }

  nc <- ncdf4::nc_open(cf_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  # ---- helper: compute coverage respecting CF missing attributes
  detect_coverage_fraction <- function(nc, varname) {

    vals <- ncdf4::ncvar_get(nc, varname)

    # get _FillValue or missing_value if present
    get_fill_attr <- function(nc, varname) {
      a <- ncdf4::ncatt_get(nc, varname, "_FillValue")
      if (!is.null(a) && isTRUE(a$hasatt) && !is.na(a$value)) return(a$value)

      a2 <- ncdf4::ncatt_get(nc, varname, "missing_value")
      if (!is.null(a2) && isTRUE(a2$hasatt) && !is.na(a2$value)) return(a2$value)

      return(NULL)
    }

    fill_val <- get_fill_attr(nc, varname)

    is_missing <- rep(FALSE, length(vals))

    if (!is.null(fill_val)) {
      is_missing <- is_missing | (!is.na(vals) & vals == fill_val)
    }

    is_missing <- is_missing | is.na(vals) | is.nan(vals)

    n <- length(vals)
    if (n == 0) return(0)
    return(sum(!is_missing) / n)
  }

  # ---- Radiation (Rg)
  has_rg <- "surface_downwelling_shortwave_flux_in_air" %in% names(nc$var)
  rg_coverage <- 0
  if (has_rg) {
    rg_coverage <- detect_coverage_fraction(
      nc,
      "surface_downwelling_shortwave_flux_in_air"
  )
  }

  # ---- PAR
  has_par <- "surface_downwelling_photosynthetic_photon_flux_in_air" %in% names(nc$var)
  par_coverage <- 0
  if (has_par) {
    par_coverage <- detect_coverage_fraction(
      nc,
      "surface_downwelling_photosynthetic_photon_flux_in_air"
    )
  }

  # ---- Soil moisture
  has_swc <- "volume_fraction_of_condensed_water_in_soil" %in% names(nc$var)
  swc_coverage <- 0
  if (has_swc) {
    swc_coverage <- detect_coverage_fraction(
      nc,
      "volume_fraction_of_condensed_water_in_soil"
    )
  }

  if (verbose) {
    PEcAn.logger::logger.info(paste("Rg coverage:", round(rg_coverage * 100, 1), "%"))
    PEcAn.logger::logger.info(paste("PAR coverage:", round(par_coverage * 100, 1), "%"))
    PEcAn.logger::logger.info(paste("Soil moisture coverage:", round(swc_coverage * 100, 1), "%"))
  }

  fill_vars <- character(0)

  if ((!has_rg || rg_coverage < threshold) &&
      (!has_par || par_coverage < threshold)) {
    fill_vars <- c(fill_vars, "surface_solar_radiation_downwards")
  }

  if (has_swc && swc_coverage < 1.0) {
    fill_vars <- c(fill_vars, "volumetric_soil_water_layer_1")
  }

  list(
    fill_vars = fill_vars,
    coverage = list(
      rg = rg_coverage,
      par = par_coverage,
      swc = swc_coverage
    )
  )
}
