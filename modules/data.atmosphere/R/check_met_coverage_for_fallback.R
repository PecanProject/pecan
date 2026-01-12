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

  time_vals <- ncdf4::ncvar_get(nc, "time")
  n_total <- length(time_vals)

  # ---- Radiation (Rg)
  has_rg <- "surface_downwelling_shortwave_flux_in_air" %in% names(nc$var)
  rg_coverage <- 0
  if (has_rg) {
    rg_data <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
    rg_coverage <- sum(!is.na(rg_data)) / n_total
  }

  # ---- PAR
  has_par <- "surface_downwelling_photosynthetic_photon_flux_in_air" %in% names(nc$var)
  par_coverage <- 0
  if (has_par) {
    par_data <- ncdf4::ncvar_get(nc, "surface_downwelling_photosynthetic_photon_flux_in_air")
    par_coverage <- sum(!is.na(par_data)) / n_total
  }

  # ---- Soil moisture
  has_swc <- "volume_fraction_of_condensed_water_in_soil" %in% names(nc$var)
  swc_coverage <- 0
  if (has_swc) {
    swc_data <- ncdf4::ncvar_get(nc, "volume_fraction_of_condensed_water_in_soil")
    swc_coverage <- sum(!is.na(swc_data)) / n_total
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
