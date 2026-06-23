#' Read soil parameters from a PEcAn soil physics file
#'
#' Reads values from a netCDF that follows the PEcAn soil standard,
#'   aggregates layers across the depth to be simulated,
#'   and converts to names and units expected by RothC.
#'
#' TODO: Currently drops all of any layer that extends below `model_depth`.
#'   It would be better instead to weight all layers by their contribution
#'   to the requested depth.
#'
#' @param path filepath to a netcdf,
#'   probably read from `settings$run$inputs$soil_physics$path`
#' @param model_depth Soil depth to be simulated, in cm
#'
#' @return one-row dataframe with columns
#'   `depth_cm`, `clay_pct`, `silt_pct`,
#'   `bulkdens_g_cm3`, `org_C_pct`, `iom_tC_ha`
#'
#' @importFrom rlang .data .env
#'
read_soil_physics <- function(path, model_depth = 23) {
  soil_vals <- netcdf2df(path)
  soil_units <- as.list(attr(soil_vals, "units"))

  # Input is documented to be in meters,
  # but providing cm instead seems to be a _very_ common error;
  # might as well try to handle it here
  if (tolower(soil_units$depth) %in% c("m", "meters")
      && any(soil_vals$depth >= 10)) {
    PEcAn.logger::logger.warn(
      "Soil depths reported to be in meters, but found values >= 10",
      "in file", path,
      "Assuming the units have been mislabeled and treating all depths as cm."
    )
    soil_units$depth <- "cm"
  }

  soil_vals |>
    dplyr::mutate(
      depth_cm = .data$depth |>
        PEcAn.utils::ud_convert(soil_units$depth, "cm")
    ) |>
    # TODO 1: Assumes depth is given to bottom of layer -- is that correct?
    # TODO 2: this drops layers that extend past bottom
    # (eg with depth=23 and 0-10/10-30 layering, would use only 0-10)
    # Consider rescaling partial layers
    # (Or throwing an error on mismatch and making everyone generate their soil
    #  files with layers that match model depth?)
    dplyr::filter(.data$depth_cm <= model_depth) |>
    dplyr::summarize(
      # TODO consider weighting by layer thickness?
      depth_cm = max(.data$depth_cm),
      clay_pct = .data$fraction_of_clay_in_soil |>
        mean() |>
        PEcAn.utils::ud_convert(soil_units$fraction_of_clay_in_soil, "%"),
      silt_pct = .data$fraction_of_silt_in_soil |>
        mean() |>
        PEcAn.utils::ud_convert(soil_units$fraction_of_silt_in_soil, "%"),
      bulkdens_g_cm3 = .data$soil_bulk_density |>
        mean() |>
        PEcAn.utils::ud_convert(soil_units$soil_bulk_density, "g cm-3"),
      org_C_pct = .data$soil_organic_carbon_stock |>
        sum() |>
        PEcAn.utils::ud_convert(
          soil_units$soil_organic_carbon_stock,
          "g cm-2"
        ) |>
        (\(x) x / (.data$depth_cm * .data$bulkdens_g_cm3))() |>
        PEcAn.utils::ud_convert("1", "%"),
      iom_tC_ha = .data$soil_organic_carbon_stock |>
        sum() |>
        PEcAn.utils::ud_convert(
          soil_units$soil_organic_carbon_stock,
          "t ha-1"
        ) |>
        # Approximation from Falloon et al. 1998, 10.1016/S0038-0717(97)00256-3
        (\(x) 0.049 * x^1.139)()
    )
}

# an internal wrapper to allow stubbing the function out under test
# without affecting code outside the package.
netcdf2df <- PEcAn.utils::netcdf2df
