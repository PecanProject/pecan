#' Generate sea-level rise and tidal inundation scenarios
#'
#' Queries one or more NOAA tide gauges and generates annual sea-level and
#' tidal inundation scenarios for each gauge.
#'
#' @param station_id Vector of NOAA station identifiers.
#' @param run_hindcast Logical. Generate historical sea-level data.
#' @param run_forecast Logical. Generate forecast sea-level scenarios.
#' @param hindcast_start First year of hindcast.
#' @param forecast_start First year of forecast.
#' @param forecast_end Final forecast year.
#' @param scenario Character vector of AR6 scenarios.
#' @param confidence_level Character vector of confidence levels.
#' @param target_quantile Numeric vector of target quantiles.
#' @param include_lt_tidal_const Include long-term tidal constituents.
#' @param datum_start_year First year used to calculate tidal datums.
#' @param datum_end_year Last year used to calculate tidal datums.
#'
#' @return A list containing:
#' \itemize{
#'   \item scenario_curves: Combined scenario table for all gauges.
#'   \item tidal_datums_summarized: Combined tidal datum table for all gauges.
#' }
#'
#' @export
generateFullTidalScenario <- function(
    station_id = 9410660,
    run_hindcast = TRUE,
    run_forecast = TRUE,
    hindcast_start = 1928,
    forecast_start = 2026,
    forecast_end = 2100,
    scenario = c("ssp126", "ssp245"),
    confidence_level = "medium",
    target_quantile = c(0.25, 0.5, 0.75),
    include_lt_tidal_const = TRUE,
    datum_start_year = 1980,
    datum_end_year = 2025
) {
  
  # Ensure station IDs are unique
  station_id <- unique(station_id)
  
  results <- purrr::map(
    station_id,
    function(gauge) {
      
      message("Generating tidal scenarios for gauge: ", gauge)
      
      .generateFullTidalScenario_one_gauge(
        station_id = gauge,
        run_hindcast = run_hindcast,
        run_forecast = run_forecast,
        hindcast_start = hindcast_start,
        forecast_start = forecast_start,
        forecast_end = forecast_end,
        scenario = scenario,
        confidence_level = confidence_level,
        target_quantile = target_quantile,
        include_lt_tidal_const = include_lt_tidal_const,
        datum_start_year = datum_start_year,
        datum_end_year = datum_end_year
      )
    }
  )
  
  names(results) <- as.character(station_id)
  
  # Combine corresponding outputs across gauges
  scenario_curves <- results %>%
    purrr::map("scenario_curves") %>%
    dplyr::bind_rows()
  
  tidal_datums_summarized <- results %>%
    purrr::map("tidal_datums_summarized") %>%
    dplyr::bind_rows()
  
  list(
    scenario_curves = scenario_curves,
    tidal_datums_summarized = tidal_datums_summarized
  )
}