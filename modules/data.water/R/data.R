#' Tidal datum values for 125 stations 
#'
#' ...Description TK...
#'
#' @format
#' A data frame with 51482 rows and 10 columns:
#' \describe{
#'   \item{Datum}{Datum name, using NOAA standard abbreviations.
#'    See https://tidesandcurrents.noaa.gov/datum_options.html}
#'   \item{n_obs}{}
#'   \item{observed}{Observed water elevation in ?feet?}
#'   \item{n_pred}{}
#'   \item{predicted}{}
#'   \item{risingTime}{}
#'   \item{fallingTime}{}
#'   \item{year}{}
#'   \item{station_id}{}
#'   \item{station_name}{}  
#' }
#' @source from <url>, compiled by James Holmquist
"annual_compiled_datums"

#' Projected sea level change under climate scenarios from IPCC Assessment
#'  Report 6
#'
#' ...description TK...
#'
#' @format
#' A data frame with 2,278,458 rows and 10 columns:
#' \describe{
#'   \item{psmsl_id}{}
#'   \item{latitude}{}
#'   \item{longitude}{}
#'   \item{year}{}
#'   \item{quantile}{}           
#'   \item{sea_level_change_mm}{}
#'   \item{noaa_id}{}
#'   \item{noaa_name}{}
#'   \item{scenario_name}{}
#'   \item{confidence}{}
#' }
#'
#' @source TK
"ar6_us_compiled_scenarios"

#' Title TK
#'
#' ...description TK...
#'
#' @format
#' A data frame with 1000 rows and 17 columns:
#' \describe{
#'   \item{station_id}{}
#'   \item{station_name}{}
#'   \item{start_date}{}
#'   \item{end_date}{}
#'   \item{tide}{}
#'   \item{mean_amp}{}
#'   \item{amp18}{}
#'   \item{phase18}{}
#'   \item{amp44}{}
#'   \item{phase44}{}
#'   \item{rse}{}
#'   \item{r2}{}
#'   \item{mean_ampb}{}
#'   \item{amp18b}{}
#'   \item{phase18b}{}
#'   \item{rseb}{}
#'   \item{r2b}{}
#' }
#'
#' @source TK
"long_term_tidal_constituents"

#' Locations, IDs, and available date ranges for 126 tidal stations
#'
#' ...description TK...
#'
#' @format
#' A data frame with 126 rows and 7 columns:
#' \describe{
#'   \item{noaa_id}{}
#'   \item{startDate}{}
#'   \item{endDate}{}
#'   \item{noaa_name}{}
#'   \item{latitude}{}
#'   \item{longitude}{}
#'   \item{state}{}
#' }
#'
#' @source TK
"noaa_us_gauge_info"
