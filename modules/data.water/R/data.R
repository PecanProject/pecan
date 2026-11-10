#' Tidal datum values for 125 NOAA stations
#'
#' Average annual datums for different types of water levels,
#'  recalculated from NOAA tide gauges using the TideHarmonics R package
#'  at average annual time steps.
#'
#' @format
#' A data frame with 51482 rows and 10 columns:
#' \describe{
#'   \item{Datum}{Datum name, HHA=Higher High Annual, HOT=Highest Observed Tide, MSL=Mean Sea Level,
#'      LLA=Lower Low Annual, LOT=Lowest Observed Tide, HHS=Higher High Spring, HH=Higher High Daily,
#'      LH=Lower High Daily, HL=Higher Low Daily, LL=Lower Low Daily, LLS=Lower Low Spring}
#'   \item{n_obs}{Number of observations}
#'   \item{observed}{Observed water elevation in meters relative to NAVD88}
#'   \item{n_pred}{Number of predictions}
#'   \item{predicted}{Predicted water elevation in meters relative to NAVD88}
#'   \item{risingTime}{Average rising time of the flood stage (hours)}
#'   \item{fallingTime}{Average falling time of the flood stage (hours)}
#'   \item{year}{Calendar year (YYYY)}
#'   \item{station_id}{Unique numeric station identifier}
#'   \item{station_name}{Unique station name}
#' }
#' @source from https://github.com/Smithsonian/Tidal_Scenarios, compiled by James Holmquist
"annual_compiled_datums"

#' Projected sea level change under climate scenarios from IPCC Assessment
#'  Report 6
#'
#' Probabilistic and localized relative sea-level rise scenarios produced for NOAA
#'  tide gauges for the 6th IPCC Assesment Report by Kopp and Garner (2023).
#'  Scenarios were reprocessed by James Holmquist into long-form tables that could be
#'  more readily queried.
#'
#'
#' @format
#' A data frame with 2,278,458 rows and 10 columns:
#' \describe{
#'   \item{psmsl_id}{Unique permanant service for mean sea level identifier}
#'   \item{latitude}{Gauge latitude in decimal degrees WGS84.}
#'   \item{longitude}{Gauge longitude in decimal degrees WGS84.}
#'   \item{year}{Calendar year (YYYY)}
#'   \item{quantile}{Value between 0 and 1 indicating position of the sea-level rise
#'    magnitude within the probability space of the scnenario.}
#'   \item{sea_level_change_mm}{Sea level change relative to the baseline period (1986–2005)}
#'   \item{noaa_id}{Unique NOOA numeric station identifier}
#'   \item{noaa_name}{Unique NOAA station name}
#'   \item{scenario_name}{Unique scenario name indicating a Shared Socioeconomic Pathway (ssp)
#'    or temperature limitation window (tlim). See source data for additional detail.}
#'   \item{confidence}{Indicates whether forecats are made propegating uncertainty at
#'     'medium' or 'low' confidence}
#' }
#'
#' @source Kopp, R. E., Garner, G. G., Hermans, T. H., Jha, S., Kumar, P., Reedy, A., ... & Smith, C. (2023).
#'  The Framework for Assessing Changes To Sea-level (FACTS) v1. 0:
#'  a platform for characterizing parametric and structural uncertainty
#'  in future global, relative, and extreme sea-level change.
#'  Geoscientific Model Development, 16(24), 7461-7489.
#'
#'  Reformatted in https://github.com/Smithsonian/Tidal_Scenarios, by James Holmquist
"ar6_us_compiled_scenarios"

#' Long term tidal constituents for NOAA tide gauges
#'
#' Harmonic constituents of long term tidal amplitude changes for NOAA tide gauges.
#'  4.4 and 18.6 year cycles in tidal amplitude change are accounted for following formulas
#'  presented in Peng et al. (2018). Formulas are modified to fit harmonic constituents to
#'  different tidal flood and ebb types matching `annual_compiled_datums`.
#'
#' @format
#' A data frame with 1000 rows and 17 columns:
#' \describe{
#'   \item{station_id}{Unique numeric NOAA station identifier}
#'   \item{station_name}{Unique NOAA station name}
#'   \item{start_date}{Calendar start date for observation (YYYYMMDD)}
#'   \item{end_date}{Calendar end date for observation (YYYYMMDD)}
#'   \item{tide}{Type of tide: HHA=Higher High Annual, HOT=Highest Observed Tide, MSL=Mean Sea Level,
#'      LLA=Lower Low Annual, LOT=Lowest Observed Tide, HHS=Higher High Spring, HH=Higher High Daily,
#'      LH=Lower High Daily, HL=Higher Low Daily, LL=Lower Low Daily, LLS=Lower Low Spring}
#'   \item{mean_amp}{Mean tidal amplitude for model 1 (4.4 and 18.6 year cycles accounted for)}
#'   \item{amp18}{18.6 year cycle amplitude of amplitude for model 1 (4.4 and 18.6 year cycles accounted for)}
#'   \item{phase18}{18.6 year cycle phase for model 1 (4.4 and 18.6 year cycles accounted for)}
#'   \item{amp44}{4.4 year cycle amplitude for model 1}
#'   \item{phase44}{4.4 year cycle phase for model 1}
#'   \item{rse}{Residual standard error for model 1 (4.4 and 18.6 year cycles accounted for)}
#'   \item{r2}{R-squared value for model 1 (4.4 and 18.6 year cycles accounted for)}
#'   \item{mean_ampb}{Mean tidal amplitude for model 2 (18.6 year cycle accounted for)}
#'   \item{amp18b}{18.6 year cycle amplitude of amplitude for model 2 (18.6 year cycle accounted for)}
#'   \item{phase18b}{18.6 year cycle phase for model 2 (18.6 year cycle accounted for)}
#'   \item{rseb}{Residual standard error for model 2 (18.6 year cycle accounted for)}
#'   \item{r2b}{R-squared value for model 2 (18.6 year cycle accounted for)}
#' }
#'
#' @source Peng, D., Hill, E. M., Meltzner, A. J., & Switzer, A. D. (2019).
#'  Tide gauge records show that the 18.61‐year nodal tidal cycle can change high water levels by up to 30 cm.
#'  Journal of Geophysical Research: Oceans, 124(1), 736-749.
#'
#'  Recalculated in https://github.com/Smithsonian/Tidal_Scenarios, by James Holmquist
#'
"long_term_tidal_constituents"

#' Locations, IDs, and available date ranges for 126 NOAA tidal stations
#'
#' Publically available NOAA tide gauge positional and temporal information organized
#'  into a single table.
#'
#' @format
#' A data frame with 126 rows and 7 columns:
#' \describe{
#'   \item{noaa_id}{Unique numeric NOAA station identifier}
#'   \item{startDate}{Calendar start date for observation record (YYYYMMDD)}
#'   \item{endDate}{Calendar end date for observation record (YYYYMMDD)}
#'   \item{noaa_name}{Unique NOAA station name}
#'   \item{latitude}{Gauge latitude in decimal degrees WGS84}
#'   \item{longitude}{Gauge longitude in decimal degrees WGS84}
#'   \item{state}{State- or territory-level two letter abbreviation}
#' }
#'
#' @source Queried from NOAA servers and organized in https://github.com/Smithsonian/Tidal_Scenarios, by James Holmquist
"noaa_us_gauge_info"
