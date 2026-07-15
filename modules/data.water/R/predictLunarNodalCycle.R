#' Predict lunar nodal cycle
#'
#' This function builds a high tide level based on the 18.61 year lunar nodal cycle.
#' @param year a numeric, the current year
#' @param meanSeaLevelDatum a numeric, mean sea-level over the last complete 18 year datum period
#' @param meanSeaLevel a vector, mean sea-level for each year of the scenario we are predicting
#' @param floodElv a numeric, high tide level over the last complete 18 year datum period
#' @param lunarNodalAmp a numeric, the amplitude of the 18 year lunar nodal cycle
#' @param lunarNodalPhase a numeric, in decimal years (YYYY) the start year of the sine wave representing the lunar nodal cycle 
#' 
#' @references Morris, J. T., Sundberg, K., & Hopkinson, C. S.- (2013). Salt marsh primary production and its responses to relative sea level and nutrients in estuaries at Plum Island, Massachusetts, and North Inlet, South Carolina, USA. Oceanography, 26(3), 78-84.
#' @references Rochlin, I., & Morris, J. T. (2017). Regulation of salt marsh mosquito populations by the 18.6‐yr lunar‐nodal cycle. Ecology, 98(8), 2059-2068.
#' 
#' @export
predictLunarNodalCycle <- function(year, floodElv, meanSeaLevelDatum, meanSeaLevel, lunarNodalAmp, lunarNodalPhase=2011.181) {
  # Build meanHighWater lines based on meanSeaLevel, long-term tidal range and lunar nodal amplitude
  meanHighWater <- meanSeaLevel + (floodElv-meanSeaLevelDatum) + (lunarNodalAmp * sin(2*pi*(year-lunarNodalPhase)/18.61))
  return(meanHighWater)
}
