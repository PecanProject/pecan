#' Build high tides into scenario table
#'
#' This function takes a sea-level scenario table and elevations for 1 to 3 tidal cycles and outputs a scenario table with annualized high tide levels.  
#' @param scenarioCurve a data frame, including year, mean sea level, and suspended sediment concentration, with each row being a year in the scenario
#' @param meanSeaLevelDatum a numeric, Mean Sea Level over a tidal datum period
#' @param meanHighWaterDatum a numeric, Mean High Water level over the last datum period
#' @param meanHighHighWaterDatum a numeric, Mean Higher High Water level over the last datum period
#' @param meanHighHighWaterSpringDatum a numeric, Mean Higher High Spring Tide Water level over the last datum period
#' @param lunarNodalAmp the amplitude of the 18-year lunar nodal cycle
#' @param lunarNodalPhase a numeric, in decimal years (YYYY) the start year of the sine wave representing the lunar nodal cycle 
#' 
#' @return a data frame, including the sea-level and suspended sediment concentration scenario inputted, with annual high tide datum(s) added
#' @export
buildHighTideScenario <- function(scenarioCurve, meanSeaLevelDatum=scenarioCurve$meanSeaLevel[1], 
                                  meanHighWaterDatum, meanHighHighWaterDatum, meanHighHighWaterSpringDatum, 
                                  lunarNodalAmp, lunarNodalPhase=2011.181) {
  
  # In create a meanHighWater and add it to the scenario
  scenarioCurve$meanHighWater <- predictLunarNodalCycle(year = scenarioCurve$year, meanSeaLevel= scenarioCurve$meanSeaLevel, 
                                             meanSeaLevelDatum = meanSeaLevelDatum, floodElv=meanHighWaterDatum, 
                                             lunarNodalAmp=lunarNodalAmp)
  
  # If meanHighHighWater and meanHighHighWaterSpring are arguements add them to the scenario table too
  if (!missing(meanHighHighWaterDatum) & !missing(meanHighHighWaterSpringDatum)) {
    scenarioCurve$meanHighHighWater <- predictLunarNodalCycle(year = scenarioCurve$year, meanSeaLevel=scenarioCurve$meanSeaLevel,
                                                meanSeaLevelDatum = meanSeaLevelDatum, floodElv=meanHighHighWaterDatum, 
                                                lunarNodalAmp=lunarNodalAmp)
    scenarioCurve$meanHighHighWaterSpring <- predictLunarNodalCycle(year = scenarioCurve$year, meanSeaLevel= scenarioCurve$meanSeaLevel, 
                                                 meanSeaLevelDatum = meanSeaLevelDatum, floodElv=meanHighHighWaterSpringDatum, 
                                                 lunarNodalAmp=lunarNodalAmp)
  }
  return(scenarioCurve)
}