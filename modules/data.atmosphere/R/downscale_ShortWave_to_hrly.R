##' @title Downscale shortwave to hourly
##' @return A dataframe of downscaled state variables
##'
##' @param debiased, data frame of variables 
##' @param time0, first timestep
##' @param lat, lat of site
##' @param lon, long of site
##' @param output_tz, output timezone
##' @export
##' 
##' @author Laura Puckett
##' 
##' 


downscale_ShortWave_to_hrly <- function(debiased, time0, lat, lon, output_tz = "UTC"){
  ## downscale shortwave to hourly
  library(lubridate)

    grouping = append("NOAA.member", "timestamp")
    ShortWave.hours <- debiased %>%
      dplyr::group_by_at(grouping) %>%
      tidyr::expand(hour = 0:23) %>% 
      dplyr::mutate(date = lubridate::as_date(timestamp))
  
  ShortWave.ds <- debiased %>% 
    dplyr::select(surface_downwelling_shortwave_flux_in_air, grouping) %>%
    full_join(ShortWave.hours, by = grouping) %>%
    dplyr::mutate(timestamp = lubridate::as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = output_tz) - 1*60*60) %>% # subtract one hour to convert times from representing pervious hour to representing the next hour
    dplyr::mutate(doy = yday(timestamp) + hour/24) %>%
    dplyr::mutate(rpot = downscale_solar_geom(doy, lon, lat)) %>% # hourly sw flux calculated using solar geometry
    dplyr::group_by_at(grouping) %>%
    dplyr::mutate(avg.rpot = mean(rpot, na.rm = TRUE)) %>% # daily sw mean from solar geometry
    ungroup() %>%
    dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(avg.rpot > 0, surface_downwelling_shortwave_flux_in_air* (rpot/avg.rpot),0)) %>%
    dplyr::select(timestamp, NOAA.member, surface_downwelling_shortwave_flux_in_air) %>% 
    filter(timestamp >= min(debiased$timestamp) & timestamp <= max(debiased$timestamp))  
  

  }