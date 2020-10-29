##' @title Downscale shortwave to hourly
##' @return A dataframe of downscaled state variables
##'
##' @param debiased, data frame of variables 
##' @param time0, first timestep
##' @param time_end, last time step
##' @param lat, lat of site
##' @param lon, long of site
##' @param output_tz, output timezone
##' @export
##' 
##' @author Laura Puckett
##' 
##' 

downscale_ShortWave_to_hrly <- function(debiased, time0, time_end, lat, lon, output_tz = "UTC"){
  ## downscale shortwave to hourly
   
  
  downscale_solar_geom <- function(doy, lon, lat) {
    
    dt <- median(diff(doy)) * 86400 # average number of seconds in time interval
    hr <- (doy - floor(doy)) * 24 # hour of day for each element of doy
    
    ## calculate potential radiation
    cosz <- PEcAn.data.atmosphere::cos_solar_zenith_angle(doy, lat, lon, dt, hr)
    rpot <- 1366 * cosz
    return(rpot)
  }
  grouping = append("NOAA.member", "timestamp")
  
  surface_downwelling_shortwave_flux_in_air<- rep(debiased$surface_downwelling_shortwave_flux_in_air, each = 6)
  time = rep(seq(from = as.POSIXct(time0, tz = output_tz), to = as.POSIXct(time_end + lubridate::hours(5), tz = output_tz), by = 'hour'), times = 21)
  
  ShortWave.hours <- as.data.frame(surface_downwelling_shortwave_flux_in_air)
  ShortWave.hours$timestamp = time
  ShortWave.hours$NOAA.member =  rep(debiased$NOAA.member, each = 6)
  ShortWave.hours$hour = as.numeric(format(time, "%H"))
  ShortWave.hours$group = as.numeric(as.factor(format(ShortWave.hours$time, "%d")))
  
  
  
  ShortWave.ds <- ShortWave.hours %>% 
    dplyr::mutate(doy = lubridate::yday(timestamp) + hour/24) %>%
    dplyr::mutate(rpot = downscale_solar_geom(doy, lon, lat)) %>% # hourly sw flux calculated using solar geometry
    dplyr::group_by_at(c("group", "NOAA.member")) %>%
    dplyr::mutate(avg.rpot = mean(rpot, na.rm = TRUE)) %>% # daily sw mean from solar geometry
    dplyr::ungroup() %>%
    dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(avg.rpot > 0, rpot* (surface_downwelling_shortwave_flux_in_air/avg.rpot),0)) %>%
    dplyr::select(timestamp, NOAA.member, surface_downwelling_shortwave_flux_in_air) %>% 
    dplyr::filter(timestamp >= min(debiased$timestamp) & timestamp <= max(debiased$timestamp))  
  

}


