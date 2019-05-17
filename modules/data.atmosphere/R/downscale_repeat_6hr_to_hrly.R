##' @title Downscale repeat to hourly
##' @return A dataframe of downscaled data
##'
##' @param data.6hr, dataframe of data to be downscaled (Longwave)
##' @export
##' 
##' @author Laura Puckett
##' 
##' 

downscale_repeat_6hr_to_hrly <- function(data.6hr){
  data.hrly = data.6hr %>%
  dplyr::group_by_all() %>%
  tidyr::expand(timestamp = c(timestamp,
                       timestamp  + lubridate::hours(1),
                       timestamp  + lubridate::hours(2),
                       timestamp  + lubridate::hours(3),
                       timestamp  + lubridate::hours(4),
                       timestamp  + lubridate::hours(5),
                       timestamp  + lubridate::hours(6))) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(timestamp = lubridate::as_datetime(timestamp, tz = "UTC")) %>% 
  dplyr::filter(timestamp >= min(data.6hr$timestamp) & timestamp <= max(data.6hr$timestamp)) %>% 
    dplyr::distinct()

  #arrange(timestamp)
return(data.hrly)
}
