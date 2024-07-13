#' @title Downscale spline to hourly
#' @param df, dataframe of data to be downscales
#' @param VarNames, variable names to be downscaled
#' @param hr, hour to downscale to- default is 1
#' @return A dataframe of downscaled state variables
#' @importFrom rlang .data
#' @author Laura Puckett
#' @export
#'

downscale_spline_to_hrly <- function(df,VarNames, hr = 1){
  # --------------------------------------
  # purpose: interpolates debiased forecasts from 6-hourly to hourly
  # Creator: Laura Puckett, December 16 2018
  # --------------------------------------
  # @param: df, a dataframe of debiased 6-hourly forecasts
  time <- NULL
  t0 = min(df$time)
  df <- df %>%
    dplyr::mutate(days_since_t0 = difftime(.data$time, t0, units = "days"))

  interp.df.days <- seq(min(df$days_since_t0), as.numeric(max(df$days_since_t0)), 1/(24/hr))

  noaa_data_interp <- tibble::tibble(time = lubridate::as_datetime(t0 + interp.df.days, tz = "UTC"))

  for(Var in 1:length(VarNames)){
    curr_data <- stats::spline(x = df$days_since_t0, y = unlist(df[VarNames[Var]]), method = "fmm", xout = interp.df.days)$y
    noaa_data_interp <- cbind(noaa_data_interp, curr_data)
  }

  names(noaa_data_interp) <- c("time",VarNames)

  return(noaa_data_interp)
}

#' @title Downscale shortwave to hourly
#' @return A dataframe of downscaled state variables
#'
#' @param df, data frame of variables
#' @param lat, lat of site
#' @param lon, long of site
#' @param hr, hour to downscale to- default is 1
#' @importFrom rlang .data
#' @return ShortWave.ds
#' @author Laura Puckett
#' @export
#'
#'

downscale_ShortWave_to_hrly <- function(df,lat, lon, hr = 1){

  ## downscale shortwave to hourly

  t0 <- min(df$time)
  df <- df %>%
    dplyr::select("time", "surface_downwelling_shortwave_flux_in_air") %>%
    dplyr::mutate(days_since_t0 = difftime(.data$time, t0, units = "days")) %>%
    dplyr::mutate(lead_var = dplyr::lead(.data$surface_downwelling_shortwave_flux_in_air, 1))

  interp.df.days <- seq(min(df$days_since_t0), as.numeric(max(df$days_since_t0)), 1/(24/hr))

  noaa_data_interp <- tibble::tibble(time = lubridate::as_datetime(t0 + interp.df.days))

  data.hrly <- noaa_data_interp %>%
    dplyr::left_join(df, by = "time")

  data.hrly$group_6hr <- NA

  group <- 0
  for(i in 1:nrow(data.hrly)){
    if(!is.na(data.hrly$lead_var[i])){
      curr <- data.hrly$lead_var[i]
      data.hrly$surface_downwelling_shortwave_flux_in_air[i] <- curr
      group <- group + 1
      data.hrly$group_6hr[i] <- group
    }else{
      data.hrly$surface_downwelling_shortwave_flux_in_air[i] <- curr
      data.hrly$group_6hr[i] <- group
    }
  }

  ShortWave.ds <- data.hrly %>%
    dplyr::mutate(hour = lubridate::hour(.data$time)) %>%
    dplyr::mutate(doy = lubridate::yday(.data$time) + .data$hour/(24/hr))%>%
    dplyr::mutate(rpot = downscale_solar_geom(.data$doy, as.vector(lon), as.vector(lat))) %>% # hourly sw flux calculated using solar geometry
    dplyr::group_by(.data$group_6hr) %>%
    dplyr::mutate(avg.rpot = mean(.data$rpot, na.rm = TRUE)) %>% # daily sw mean from solar geometry
    dplyr::ungroup() %>%
    dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(.data$avg.rpot > 0, .data$rpot* (.data$surface_downwelling_shortwave_flux_in_air/.data$avg.rpot),0)) %>%
    dplyr::select("time", "surface_downwelling_shortwave_flux_in_air")

  return(ShortWave.ds)

}


#' @title Downscale repeat to hourly
#' @param df, dataframe of data to be downscaled (Longwave)
#' @param varName, variable names to be downscaled
#' @param hr, hour to downscale to- default is 1
#' @return A dataframe of downscaled data
#' @importFrom rlang .data
#' @author Laura Puckett
#' @export
#'

downscale_repeat_6hr_to_hrly <- function(df, varName, hr = 1){

  #bind variables
  lead_var <- time <- NULL
  #Get first time point
  t0 <- min(df$time)

  df <- df %>%
    dplyr::select("time", tidyselect::all_of(varName)) %>%
    #Calculate time difference
    dplyr::mutate(days_since_t0 = difftime(.data$time, t0, units = "days")) %>%
    #Shift valued back because the 6hr value represents the average over the
    #previous 6hr period
    dplyr::mutate(lead_var = dplyr::lead(df[,varName], 1))

  #Create new vector with all hours
  interp.df.days <- seq(min(df$days_since_t0),
                        as.numeric(max(df$days_since_t0)),
                        1 / (24 / hr))

  #Create new data frame
  noaa_data_interp <- tibble::tibble(time = lubridate::as_datetime(t0 + interp.df.days))

  #Join 1 hr data frame with 6 hr data frame
  data.hrly <- noaa_data_interp %>%
    dplyr::left_join(df, by = "time")

  #Fill in hours
  for(i in 1:nrow(data.hrly)){
    if(!is.na(data.hrly$lead_var[i])){
      curr <- data.hrly$lead_var[i]
    }else{
      data.hrly$lead_var[i] <- curr
    }
  }

  #Clean up data frame
  data.hrly <- data.hrly %>% dplyr::select("time", "lead_var") %>%
    dplyr::arrange(.data$time)

  names(data.hrly) <- c("time", varName)

  return(data.hrly)
}



#' @title Calculate potential shortwave radiation
#'
#' @param doy, day of year in decimal
#' @param lon, longitude
#' @param lat, latitude
#' @return vector of potential shortwave radiation for each doy
#'
#' @author Quinn Thomas
#' @export
#'
downscale_solar_geom <- function(doy, lon, lat) {

  dt <- stats::median(diff(doy)) * 86400 # average number of seconds in time interval
  hr <- (doy - floor(doy)) * 24 # hour of day for each element of doy

  ## calculate potential radiation
  cosz <- cos_solar_zenith_angle(doy, lat, lon, dt, hr)
  rpot <- 1366 * cosz
  return(rpot)
}
