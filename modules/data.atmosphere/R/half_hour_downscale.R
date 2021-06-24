#' @name half_hour_downscale
#' @title half_hour_downscale
#'
#' @return A list of data frames is returned containing information about the data file that can be used to locate it later.  Each
#' data frame contains information about one file.
#'
#' @param input_file location of NOAAGEFS_1hr files
#' @param output_file location where to store half_hour files
#' @param overwrite whether to force hamf_hour_downscale to proceed
#' @param hr set half hour
#'
#' @export
#' 
#' @examples
temporal_downscale_half_hour <- function(input_file, output_file, overwrite = TRUE, hr = 0.5){
  
  # open netcdf
  nc <- ncdf4::nc_open(input_file)
  
  if(stringr::str_detect(input_file, "ens")){
    ens_postion <- stringr::str_locate(input_file, "ens")
    ens_name <- stringr::str_sub(input_file, start = ens_postion[1], end = ens_postion[2] + 2)
    ens <- as.numeric(stringr::str_sub(input_file, start = ens_postion[2] + 1, end = ens_postion[2] + 2))
  }else{
    ens <- 0
    ens_name <- "ens00"
  }
  
  # retrive variable names
  cf_var_names <- names(nc$var)
  
  # generate time vector
  time <- ncdf4::ncvar_get(nc, "time")
  begining_time <- lubridate::ymd_hm(ncdf4::ncatt_get(nc, "time",
                                                      attname = "units")$value)
  time <- begining_time + lubridate::hours(time)
  
  # retrive lat and lon
  lat.in <- ncdf4::ncvar_get(nc, "latitude")
  lon.in <- ncdf4::ncvar_get(nc, "longitude")
  
  # generate data frame from netcdf variables and retrive units
  noaa_data <- tibble::tibble(time = time)
  var_units <- rep(NA, length(cf_var_names))
  for(i in 1:length(cf_var_names)){
    curr_data <- ncdf4::ncvar_get(nc, cf_var_names[i])
    noaa_data <- cbind(noaa_data, curr_data)
    var_units[i] <- ncdf4::ncatt_get(nc, cf_var_names[i], attname = "units")$value
  }
  
  ncdf4::nc_close(nc)
  
  names(noaa_data) <- c("time",cf_var_names)
  
  # spline-based downscaling
  if(length(which(c("air_temperature", "wind_speed","specific_humidity", "air_pressure") %in% cf_var_names) == 4)){
    forecast_noaa_ds <- downscale_spline_to_half_hrly(df = noaa_data, VarNames = c("air_temperature", "wind_speed","specific_humidity", "air_pressure"))
  }else{
    #Add error message
  }
  
  # Convert splined SH, temperature, and presssure to RH
  forecast_noaa_ds <- forecast_noaa_ds %>%
    dplyr::mutate(relative_humidity = qair2rh(qair = forecast_noaa_ds$specific_humidity,
                                              temp = forecast_noaa_ds$air_temperature,
                                              press = forecast_noaa_ds$air_pressure)) %>%
    dplyr::mutate(.data$relative_humidity = .data$relative_humidity,
                  relative_humidity = ifelse(.data$relative_humidity > 1, 0, .data$relative_humidity))
  
  # convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  if("surface_downwelling_longwave_flux_in_air" %in% cf_var_names){
    LW.flux.hrly <- downscale_repeat_6hr_to_half_hrly(df = noaa_data, varName = "surface_downwelling_longwave_flux_in_air")
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, LW.flux.hrly, by = "time")
  }else{
    #Add error message
  }
  
  # convert precipitation to hourly (just copy 6 hourly values over past 6-hour time period)
  if("surface_downwelling_longwave_flux_in_air" %in% cf_var_names){
    Precip.flux.hrly <- downscale_repeat_6hr_to_half_hrly(df = noaa_data, varName = "precipitation_flux")
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, Precip.flux.hrly, by = "time")
  }else{
    #Add error message
  }
  
  # convert cloud_area_fraction to hourly (just copy 6 hourly values over past 6-hour time period)
  if("cloud_area_fraction" %in% cf_var_names){
    cloud_area_fraction.flux.hrly <- downscale_repeat_6hr_to_half_hrly(df = noaa_data, varName = "cloud_area_fraction")
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, cloud_area_fraction.flux.hrly, by = "time")
  }else{
    #Add error message
  }
  
  # use solar geometry to convert shortwave from 6 hr to 1 hr
  if("surface_downwelling_shortwave_flux_in_air" %in% cf_var_names){
    ShortWave.hrly <- downscale_ShortWave_to_half_hrly(df = noaa_data, lat = lat.in, lon = lon.in)
    forecast_noaa_ds <- dplyr::inner_join(forecast_noaa_ds, ShortWave.hrly, by = "time")
  }else{
    #Add error message
  }
  
  #Add dummy ensemble number to work with write_noaa_gefs_netcdf()
  forecast_noaa_ds$NOAA.member <- ens
  
  #Make sure var names are in correct order
  forecast_noaa_ds <- forecast_noaa_ds %>%
    dplyr::select("time", tidyselect::all_of(cf_var_names), "NOAA.member")
  
  #Write netCDF
  write_noaa_gefs_netcdf(df = forecast_noaa_ds,
                                                ens = ens,
                                                lat = lat.in,
                                                lon = lon.in,
                                                cf_units = var_units,
                                                output_file = output_file,
                                                overwrite = overwrite)
  
} #temporal_downscale


#' @title Downscale spline to half hourly
#' @param df dataframe of data to be downscales
#' @param VarNames variable names to be downscaled
#' @param hr hour to downscale to- default is 0.5
#' @return A dataframe of half hourly downscaled state variables
#' @importFrom rlang .data
#' @author Laura Puckett
#' @export
#'

downscale_spline_to_half_hrly <- function(df,VarNames, hr = 0.5){
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

#' @title Downscale shortwave to half hourly
#' @return A dataframe of downscaled state variables
#' 
#' @param df data frame of variables
#' @param lat lat of site
#' @param lon long of site
#' @param hr hour to downscale to- default is 1
#' @importFrom rlang .data
#' @return ShortWave.ds
#' @author Laura Puckett
#' @export
#'

downscale_ShortWave_to_half_hrly <- function(df,lat, lon, hr = 0.5){
  ## downscale shortwave to half hourly
  
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
    dplyr::mutate(rpot = downscale_solar_geom_halfhour(.data$doy, as.vector(lon), as.vector(lat))) %>% # hourly sw flux calculated using solar geometry
    dplyr::group_by(.data$group_6hr) %>%
    dplyr::mutate(avg.rpot = mean(.data$rpot, na.rm = TRUE)) %>% # daily sw mean from solar geometry
    dplyr::ungroup() %>%
    dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(.data$avg.rpot > 0, .data$rpot* (.data$surface_downwelling_shortwave_flux_in_air/.data$avg.rpot),0)) %>%
    dplyr::select(.data$time, .data$surface_downwelling_shortwave_flux_in_air)
  
  return(ShortWave.ds)
  
}


#' @title Downscale repeat to half hourly
#' @param df dataframe of data to be downscaled (Longwave)
#' @param varName variable names to be downscaled
#' @param hr hour to downscale to- default is 0.5
#' @return A dataframe of downscaled data
#' @importFrom rlang .data
#' @author Laura Puckett
#' @export
#'

downscale_repeat_6hr_to_half_hrly <- function(df, varName, hr = 0.5){
  
  #bind variables
  lead_var <- time <- NULL
  #Get first time point
  t0 <- min(df$time)
  
  df <- df %>%
    dplyr::select("time", all_of(varName)) %>%
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
  data.hrly <- data.hrly %>% dplyr::select("time", .data$lead_var) %>%
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
#'
downscale_solar_geom_halfhour <- function(doy, lon, lat) {
  
  dt <- stats::median(diff(doy)) * 86400 # average number of seconds in time interval
  hr <- (doy - floor(doy)) * 48 # hour of day for each element of doy
  
  ## calculate potential radiation
  cosz <- cos_solar_zenith_angle(doy, lat, lon, dt, hr)
  rpot <- 1366 * cosz
  return(rpot)
}
