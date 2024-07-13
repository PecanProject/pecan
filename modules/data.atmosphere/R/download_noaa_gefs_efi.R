#' download_NOAA_GEFS_EFI
#'
#' @param start_date start date for met forecast
#' @param sitename NEON site name
#' @param outfolder filepath to save ensemble member .nc files
#' @param site.lat site lat
#' @param site.lon site lon
#'
#' @return message confirming download complete and location of .nc files
#'
#'
#' @author Alexis Helgeson
#' 
download_NOAA_GEFS_EFI <- function(sitename, outfolder, start_date, site.lat, site.lon){
  #using the stage2 fcn mean that the met as already been downscaled and gapfilled to 1 hr intervals
  met = PEcAn.data.atmosphere::noaa_stage2(cycle = 0, 
                    version = "v12", 
                    endpoint = "data.ecoforecast.org", 
                    verbose = TRUE, 
                    start_date = start_date)
  
  weather = met %>% 
    dplyr::filter(.data$reference_datetime == as.POSIXct(start_date,tz="UTC"), sitename == sitename) %>%
    dplyr::collect() %>%
    dplyr::select(.data$sitename, .data$prediction, .data$variable, .data$horizon, .data$parameter, .data$datetime)
  
  PEcAn.logger::logger.info("Met Aquired for", sitename, "on", as.character(start_date))
  #grab/calculate timestep, this might not be necessary b/c of the datetime column?
  forecast_date = start_date
  cycle = 0
  hours_char <- unique(weather$horizon)
  forecast_times <- lubridate::as_datetime(forecast_date) + lubridate::hours(as.numeric(cycle)) + lubridate::hours(as.numeric(hours_char))
  
  #the neon4cast fcn already has the weather variable names in cf standard
  cf_var_names <- unique(weather$variable)
  
  noaa_data <- list()
  
  for(v in 1:length(cf_var_names)){
    
    noaa_data[v] <- NULL
    #filter for met variable
    curr_var <- dplyr::filter(weather, .data$variable == cf_var_names[v])
    #remove ensemble member 31 does not cover full timeseries
    #this is a HACK should add a generalized method for ensemble member outlier detection 
    curr_var <- dplyr::filter(curr_var, .data$parameter <= 30)
    noaa_data[[v]] <- list(value = curr_var$prediction,
                           ensembles = curr_var$parameter,
                           forecast.date = curr_var$datetime)
    
  }
  
  names(noaa_data) <- cf_var_names
  
  #adding in windspeed and specific humidity
  cf_var_names1 <- c("surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "precipitation_flux", "air_pressure", "relative_humidity", "air_temperature", "specific_humidity", "wind_speed")
  cf_var_units1 <- c("Wm-2", "Wm-2", "kgm-2s-1", "Pa", "1", "K", "1", "ms-1")
  #calculate specific humdity using realtive humidity (no unit conversion requied as relative humidity is in range 0-1), air temperature (no unit conversion already in K), and air pressure (no unit conversion already in Pa)
  specific_humidity <- rep(NA, length(noaa_data$relative_humidity$value))
  specific_humidity[which(!is.na(noaa_data$relative_humidity$value))] <- PEcAn.data.atmosphere::rh2qair(rh = noaa_data$relative_humidity$value[which(!is.na(noaa_data$relative_humidity$value))],
                                                                                                        T = noaa_data$air_temperature$value[which(!is.na(noaa_data$relative_humidity$value))],
                                                                                                        press = noaa_data$air_pressure$value[which(!is.na(noaa_data$relative_humidity$value))])
  
  #Calculate wind speed from east and north components
  wind_speed <- sqrt(noaa_data$eastward_wind$value^2 + noaa_data$northward_wind$value^2)
  
  forecast_noaa <- tibble::tibble(time = noaa_data$surface_downwelling_longwave_flux_in_air$forecast.date,
                                  NOAA.member = noaa_data$surface_downwelling_longwave_flux_in_air$ensembles,
                                  air_temperature = noaa_data$air_temperature$value,
                                  air_pressure= noaa_data$air_pressure$value,
                                  relative_humidity = noaa_data$relative_humidity$value,
                                  surface_downwelling_longwave_flux_in_air = noaa_data$surface_downwelling_longwave_flux_in_air$value,
                                  surface_downwelling_shortwave_flux_in_air = noaa_data$surface_downwelling_shortwave_flux_in_air$value,
                                  precipitation_flux = noaa_data$precipitation_flux$value,
                                  specific_humidity = specific_humidity,
                                  wind_speed = wind_speed)
  
  PEcAn.logger::logger.info("Met df complied including specific humidity and wind speed")
  
  #create directory to save ensemble member if one does not already exist
  output_path = file.path(outfolder, "noaa/NOAAGEFS_1hr/", sitename, "/", forecast_date, "/00/")
  if(!dir.exists(output_path)){dir.create(output_path, recursive = TRUE)}
  
  for (ens in 1:length(unique(forecast_noaa$NOAA.member))) { # i is the ensemble number
    
    forecast_noaa_ens <- forecast_noaa %>%
      dplyr::filter(.data$NOAA.member == ens) %>%
      dplyr::filter(!is.na(.data$air_temperature))
    
    end_date <- forecast_noaa_ens %>%
      dplyr::summarise(max_time = max(.data$time))
    
    identifier = paste("NOAA_GEFS", sitename, ens, format(as.POSIXct(forecast_date), "%Y-%m-%dT%H:%M"), 
                       format(end_date$max_time, "%Y-%m-%dT%H:%M"), sep="_")     
    
    fname <- paste0(identifier, ".nc")
    #ensemble_folder = file.path(output_path, identifier)
    output_file <- file.path(output_path,fname)
    
    #Write netCDF
    if(!nrow(forecast_noaa_ens) == 0){      
      PEcAn.data.atmosphere::write_noaa_gefs_netcdf(df = forecast_noaa_ens,ens, lat = site.lat, lon = site.lon, cf_units = cf_var_units1, output_file = output_file, overwrite = TRUE)
    }else {next}
  }
  
  return(PEcAn.logger::logger.info("Met download complete and saved as .nc files at", output_path))
}




