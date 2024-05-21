
##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.LDNDC
##' @title Write LDNDC met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param start_date start date of the results
##' @param end_date end date of the results
##' @param overwrite logical: replace output files if they already exist?
##' @param ... Other arguments
##' @return invisible(results)
##' @export
##' @author Henri Kajasilta
##-------------------------------------------------------------------------------------------------#
met2model.LDNDC <- function(in.path, in.prefix, outfolder, start_date, end_date, overwrite = FALSE, ...) {
  
  # Logger info
  PEcAn.logger::logger.info("START met2model.LDNDC")
  
  # Years
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  
  # Check the nc-files
  nc_file <- list.files(in.path)[grep("*.nc", list.files(in.path))]
  
  # Set-up the outfile that will be returned as results
  out.file <- paste(in.prefix, format(as.Date(start_date), "%Y-%m-%d"),
                    format(as.Date(end_date), "%Y-%m-%d"),
                    "txt",
                    sep = ".")
  
  out.file.full <- file.path(outfolder, out.file)
  
  # Results
  results <- data.frame(file = out.file.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/plain",
                        formatname = "LDNDC_Climate",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  
  
  
  if(length(nc_file) == 0){
    PEcAn.logger::logger.severe("Based on the given file path, nc-files was not found")
  }
  
  
  for(year in start_year:end_year){
    
    # Year
    PEcAn.logger::logger.info(year)
    
    
    old.file <- file.path(in.path, paste0(in.prefix, ".", year, ".nc"))
    
    
    if(file.exists(old.file)){
      
      ## Open netCDF file
      nc <- ncdf4::nc_open(old.file)
      
      # Data points are relational to this date
      units <- nc$dim$time$units
      
      # Check that the simulation doesn't take place before there are data points
      if(year == start_year & PEcAn.utils::datetime2cf(start_date, units, tz = "UTC") < 0){
        PEcAn.logger::logger.severe("Data in the met drivers seem not to be available
                                    for given start date specified in simulation. Consider applying a later start date")
      }
      
      
      # Convert the time fractions to be seconds by starting from the date in file's units
      sec <- nc$dim$time$vals
      sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(units, " "))[1], "seconds")
      
    
      # Calculate the time steps
      # 3600 * 24 = 86 400 (seconds in a day)
      tstep <- 86400/(sec[2]- sec[1])
      
      
      
      
      ## Determine the simulation days and calculate the start and end indexes
      ## by using start_index and end_index functions. These functions are found
      ## at the end of this file
      
      if(year != start_year & year != end_year){  
        
        # Simulation days
        simu_days <- 1:PEcAn.utils::days_in_year(year)
        
        # Whole year, from first to last index
        ind_s <- 1
        ind_e <- which.max(sec)
        
      }else if(year == start_year & year != end_year){
        
        # Simulation days
        simu_days <- seq(lubridate::yday(start_date),PEcAn.utils::days_in_year(year))
        
        # Check the function to see how the start index is calculated
        ind_s <- start_index(units, start_date, sec)
        ind_e <- which.max(sec)
        
      }else if(year != start_year & year == end_year){
        
        # Simulation days
        simu_days <- seq(1,lubridate::yday(end_date))
        
        # Check the function to see how the end index is calculated
        ind_s <- 1
        ind_e <- end_index(units, start_date, end_date, sec, tstep)
        
      }else{
        
        # Simulation days
        simu_days <- seq(lubridate::yday(start_date), lubridate::yday(end_date))
        
        # Need to calculate both first and last index by using functions
        ind_s <- start_index(units, start_date, sec)
        ind_e <- end_index(units, start_date, end_date, sec, tstep)
        
      }
      
      # Based on the simulation days, this is year, day and subday info
      # for model to use.
      y <- rep(year, length(simu_days)*tstep) #year
      d <- rep(simu_days, each = tstep) #day
      subd <- rep(1:tstep, length(simu_days)) #subday
      
      # On above the starting and ending indexes has been determined.
      # Here those are just collapsed.
      ind <- ind_s:ind_e
      
        
      
      ## Info for climate file that model is using ##
      # Latitude and longitude
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      
      
      # Average air temperature
      Tair <-ncdf4::ncvar_get(nc, "air_temperature")[ind]  ## in Kelvin
      tavg <- PEcAn.utils::ud_convert(Tair, "K", "degC")
      
      # Wind speed
      wind <- try(ncdf4::ncvar_get(nc, "wind_speed"))[ind]
      if (!is.numeric(wind)) {
        U <- ncdf4::ncvar_get(nc, "eastward_wind")[ind]
        V <- ncdf4::ncvar_get(nc, "northward_wind")[ind]
        wind <- sqrt(U ^ 2 + V ^ 2)
        PEcAn.logger::logger.info("wind_speed absent; calculated from eastward_wind and northward_wind")
      }
      #wind <- try(ncdf4::ncvar_get(nc, "wind_speed"))[ind]
      
      # Precipation
      prec <- ncdf4::ncvar_get(nc, "precipitation_flux")[ind]
      
      # Global radiation
      grad <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")[ind]  ## in W/m2
      
      # Air pressure Pa ---> mbar, needs to be converted
      press <- ncdf4::ncvar_get(nc, "air_pressure")[ind]
      press <- PEcAn.utils::ud_convert(press, "Pa", "millibar")
      
      # Vapor Pressure Deficit Pa ---> kPa, needs to be converted, check below
      VPD <- try(ncdf4::ncvar_get(nc, "water_vapor_saturation_deficit"))[ind]  ## in Pa
      if (!is.numeric(VPD)) {
        
        # Fetch these values in order to construct VPD
        Qair <-ncdf4::ncvar_get(nc, "specific_humidity")[ind]  #humidity (kg/kg)
        SVP <- PEcAn.utils::ud_convert(PEcAn.data.atmosphere::get.es(tavg), "millibar", "Pa")  ## Saturation vapor pressure
        
        # VPD calculated, if not directly found from the nc-file
        VPD <- SVP * (1 - PEcAn.data.atmosphere::qair2rh(Qair, tavg, press))
        PEcAn.logger::logger.info("water_vapor_saturation_deficit absent; VPD calculated from Qair, Tair, and SVP (saturation vapor pressure) ")
      }
      VPD <- PEcAn.utils::ud_convert(VPD, "Pa", "kPa") # Pa ---> kPa
      
      # Relative humidity (%)
      rhum <- ncdf4::ncvar_get(nc, "relative_humidity")[ind]
      
      # Close connection after all necessary values have been fetch
      ncdf4::nc_close(nc)
      

      # Gather the vectors to dataframe
      data <- as.data.frame(do.call("cbind", list(y = y, d = d, s = subd,
                                                  prec = prec*86400/tstep, #mm
                                                  tavg = tavg, #degC
                                                  grad = grad, #W m-2
                                                  vpd = VPD, #kPa
                                                  wind = wind, #m s-1
                                                  press = press))) #mbar
      
      # Write prefix before the actual data
      if(year == start_year){
        
        # General information before the daily/subdaily climate data
        prefix_global <- paste0('\t time = "', start_date, '/', tstep, '"')
        prefix_climate <- paste0('\t id = "', 0, '"')
        prefix_latitude <- paste0('\t latitude = "', lat, '"')
        prefix_longitude <- paste0('\t longitude = "', lon, '"')
        
        data_prefix <- paste("%global", prefix_global, # global includes the global time, but this is already got
                              #from elsewhere and not necessary here(?).
                             "%climate", prefix_climate,
                             "%attributes", prefix_latitude, prefix_longitude,
                             "%data \n", sep = "\n")
        
        # Write prefix information before the data
        cat(data_prefix, file = file.path(outfolder, out.file))
        
        # For the first year, keep col.names as TRUE
        readr::write_delim(x = data, file = file.path(outfolder, out.file),
                         delim = "\t", append = T, quote = "none")
        
        
      }else{
        # For the other years, col.names are FALSE
        readr::write_delim(x = data, file = file.path(outfolder, out.file),
                           delim = "\t", col_names = F, append = T)
      }
      
      
      
    } else{
      PEcAn.logger::logger.severe(paste(old.file, "file does not exist"))
    }
    
    
    
  }

  return(invisible(results))
  
} # met2model.LDNDC


# Calculates the start index. This function determines the difference between
# netcdf file's starting date and simulation's starting date and converting that
# difference to seconds. Returns +1 index based on the matching seconds.
start_index <- function(units, start_date, sec){
  timediff <-round((PEcAn.utils::datetime2cf(start_date, units, tz = "UTC"))*86400)
  if(timediff == 0){
    return(1)
  }else{
    return(which(sec == timediff))
  }
}


end_index <- function(units, start_date, end_date, sec, tstep){
  #if(lubridate::year(start_date) == lubridate::year(end_date)){
  timediff <- round((PEcAn.utils::datetime2cf(end_date, units, tz = "UTC")+1)*86400)
  return(which(sec == (timediff-86400/tstep)))
}
