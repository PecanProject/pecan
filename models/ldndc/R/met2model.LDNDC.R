#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.LDNDC
##' @title Write LDNDC met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param overwrite logical: replace output files if they already exist?
##' @return OK if everything was succesful.
##' @export
##' @author Henri Kajasilta
##-------------------------------------------------------------------------------------------------#
met2model.LDNDC <- function(in.path, in.prefix, outfolder, start_date, end_date, overwrite = FALSE) {
  #PEcAn.logger::logger.severe("NOT IMPLEMENTED")
  
  PEcAn.logger::logger.info("START met2model.LDNDC")
  
  #
  #in.path <- "/data/istem/sandbox/Qvidja/test"
  #in.prefix <- "FieldObs_Qvidja."
  
  #outfolder <- "/data/workflows/PEcAn_15000000076/run/15000215313"
  
  
  
  #start_date <- "2018-12-08"
  #end_date <- "2020-02-28"
  
  # Years
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  
  
  nc_file <- list.files(in.path)[grep("*.nc", list.files(in.path))]
  if(length(nc_file) > 0){
    # Something something
  }
  
  for(year in start_year:end_year){
    
    PEcAn.logger::logger.info(year)
    
    
    old.file <- file.path(in.path, paste0(in.prefix, year, ".nc"))
    
    
    if(file.exists(old.file)){
      
      ## Open netCDF file
      nc <- ncdf4::nc_open(old.file)
      
      # Date were the datapoints' time is relative
      units <- nc$dim$time$units
      
      # Check that simulation doesn't take place before there are data points
      if(year == start_year & PEcAn.utils::datetime2cf(start_date, units, tz = "UTC") < 0){
        PEcAn.logger::logger.severe("No data from the start date, consider postponing it")
      }
      
      
      # Convert the time fractions to be seconds by starting from the date in file's units
      sec <- nc$dim$time$vals
      sec <- udunits2::ud.convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
      
    
      # Calculate the time steps
      # 3600 * 24 = 86 400 (seconds in a day)
      tstep <- 86400/(sec[2]- sec[1])
      
      
      
      
      ## Determine the simulation days and calculate the start and end indexes
      ## by using start_index and end_index functions.
      
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
        simu_days <- seq(1:lubridate::yday(end_date))
        
        # Check the function to see how the end index is calculated
        ind_s <- 1
        ind_e <- end_index(units, start_date, end_date, sec)
        
      }else{
        
        # Simulation days
        simu_days <- seq(lubridate::yday(start_date), lubridate::yday(end_date))
        
        # Need to calculate both first and last index by using functions
        ind_s <- start_index(units, start_date, sec)
        ind_e <- end_index(units, start_date, end_date, sec)
        
      }
      
      # Based on the simulation days, this is year, day and subday info
      # for model to use.
      y <- rep(year, length(simu_days)*tstep) #year
      d <- rep(simu_days, each = tstep) #day
      subd <- rep(1:tstep, length(simu_days)) #subday
      
      # On above the starting and ending indexes has been determined.
      # Here those are just collapsed.
      ind <- ind_s:ind_e
      
        
      
      ## Info for climate.txt file ##
      # Latitude and longitude
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      
      
      # Average air temperature
      Tair <-ncdf4::ncvar_get(nc, "air_temperature")[ind]  ## in Kelvin
      tavg <- udunits2::ud.convert(Tair, "K", "degC")
      
      # Wind speed
      wind <- try(ncdf4::ncvar_get(nc, "wind_speed"))[ind]
      
      # Precipation
      prec <- ncdf4::ncvar_get(nc, "precipitation_flux")[ind]
      
      # Global radiation
      grad <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")[ind]  ## in W/m2
      
      # Vapor Pressure Deficit Pa ---> kPa, need to ne divided by 1000
      VPD <- try(ncdf4::ncvar_get(nc, "water_vapor_saturation_deficit"))[ind]  ## in Pa
      
      # Relative humidity (%)
      rhum <- ncdf4::ncvar_get(nc, "relative_humidity")[ind]
      
      # Air pressure Pa ---> mbar, needs to divided by 100
      press <- ncdf4::ncvar_get(nc, "air_pressure")[ind]
      
      
      # Close connection after all necessary values have been fetch
      ncdf4::nc_close(nc)
      

      # Gather the vectors to dataframe
      data <- as.data.frame(do.call("cbind", list(y = y, d = d, s = subd, prec = prec, tavg = tavg,
                                    grad = grad, vpd = VPD/1000, wind = wind, press = press/100)))
      
      # Write prefix before the actual data
      if(year == start_year){
        
        # General information before the daily/subdaily climate data
        prefix_global <- paste0('\t time = "', start_date, '/', tstep, '"')
        prefix_climate <- paste0('\t id = "', 0, '"')
        prefix_latitude <- paste0('\t latitude = "', lat, '"')
        prefix_longitude <- paste0('\t longitude = "', lon, '"')
        
        data_prefix <- paste(#"%global", prefix_global,
                             "%climate", prefix_climate,
                             "%attributes", prefix_latitude, prefix_longitude,
                             "%data \n", sep = "\n")
        
        # Write prefix information before the data
        cat(data_prefix, file = file.path(outfolder, "climate.txt"))
        
        # For the first year, keep col.names as TRUE
        data.table::fwrite(x = data, file = file.path(outfolder, "climate.txt"),
                           sep = "\t", col.names = T, append = T)
        
      }else{
        # For the other year, col.names are FALSE
        data.table::fwrite(x = data, file = file.path(outfolder, "climate.txt"),
                           sep = "\t", col.names = F, append = T)
      }
      
      
      
    } else{
      PEcAn.logger::logger.info(paste(old.file, "file does not exist"))
    }
    
    
    
  }
  
  
} # met2model.LDNDC


# Calculates the start index. This function determines the difference between
# netcdf file's starting date and simulation's starting date and converting that
# difference to seconds. Returns +1 index based on the matching seconds.
start_index <- function(units, start_date, sec){
  timediff <-(PEcAn.utils::datetime2cf(start_date, units, tz = "UTC")-1)*86400
  if(timediff == 0){
    return(1)
  }else{
    return(which(sec == timediff)+1)
  }
}


end_index <- function(units, start_date, end_date, sec){
  if(lubridate::year(start_date) == lubridate::year(end_date)){
    timediff <-PEcAn.utils::datetime2cf(end_date, units, tz = "UTC")*86400
    return(which(sec == timediff))
  }else{
    timediff <-lubridate::yday(end_date)*86400
    return(which(sec == timediff))
  }
}
