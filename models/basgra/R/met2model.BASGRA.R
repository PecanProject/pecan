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
##' @name met2model.BASGRA
##' @title Write BASGRA met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param start_date beginning of the weather data
##' @param end_date end of the weather data
##' @return OK if everything was succesful.
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
met2model.BASGRA <- function(in.path, in.prefix, outfolder, overwrite = FALSE, 
                             start_date, end_date, ...) {

  PEcAn.logger::logger.info("START met2model.BASGRA")
  
  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  out.file <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"),
                    strptime(end_date, "%Y-%m-%d"),
                    "txt",
                    sep = ".")
  
  out.file.full <- file.path(outfolder, out.file)
  
  results <- data.frame(file = out.file.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/csv",
                        formatname = "Weather-Bioforsk",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  PEcAn.logger::logger.info("internal results")
  PEcAn.logger::logger.info(results)
  
  if (file.exists(out.file.full) && !overwrite) {
    PEcAn.logger::logger.debug("File '", out.file.full, "' already exists, skipping to next file.")
    return(invisible(results))
  }
  
  out.list <- list()
  
  ctr <- 1
  for(year in start_year:end_year) {
    
    PEcAn.logger::logger.info(year)
    
    # prepare data frame for BASGRA format, daily inputs, but doesn't have to be full year
    diy <- PEcAn.utils::days_in_year(year)
    out <- data.frame(ST = rep(42, diy), # station number, not important
                      YR = rep(year, diy), # year
                      doy = seq_len(diy)) # day of year, simple implementation for now
                      
    
    
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))
    
    if (file.exists(old.file)) {
      
      ## open netcdf
      nc <- ncdf4::nc_open(old.file)  
      
      ## convert time to seconds
      sec <- nc$dim$time$vals
      sec <- udunits2::ud.convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
      
      dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
      tstep <- round(86400 / dt)
      dt <- 86400 / tstep
      
      ## extract variables
      Tair <-ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
      Tair_C <- udunits2::ud.convert(Tair, "K", "degC")
      
      # compute daily mean, min and max
      ind <- rep(seq_len(diy), each = tstep)
      t_dmean <- tapply(Tair_C, ind, mean, na.rm = TRUE) # maybe round these numbers 
      t_dmax  <- tapply(Tair_C, ind, max,  na.rm = TRUE) 
      t_dmin  <- tapply(Tair_C, ind, min,  na.rm = TRUE)
      
      out$T     <- t_dmean # mean temperature (degrees Celsius)
      out$TMMXI <- t_dmax  # max temperature (degrees Celsius)
      out$TMMNI <- t_dmin  # min temperature (degrees Celsius)    
      
      RH <-ncdf4::ncvar_get(nc, "relative_humidity")  # %
      RH <- tapply(RH, ind, mean, na.rm = TRUE) 
      
      out$RH <- RH # relative humidity (%)
      
      Rain  <- ncdf4::ncvar_get(nc, "precipitation_flux") # kg m-2 s-1
      raini <- tapply(Rain*86400, ind, mean, na.rm = TRUE) 
      
      out$RAINI <- raini # precipitation (mm d-1)	
      
      
      U <- ncdf4::ncvar_get(nc, "eastward_wind")
      V <- ncdf4::ncvar_get(nc, "northward_wind")
      ws <- sqrt(U ^ 2 + V ^ 2)
      
      out$WNI <- tapply(ws, ind, mean,  na.rm = TRUE) # mean wind speed (m s-1)			

      rad <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")
      gr  <- rad *  0.0864 # W m-2 to MJ m-2 d-1
      
      out$GR <- tapply(gr, ind, mean, na.rm = TRUE) # irradiation (MJ m-2 d-1)
      
      ncdf4::nc_close(nc)
    } else {
      PEcAn.logger::logger.info("File for year", year, "not found. Skipping to next year")
      next
    }
    
    out.list[[ctr]] <- out
    ctr <- ctr + 1
  } # end for-loop around years
  
  clim <- do.call("rbind", out.list)
  
  ## write output
  write.table(clim, out.file.full, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  return(invisible(results))
  
} # met2model.BASGRA
