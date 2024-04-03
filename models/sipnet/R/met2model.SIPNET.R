
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# R Code to convert NetCDF CF met files into SIPNET met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model wrapper for SIPNET
##'
##' @title met2model.SIPNET
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files OR the full file name if year.fragment = TRUE
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param year.fragment the function should ignore whether or not the data is stored as a set of complete years (such as for forecasts).
##' @author Luke Dramko, Michael Dietze, Alexey Shiklomanov, Rob Kooper
met2model.SIPNET <- function(in.path, in.prefix, outfolder, start_date, end_date,
                             overwrite = FALSE, verbose = FALSE, year.fragment = FALSE, ...) {
 
  

  PEcAn.logger::logger.info("START met2model.SIPNET")
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  if (year.fragment) { # in.prefix is not guaranteed to contain the file extension.
    escaped <- gsub("(\\W)", "\\\\\\1", in.prefix) # The file name may contain special characters that could mess up the regular expression.
    matching_files <- grep(escaped, list.files(in.path), value=TRUE) 
    if (length(matching_files) == 0) {
      PEcAn.logger::logger.severe(paste0("No files found matching ", in.prefix, "; cannot process data."))
    }
    
    # This function is supposed to process netcdf files, so we'll search for files with the extension .nc and use those first.
    nc_file = grep("\\.nc$", matching_files)
    if (length(nc_file) > 0) {
      if (grepl("\\.nc$", in.prefix)) {
        out.file <- sub("\\.nc$", ".clim", in.prefix)
      } else {
        out.file <- paste0(in.prefix, ".clim")
        in.prefix <- paste0(in.prefix, ".nc")
      }
    } else { # no .nc files found... it could be that the extension was left off, or some other problem
      PEcAn.logger::logger.warn("No files found with extension '.nc'.  Using the first file in the list below:")
      PEcAn.logger::logger.warn(matching_files)
      in.prefix <- matching_files[1]
    }
  } else { # Default behavior
    out.file <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"),
                      strptime(end_date, "%Y-%m-%d"),
                      "clim",
                      sep = ".")
  }
  
  out.file.full <- file.path(outfolder, out.file)
  
  results <- data.frame(file = out.file.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/csv",
                        formatname = "Sipnet.climna",
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
  
  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  out <- NULL
  
  # get start/end year since inputs are specified on year basis
  # only if year.fragment = FALSE
  start_year <- lubridate::year(start_date)
  if (year.fragment) {
    end_year <- lubridate::year(start_date) # Start year is listed twice because there's only one file. start_year and end_year only control
    # the loop and file name, which are overriden for year.fragment
  } else {
    end_year <- lubridate::year(end_date)
  }
  
  ## loop over files
  for (year in start_year:end_year) {
    
    skip <- FALSE
    PEcAn.logger::logger.info(year)
    
    diy <- PEcAn.utils::days_in_year(year)
    
    if (!year.fragment) { # default behavior
      old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))
    } else { # Use the supplied file name
      old.file <- file.path(in.path, in.prefix)
    }
    
    if (file.exists(old.file)) {
      ## open netcdf
      nc <- ncdf4::nc_open(old.file)  
      
      ## convert time to seconds
      sec <- nc$dim$time$vals
      sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
      
      # Calculate the delta time.  If using whole-year data, the appropriate length in seconds is 
      # fetched; otherwise, it is assumed that the length of time provided in the time dimension of
      # the input file is correct.
      if (year.fragment) {
        dt <- mean(diff(sec), na.rm=TRUE)
        
      } else {
        dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
      }
      tstep <- round(86400 / dt)
      dt <- 86400 / tstep
      
      ## extract variables
      lat <- ncdf4::ncvar_get(nc, "latitude")
      lon <- ncdf4::ncvar_get(nc, "longitude")
      Tair <-ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
      Tair_C <- PEcAn.utils::ud_convert(Tair, "K", "degC")
      Qair <-ncdf4::ncvar_get(nc, "specific_humidity")  #humidity (kg/kg)
      ws <- try(ncdf4::ncvar_get(nc, "wind_speed"))
      if (!is.numeric(ws)) {
        U <- ncdf4::ncvar_get(nc, "eastward_wind")
        V <- ncdf4::ncvar_get(nc, "northward_wind")
        ws <- sqrt(U ^ 2 + V ^ 2)
        PEcAn.logger::logger.info("wind_speed absent; calculated from eastward_wind and northward_wind")
      }
      
      Rain <- ncdf4::ncvar_get(nc, "precipitation_flux")
      
      press <- ncdf4::ncvar_get(nc,'air_pressure') ## in pascal

      SW <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  ## in W/m2
      
      PAR <- try(ncdf4::ncvar_get(nc, "surface_downwelling_photosynthetic_photon_flux_in_air"))  ## in umol/m2/s
      if (!is.numeric(PAR)) {
        PAR <- PEcAn.utils::ud_convert(PEcAn.data.atmosphere::sw2ppfd(SW), "umol ", "mol")
        PEcAn.logger::logger.info("surface_downwelling_photosynthetic_photon_flux_in_air absent; PAR set to SW * 0.45")
      }
      
      soilT <- try(ncdf4::ncvar_get(nc, "soil_temperature"))
      if (!is.numeric(soilT)) {
        # approximation borrowed from SIPNET CRUNCEP preprocessing's tsoil.py
        tau <- 15 * tstep
        filt <- exp(-(1:length(Tair)) / tau)
        filt <- (filt / sum(filt))
        soilT <- convolve(Tair, filt)
        soilT <- PEcAn.utils::ud_convert(soilT, "K", "degC")
        PEcAn.logger::logger.info("soil_temperature absent; soilT approximated from Tair")
      } else {
        soilT <- PEcAn.utils::ud_convert(soilT, "K", "degC")
      }
      
      SVP <- PEcAn.utils::ud_convert(PEcAn.data.atmosphere::get.es(Tair_C), "millibar", "Pa")  ## Saturation vapor pressure
      VPD <- try(ncdf4::ncvar_get(nc, "water_vapor_saturation_deficit"))  ## in Pa
      if (!is.numeric(VPD)) {

        VPD <- SVP * (1 - PEcAn.data.atmosphere::qair2rh(Qair, Tair_C, press = press/100))

        PEcAn.logger::logger.info("water_vapor_saturation_deficit absent; VPD calculated from Qair, Tair, and SVP (saturation vapor pressure) ")
      }
      e_a <- SVP - VPD
      VPDsoil <- PEcAn.utils::ud_convert(PEcAn.data.atmosphere::get.es(soilT), "millibar", "Pa") *
        (1 - PEcAn.data.atmosphere::qair2rh(Qair, soilT, press/100))
      
      ncdf4::nc_close(nc)
    } else {
      PEcAn.logger::logger.info("Skipping to next year")
      next
    }
    
    ## build time variables (year, month, day of year)
    nyr <- floor(length(sec) / 86400 / 365 * dt)
    yr <- NULL
    doy <- NULL
    hr <- NULL
    asec <- sec
    for (y in year + 1:nyr - 1) {
      ytmp <- rep(y, diy * 86400 / dt)
      dtmp <- rep(seq_len(diy), each = 86400 / dt)
      if (is.null(yr)) {
        yr <- ytmp
        doy <- dtmp
        hr <- rep(NA, length(dtmp))
      } else {
        yr <- c(yr, ytmp)
        doy <- c(doy, dtmp)
        hr <- c(hr, rep(NA, length(dtmp)))
      }
      rng <- length(doy) - length(ytmp):1 + 1
      if (!all(rng >= 0)) {
        skip <- TRUE
        PEcAn.logger::logger.warn(paste(year, "is not a complete year and will not be included"))
        break
      }
      asec[rng] <- asec[rng] - asec[rng[1]]
      hr[rng] <- (asec[rng] - (dtmp - 1) * 86400) / 86400 * 24
    }
    if (length(yr) < length(sec)) {
      rng <- (length(yr) + 1):length(sec)
      if (!all(rng >= 0)) {
        skip <- TRUE
        PEcAn.logger::logger.warn(paste(year, "is not a complete year and will not be included"))
        break
      }
      yr[rng] <- rep(y + 1, length(rng))
      doy[rng] <- rep(1:366, each = 86400 / dt)[1:length(rng)]
      hr[rng] <- rep(seq(0, length = 86400 / dt, by = dt/86400 * 24), 366)[1:length(rng)]
    }
    if (skip) {
      PEcAn.logger::logger.info("Skipping to next year")
      next
    }
    
    ## 0 YEAR DAY HOUR TIMESTEP AirT SoilT PAR PRECIP VPD VPD_Soil AirVP(e_a) WIND SoilM build data
    ## matrix
    n <- length(Tair)
    tmp <- cbind(rep(0, n),
                 yr[1:n],
                 doy[1:n],
                 hr[1:n],
                 rep(dt / 86400, n),
                 Tair_C,
                 soilT,
                 PAR * dt,  # mol/m2/hr
                 Rain * dt, # converts from mm/s to mm
                 VPD,
                 VPDsoil,
                 e_a,
                 ws, # wind
                 rep(0.6, n)) # put soil water at a constant. Don't use, set SIPNET to MODEL_WATER = 1
    
    ## quick error check, sometimes get a NA in the last hr
    hr.na <- which(is.na(tmp[, 4]))
    if (length(hr.na) > 0) {
      tmp[hr.na, 4] <- tmp[hr.na - 1, 4] + dt/86400 * 24
    }
    
    ## filter out days not included in start or end date if not a year fragment. (This procedure would be nonsensible for a year
    ## fragment, as it would filter out all of the days.)
    if(year == start_year && !year.fragment){
      extra.days <- length(as.Date(paste0(start_year, "-01-01")):as.Date(start_date)) #extra days length includes the start date
      if (extra.days > 1){
        PEcAn.logger::logger.info("Subsetting SIPNET met to match start date")
        start.row <-  ((extra.days - 1) * 86400 / dt) + 1 #subtract to include start.date, add to exclude last half hour of day before
        tmp <- tmp[start.row:nrow(tmp),]
      }
    }
    if (year == end_year && !year.fragment){
      if(year == start_year){
        extra.days  <- length(as.Date(start_date):as.Date(end_date))
        if (extra.days > 1){
          PEcAn.logger::logger.info("Subsetting SIPNET met to match end date")
          end.row <-  extra.days * 86400 / dt  #subtract to include end.date
          tmp <- tmp[1:end.row,]
        }
      } else{
        extra.days <- length(as.Date(end_date):as.Date(paste0(end_year, "-12-31"))) #extra days length includes the end date
        if (extra.days > 1){
          PEcAn.logger::logger.info("Subsetting SIPNET met to match end date")
          end.row <-  nrow(tmp) - ((extra.days - 1) * 86400 / dt)  #subtract to include end.date
          tmp <- tmp[1:end.row,]
        }
      }
    }
 
  if(year.fragment){ #gets correct DOY for fragmented years
   
    doy.start.index <- which(doy == lubridate::yday(start_date))  #which part of full doy set matches the start and end date
    doy.end.index <- which(doy == lubridate::yday(end_date))
    #need to use the start and end time to figure out how many time steps to include in the doy subset 
    doy.start <- doy.start.index[ifelse(lubridate::hour(start_date) == 0, 1, lubridate::hour(start_date) / (24 / (86400 / dt)))] 
    doy.end <- doy.end.index[ifelse(lubridate::hour(end_date) == 0, 1, lubridate::hour(end_date) / (24 / (86400 / dt)))]
    #check to see if doy matches with downloaded data dims, if not last time is removed
    if(length(doy) != n){d<-doy[doy.start:(doy.end - 1)] }else{d<-(doy[doy.start:(doy.end)])  }
    
    if(year.fragment){ #gets correct DOY for fragmented years using start date, time since start date and end date
      doy.seq <- as.Date(seq(from = start_date + sec[1], to = end_date, length.out = length(sec)))
      doy <- as.numeric(strftime(doy.seq, format = "%j")) #starts with 1 on 1-01
      #doy.start <-  length(as.Date(paste0(start_year, "-01-01")):as.Date(start_date)) * (86400 / dt) + 1 #subtract to include start.date, add to exclude last half hour of day before
      #doy.end <-  length(as.Date(paste0(start_year, "-01-01")):as.Date(end_date)) * (86400 / dt)
      #doy <- doy[doy.start:doy.end]
      year <- as.numeric(strftime(doy.seq, format = "%Y"))
      tmp[,3] <- doy
      tmp[,2] <- year
    }
  }

    
    if (is.null(out)) {
      out <- tmp
    } else {
      out <- rbind(out, tmp)
    }
    
  }  ## end loop over years
  
  if (!is.null(out)) {
    
    ## write output
    utils::write.table(out, out.file.full, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
    return(invisible(results))
  } else {
    PEcAn.logger::logger.info("NO MET TO OUTPUT")
    return(invisible(NULL))
  }
} # met2model.SIPNET