#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# R Code to convert NetCDF CF met files into GDAY met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model for GDAY
##'
##' @title met2model.GDAY
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will
##'        only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use
##'        the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##'
##' @author Martin De Kauwe
##' @importFrom ncdf4 ncvar_get
##' @importFrom ncdf4 ncdim_def
##' @importFrom ncdf4 ncatt_get
met2model.GDAY <- function(in.path, in.prefix, outfolder, start_date, end_date, 
                           overwrite = FALSE, verbose = FALSE, ...) {
  
  ## GDAY driver format (.csv):
  ## 30min: year (-), doy (-; NB. leap years), hod (-), rainfall (mm 30 min-1),
  ##        par (umol m-2 s-1), tair (deg C), tsoil (deg C), vpd (kPa),
  ##        co2 (ppm), ndep (t ha-1 30 min-1), wind (m-2 s-1), press (kPa)
  ##
  ## Daily:
  ## 30min: year (-), doy (-; NB. leap years), tair (deg C),
  ##        rainfall (mm day-1), tsoil (deg C), tam (deg C), tpm (deg C),
  ##        tmin (deg C), tmax (deg C), tday (deg C), vpd_am (kPa),
  ##        vpd_pm (kPa), co2 (ppm), ndep (t ha-1 day-1), wind (m-2 s-1),
  ##        press (kPa), wind_am (m-2 s-1), wind_pm (m-2 s-1),
  ##        par_am (umol m-2 s-1), par_pm (umol m-2 s-1)
  
  SW_2_PAR <- 2.3
  PA_2_KPA <- 0.001
  SEC_TO_HFHR <- 60 * 30
  K_TO_DEG <- -273.15
  MOL_2_UMOL <- 1e+06
  
  library(PEcAn.utils)
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  out.file <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"), 
                    strptime(end_date, "%Y-%m-%d"), 
                    "dat", sep = ".")
  out.file.full <- file.path(outfolder, out.file)
  
  results <- data.frame(file = c(out.file.full), 
                        host = c(fqdn()), 
                        mimetype = c("text/plain"), 
                        formatname = c("GDAY meteorology"), 
                        startdate = c(start_date), 
                        enddate = c(end_date), 
                        dbfile.name = out.file, 
                        stringsAsFactors = FALSE)
  
  if (file.exists(out.file.full) && !overwrite) {
    logger.debug("File '", out.file.full, "' already exists, skipping to next file.")
    return(invisible(results))
  }
  
  library(PEcAn.data.atmosphere)
  
  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  ## For now setting this to be false till we resolve the best route forward
  sub_daily <- FALSE
  
  # Create an empty holder for each (hour)days translated met file
  out <- NULL

  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  for (year in start_year:end_year) {
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))
    
    nc <- ncdf4::nc_open(old.file)
    
    ## extract variables
    lat  <- ncvar_get(nc, "latitude")
    lon  <- ncvar_get(nc, "longitude")
    Tair <- ncvar_get(nc, "air_temperature")  ## in Kelvin
    Tair <- Tair + K_TO_DEG
    PAR  <- try(ncvar_get(nc, "surface_downwelling_photosynthetic_photon_flux_in_air"))
    PAR  <- PAR * MOL_2_UMOL
    if (!is.numeric(PAR)) {
      SW <- ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  ##in W/m2
      PAR <- SW * SW_2_PAR
    }
    CO2 <- try(ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"))
    SH  <- try(ncvar_get(nc, "specific_humidity"))  ## kg/kg
    VPD <- try(ncvar_get(nc, "water_vapor_saturation_deficit"))  ## Pa
    if (!is.numeric(VPD)) {
      RH  <- qair2rh(SH, Tair)
      VPD <- get.vpd(RH, Tair) * MB_2_KPA
    } else {
      VPD <- VPD * PA_2_KPA
    }
    
    wind_speed <- try(ncvar_get(nc, "wind_speed"))  ## m/s
    air_pressure <- try(ncvar_get(nc, "air_pressure"))  ## Pa
    ppt <- try(ncvar_get(nc, "precipitation_flux"))  ## kg/m2/s
    
    ncdf4::nc_close(nc)
    
    ## is CO2 present?
    if (!is.numeric(CO2)) {
      logger.warn("CO2 not found in", old.file, "setting to default: 400 ppm")
      CO2 <- rep(400, length(Tair))
    } else {
      CO2 <- CO2 * MOL_2_UMOL
    }
    
    if (sub_daily) {
      
      if (lubridate::leap_year(year)) {
        ndays <- 366
      } else {
        ndays <- 365
      }
      
      for (doy in seq_len(ndays)) {
        
        day_idx <- idx:((idx - 1) + 48)
        
        # Grab the days data
        tair_day  <- Tair[day_idx]
        rain_day  <- ppt[day_idx]
        par_day   <- PAR[day_idx]
        vpd_day   <- VPD[day_idx]
        wind_day  <- wind_speed[day_idx]
        press_day <- air_pressure[day_idx] * PA_2_KPA
        co2_day   <- CO2[day_idx]
        
        ## If there is no Tsoil variabile use Tair...it doesn't look like Tsoil is a standard input
        tsoil_out <- mean(tair_day)
        
        for (hod in 1:48) {
          
          rain_out  <- rain_day[hod] * SEC_TO_HFHR
          par_out   <- par_day[hod]
          tair_out  <- tair_day[hod]
          wind_out  <- wind_day[hod]
          press_out <- press_day[hod]
          vpd_out   <- vpd_day[hod]
          co2_out   <- co2_day[hod]
          
          # This is an assumption of the Medlyn gs model
          if (vpd_out < 0.05) {
            vpd_out <- 0.05
          }
          
          ## No NDEP, so N-cycle will have to be switched off by default
          ndep_out <- -999.9
          
          ## build output data matrix
          tmp <- cbind(year, doy, hod, 
                       rain_out, par_out, tair_out, tsoil_out, vpd_out, 
                       co2_out, ndep_out, wind_out, press_out)
          
          if (is.null(out)) {
            out <- tmp
          } else {
            out <- rbind(out, tmp)
          }
          
          idx <- idx + 48
        }  ## Hour of day loop
      }  ## Day of year loop
      
    } else {
      
      if (lubridate::leap_year(year)) {
        ndays <- 366
      } else {
        ndays <- 365
      }
      
      for (doy in 1:ndays) {
        
        # Build day, morning and afternoon indicies
        day_idx <- idx:((idx - 1) + 48)
        mor_idx <- 1:24
        eve_idx <- 25:48
        
        # Grab the days data
        tair_day <- Tair[day_idx]
        par_day  <- PAR[day_idx]
        vpd_day  <- VPD[day_idx]
        wind_day <- wind_speed[day_idx]
        
        # Calculate the output met vars
        tair_out <- mean(tair_day)
        rain_out <- sum(ppt[day_idx] * SEC_TO_HFHR)
        
        ## If there is no Tsoil variabile use Tair...it doesn't look like Tsoil is a standard input
        tsoil_out <- mean(tair_day)
        
        ## No NDEP, so N-cycle will have to be switched off by default
        ndep_out <- -999.9
        
        co2_out   <- mean(CO2[day_idx])
        wind_out  <- mean(wind_speed[day_idx])
        press_out <- mean(air_pressure[day_idx] * PA_2_KPA)
        
        ## Needs to be morning (am) / afternoon (pm)
        tair_am_out <- mean(tair_day[mor_idx][par_day[mor_idx] > 0])
        tair_pm_out <- mean(tair_day[eve_idx][par_day[eve_idx] > 0])
        tmin_out    <- min(tair_day)
        tmax_out    <- max(tair_day)
        tday_out    <- mean(tair_day)
        
        vpd_am_out <- mean(vpd_day[mor_idx][par_day[mor_idx] > 0])
        # This is an assumption of the Medlyn gs model
        if (vpd_am_out < 0.05) {
          vpd_am_out <- 0.05
        }
        
        vpd_pm_out <- mean(vpd_day[eve_idx][par_day[eve_idx] > 0])
        # This is an assumption of the Medlyn gs model
        if (vpd_pm_out < 0.05) {
          vpd_pm_out <- 0.05
        }
        
        wind_am_out <- mean(wind_day[mor_idx][par_day[mor_idx] > 0])
        wind_pm_out <- mean(wind_day[eve_idx][par_day[eve_idx] > 0])
        par_am_out <- sum(par_day[mor_idx][par_day[mor_idx] > 0])
        par_pm_out <- sum(par_day[eve_idx][par_day[eve_idx] > 0])
        
        ## build output data matrix
        tmp <- cbind(year, doy, tair_out, rain_out, tsoil_out, tair_am_out, tair_pm_out, 
                     tmin_out, tmax_out, tday_out, vpd_am_out, vpd_pm_out, co2_out, ndep_out, wind_out, 
                     press_out, wind_am_out, wind_pm_out, par_am_out, par_pm_out)
        
        if (is.null(out)) {
          out <- tmp
        } else {
          out <- rbind(out, tmp)
        }
        
        idx <- idx + 1
      }  ## end of day loop
      
    }  ## end sub-daily/day if/else block
  }  ## end loop over years
  
  ## write output
  write.table(out, out.file.full, quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  
  return(invisible(results))
} # met2model.GDAY
