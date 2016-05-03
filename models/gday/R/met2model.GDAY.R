#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into GDAY met files

##If files already exist in "Outfolder", the default function is NOT to
## overwrite them and only gives user the notice that file already exists. If
## user wants to overwrite the existing files, just change overwrite statement
## below to TRUE.

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
met2model.GDAY <- function(in.path, in.prefix, outfolder, start_date,
                           end_date, ..., overwrite=FALSE,verbose=FALSE){

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
  SEC_TO_HFHR <- 60.0 * 30.0
  K_TO_DEG <- -273.15

  if(!require(PEcAn.utils)) print("install PEcAn.utils")

  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date<- as.POSIXlt(end_date, tz = "GMT")
  out.file <- paste(in.prefix,
                    strptime(start_date, "%Y-%m-%d"),
                    strptime(end_date, "%Y-%m-%d"),
                    "dat", sep=".")
  out.file.full <- file.path(outfolder, out.file)

  results <- data.frame(file=c(out.file.full),
                        host=c(fqdn()),
                        mimetype=c('text/plain'),
                        formatname=c('GDAY meteorology'),
                        startdate=c(start_date),
                        enddate=c(end_date),
                        dbfile.name=out.file,
                        stringsAsFactors=FALSE)
  print("internal results")
  print(results)

  if (file.exists(out.file.full) && !overwrite) {
    logger.debug("File '", out.file.full,
                 "' already exists, skipping to next file.")
    return(invisible(results))
  }

  require(ncdf4)
  require(lubridate)
  require(PEcAn.data.atmosphere)
  #  require(ncdf)

  ## check to see if the outfolder is defined, if not create directory for
  ## output
  if (!file.exists(outfolder)){
    dir.create(outfolder)
  }

  out <- NULL

  # get start/end year since inputs are specified on year basis
  start_year <- year(start_date)
  end_year <- year(end_date)

  ## loop over files
  # TODO need to filter out the data that is not inside start_date, end_date
  for (year in start_year:end_year) {
    print(year)
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))

    ## open netcdf
    nc <- nc_open(old.file)

    ## convert time to seconds
    sec   <- nc$dim$time$vals
    sec = udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],
                               "seconds")
    timestep.s=86400 #seconds in a day
    ifelse(leap_year(year)==TRUE,
           dt <- (366*24*60*60)/length(sec), #leap year
           dt <- (365*24*60*60)/length(sec)) #non-leap year
    tstep = round(timestep.s/dt)
    dt = timestep.s/tstep #dt is now an integer

    ## extract variables
    lat  <- ncvar_get(nc, "latitude")
    lon  <- ncvar_get(nc, "longitude")
    Tair <- ncvar_get(nc, "air_temperature")  ## in Kelvin
    SW   <- ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air") ##in W/m2
    CO2  <- try(ncvar_get(nc, "mole_fraction_of_carbon_dioxide_in_air"))
    SH  <- try(ncvar_get(nc, "specific_humidity")) ## kg/kg
    wind_speed  <- try(ncvar_get(nc, "wind_speed")) ## m/s
    air_pressure <- try(ncvar_get(nc, "air_pressure")) ## Pa
    ppt <- try(ncvar_get(nc, "precipitation_flux")) ## kg/m2/s

    nc_close(nc)

    useCO2 = is.numeric(CO2)
    if(useCO2)  CO2 <- CO2 * 1e6  ## convert from mole fraction (kg/kg) to ppm

    ## is CO2 present?
    if (!is.numeric(CO2)){
      logger.warn("CO2 not found in",old.file,"setting to default: 400 ppm")
      CO2 = rep(400,length(Tair))
    }


    ## For now setting this to be always true till I figure out how to
    ## interface with the sub_daily param file. Should detech if met-data
    ## is coarser than 30-min and swapped to day version?
    sub_daily = TRUE

    if (sub_daily) {

      if(year %% 4 == 0) {
        ndays <= 366
      } else {
        ndays <- 365
      }
      idx = 0
      for (doy in 1:ndays) {

        ## If there is no Tsoil variabile use Tair...it doesn't look like Tsoil
        ## is a standard input
        tsoil = mean(tair[idx:idx+48] + K_TO_DEG)
        for (hod in 1:48) {

          rain = ppt[idx] * SEC_TO_HFHR
          par = SW[idx] * SW_2_PAR
          tair = Tair[idx] + K_TO_DEG
          wind = wind_speed[idx]
          press = air_pressure[idx] * PA_2_KPA
          rh = qair2rh(SH[idx], Tair[idx])
          vpd = get.vpd(rh[idx], Tair[idx])
          co2 = CO2[idx]

          # This is an assumption of the Medlyn gs model
          if (vpd < 0.05) {
            vpd = 0.05
          }

          ## No NDEP, so N-cycle will have to be switched off by default
          ndep = -999.9

          idx <- idx + 1
        } ## Hour of day loop

        ## build data matrix
        tmp <- cbind(year,
                     doy,
                     hod,
                     rain,
                     par,
                     tair,
                     tsoil,
                     vpd,
                     CO2,
                     ndep,
                     wind,
                     press)

        if (is.null(out)) {
          out = tmp
        } else {
          out = rbind(out, tmp)
        }
      } ## Day of year loop

    } else {

      idx = 0
      if(year %% 4 == 0) {
        ndays <= 366
      } else {
        ndays <- 365
      }
      for (doy in 1:ndays) {

        ## Needs to be daylight hours...how do we access sun up/down
        tair = ?
        rain = sum(ppt[idx:idx+48] * SEC_TO_HFHR)

        ## If there is no Tsoil variabile use Tair...it doesn't look like Tsoil
        ## is a standard input
        tsoil = mean(tair[idx:idx+48] + K_TO_DEG)

        ## Needs to be AM/PM
        tam = ?
        tpm = ?

        tmin = min(tair[idx:idx+48] + K_TO_DEG)
        tmax = max(tair[idx:idx+48] + K_TO_DEG)
        tday = mean(tair[idx:idx+48] + K_TO_DEG)

        # Needs to be AM/PM
        vpd_am = ?
        # This is an assumption of the Medlyn gs model
        if (vpd_am < 0.05) {
          vpd_am = 0.05
        }

        vpd_pm = ?
        # This is an assumption of the Medlyn gs model
        if (vpd_pm < 0.05) {
          vpd_pm = 0.05
        }
        co2 = mean(CO2[idx:idx+48])

        ## No NDEP, so N-cycle will have to be switched off by default
        ndep = -999.9

        wind = mean(wind_speed[idx:idx+48])
        press = mean(air_pressure[idx:idx+48] * PA_2_KPA)

        # Needs to be AM/PM
        wind_am = ?
        wind_pm = ?
        par_am = ?
        par_pm = ?
      }

      ## build data matrix
      tmp <- cbind(year,
                   doy,
                   tair,
                   rain,
                   tsoil,
                   tam,
                   tpm,
                   tmin,
                   tmax,
                   tday,
                   vpd_am,
                   vpd_pm,
                   CO2,
                   ndep,
                   wind,
                   press,
                   wind_am,
                   wind_pm,
                   par_am,
                   par_pm)

      if (is.null(out)) {
        out = tmp
      } else {
        out = rbind(out,tmp)
      }
    } ## end sub-daily/day if/else block
  } ## end loop over years

  ## write output
  write.table(out, out.file.full, quote=FALSE,sep=" ", row.names=FALSE,
              col.names=FALSE)

  invisible(results)


}
