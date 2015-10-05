#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into PRELES met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and 
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change 
##overwrite statement below to TRUE.

##' met2model for PRELES
##'
##' @title met2model.PRELES
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
met2model.PRELES <- function(in.path, in.prefix, outfolder, start_date, end_date, ..., overwrite=FALSE,verbose=FALSE){
  
  ## PRELES 1 driver format (.csv):
  ## Runday,  Min temp (°C), Max temp (°C), Radiation (MJ d-1), Atmospheric CO2 (μmol mol-1), Day of year
  
  ## PRELES EnKF (Quaife) format (.dat, space delimited):
  ## The nine columns of driving data are: day of year; mean air temperature (deg C); max daily temperature (deg C); min daily temperature (deg C); incident radiation (MJ/m2/day); maximum soil-leaf water potential difference (MPa); atmospheric carbon dioxide concentration (ppm); total plant-soil hydraulic resistance (MPa.m2.s/mmol-1); average foliar nitorgen (gC/m2 leaf area).
  ## Calculate these from air_temperature (K), surface_downwelling_shortwave_flux_in_air (W/m2), CO2 (ppm)
  
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
                        formatname=c('PRELES meteorology'),
                        startdate=c(start_date),
                        enddate=c(end_date),
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  print("internal results")
  print(results)
  
  if (file.exists(out.file.full) && !overwrite) {
    logger.debug("File '", out.file.full, "' already exists, skipping to next file.")
    return(invisible(results))
  }
  
  require(ncdf4)
  require(lubridate)
  require(PEcAn.data.atmosphere)
  #  require(ncdf)
  
  ## check to see if the outfolder is defined, if not create directory for output
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  out <- NULL
  
  # get start/end year since inputs are specified on year basis
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  ## loop over files
  # TODO need to filter out the data that is not inside start_date, end_date
  for(year in start_year:end_year) {
    print(year)
    
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))
    
    ## open netcdf
    nc <- nc_open(old.file)
    
    ## convert time to seconds
    sec   <- nc$dim$time$vals  
    sec = udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")
    
    ifelse(leap_year(year)==TRUE,
           dt <- (366*24*60*60)/length(sec), #leap year
           dt <- (365*24*60*60)/length(sec)) #non-leap year
    tstep = 86400/dt
    
    ## extract variables
    lat    <- ncvar_get(nc,"latitude")
    lon    <- ncvar_get(nc,"longitude")
    PAR    <- ncvar_get(nc, "surface_downwelling_photosynthetic_photon_flux_in_air") ## mol/m2s1
    Tair   <- ncvar_get(nc,"air_temperature")  ## in Kelvin
    Precip <- ncvar_get(nc, "precipitation_flux") ## kg/m2
    VPD    <- ncvar_get(nc, "water_vapor_saturation_deficit") ## Pa
    SW     <- ncvar_get(nc,"surface_downwelling_shortwave_flux_in_air") ## in W/m2
    CO2    <- try(ncvar_get(nc,"mole_fraction_of_carbon_dioxide_in_air")) ## mole fraction of carbon dioxide mol/mol

    nc_close(nc)
    
    useCO2 = is.numeric(CO2)  
    if(useCO2)  CO2 <- CO2/1e6  ## convert from mole fraction (kg/kg) to ppm
    
    
    ## is CO2 present?
    if(!is.numeric(CO2)){
      logger.warn("CO2 not found in",old.file,"setting to default: 400 ppm")
      CO2 = rep(400,length(Tair))
    }
    
    ##build day of year
    doy <- rep(1:365,each=86400/dt)
    if(year %% 4 == 0){  ## is leap
      doy <- rep(1:366,each=86400/dt)
    }
    
    ## Aggregate variables up to daily
    TAir   = udunits2::ud.convert(tapply(Tair,doy,mean,na.rm=TRUE),"Kelvin","Celsius")
    PAR    = tapply(PAR,doy,sum,na.rm=TRUE)
    Precip = tapply(Precip, doy,sum, na.rm=TRUE)
    VPD    = udunits2::ud.convert(tapply(VPD,doy,mean,na.rm=TRUE),"Pa","kPa")
    CO2    = tapply(CO2,doy,mean)
    doy    = tapply(doy,doy,mean)
    fAPAR  = rep(0.8,length=length(doy))
    
    ## The nine columns of driving data are: Photosynthetically active radiation mol/m2/day,mean air temperature (deg C);Mean vapour pressure deficit kPa; Precipitatin above Canopy mm; atmospheric carbon dioxide concentration (ppm)
    
    ## build data matrix
    tmp <- cbind(PAR,
                 TAir,
                 VPD,
                 Precip,
                 CO2,
                 fAPAR
                 )
    
    if(is.null(out)){
      out = tmp
    } else {
      out = rbind(out,tmp)
    }
    
  } ## end loop over years
  
  
}
