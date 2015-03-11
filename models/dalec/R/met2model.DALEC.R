#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into DALEC met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and 
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change 
##overwrite statement below to TRUE.

##' met2model for DALEC
##'
##' @title met2model.DALEC
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
met2model.DALEC <- function(in.path, in.prefix, outfolder, start_date, end_date, overwrite=FALSE,verbose=FALSE,...){
  
  ## DALEC 1 driver format (.csv):
  ## Runday,  Min temp (°C), Max temp (°C), Radiation (MJ d-1), Atmospheric CO2 (μmol mol-1), Day of year
  
  ## DALEC EnKF (Quaife) format (.dat, space delimited):
  ## The nine columns of driving data are: day of year; mean air temperature (deg C); max daily temperature (deg C); min daily temperature (deg C); incident radiation (MJ/m2/day); maximum soil-leaf water potential difference (MPa); atmospheric carbon dioxide concentration (ppm); total plant-soil hydraulic resistance (MPa.m2.s/mmol-1); average foliar nitorgen (gC/m2 leaf area).
  ## Calculate these from air_temperature (K), surface_downwelling_shortwave_flux_in_air (W/m2), CO2 (ppm)
  
  ## Assuming default values for leaf water potential, hydraulic resistance, foliar N
  leafN = 2.5
  HydResist = 1
  LeafWaterPot = -0.8
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date<- as.POSIXlt(end_date, tz = "GMT")
  out.file <- file.path(outfolder, paste(in.prefix,
                                         strptime(start_date, "%Y-%m-%d"),
                                         strptime(end_date, "%Y-%m-%d"),
                                         "dat", sep="."))
  
  results <- data.frame(file=c(out.file),
                        host=c(fqdn()),
                        mimetype=c('text/plain'),
                        formatname=c('DALEC meteorology'),
                        startdate=c(start_date),
                        enddate=c(end_date))
  
  
  if (file.exists(out.file) && !overwrite) {
    logger.debug("File '", out.file, "' already exists, skipping to next file.")
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
    lat  <- ncvar_get(nc,"latitude")
    lon  <- ncvar_get(nc,"longitude")
    Tair <- ncvar_get(nc,"air_temperature")  ## in Kelvin
    SW   <- ncvar_get(nc,"surface_downwelling_shortwave_flux_in_air") ## in W/m2
    CO2  <- try(ncvar_get(nc,"mole_fraction_of_carbon_dioxide_in_air"))
    nc_close(nc)
    
    useCO2 = is.numeric(CO2)  
    if(useCO2)  CO2 <- CO2/1e6  ## convert from mole fraction (kg/kg) to ppm
    
    
    ## is CO2 present?
    if(!is.numeric(CO2)){
      logger.warn("CO2 not found in",old.file,"setting to default: 400 ppm")
      CO2 = rep(400,length(Tair))
    }
    
    if(length(leafN) == 1){
      logger.warn("Leaf N not specified, setting to default: ",leafN)
      leafN = rep(leafN,length(Tair))
    }  
    if(length(HydResist)==1){
      logger.warn("total plant-soil hydraulic resistance (MPa.m2.s/mmol-1) not specified, setting to default: ",HydResist)
      HydResist = rep(HydResist,length(Tair))
    }
    if(length(LeafWaterPot)==1){
      logger.warn("maximum soil-leaf water potential difference (MPa) not specified, setting to default: ",LeafWaterPot)
      LeafWaterPot = rep(LeafWaterPot,length(Tair))
    }
    
    ##build day of year
    doy <- rep(1:365,each=86400/dt)
    if(year %% 4 == 0){  ## is leap
      doy <- rep(1:366,each=86400/dt)
    }
    
    ## Aggregate variables up to daily
    Tmean = udunits2::ud.convert(tapply(Tair,doy,mean,na.rm=TRUE),"Kelvin","Celsius")
    Tmin  = udunits2::ud.convert(tapply(Tair,doy,min,na.rm=TRUE),"Kelvin","Celsius")
    Tmax  = udunits2::ud.convert(tapply(Tair,doy,max,na.rm=TRUE),"Kelvin","Celsius")
    Rin   = tapply(SW,doy,sum)*dt*1e-6 # J/m2/s * s * MJ/J
    LeafWaterPot = tapply(LeafWaterPot,doy,mean)
    CO2   = tapply(CO2,doy,mean)
    HydResist = tapply(HydResist,doy,mean)
    leafN = tapply(leafN,doy,mean)
    doy   = tapply(doy,doy,mean)
    
    ## The nine columns of driving data are: day of year; mean air temperature (deg C); max daily temperature (deg C); min daily temperature (deg C); incident radiation (MJ/m2/day); maximum soil-leaf water potential difference (MPa); atmospheric carbon dioxide concentration (ppm); total plant-soil hydraulic resistance (MPa.m2.s/mmol-1); average foliar nitorgen (gC/m2 leaf area).
    
    ## build data matrix
    tmp <- cbind(doy,
                 Tmean,
                 Tmax,
                 Tmin,
                 Rin,
                 LeafWaterPot,
                 CO2,
                 HydResist,
                 leafN)
    
    if(is.null(out)){
      out = tmp
    } else {
      out = rbind(out,tmp)
    }
    
  } ## end loop over years
  
  ## write output
  write.table(out,out.file,quote = FALSE,sep=" ",row.names=FALSE,col.names=FALSE)
  
  invisible(results)
  
  
}
