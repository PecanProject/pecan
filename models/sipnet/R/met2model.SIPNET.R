#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into SIPNET met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and 
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change 
##overwrite statement below to TRUE.

##' @export
##' @param start_year first year to be converted
##' @param end_year last year to be converted
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param overwrite should existing files be overwritten
met2model.SIPNET <- function(start_year, end_year, in.path, in.prefix, outfolder,overwrite=FALSE,verbose=FALSE){
  
  out.file = file.path(outfolder, paste(in.prefix, start_year, end_year, "clim", sep="."))
  results <- data.frame(file=c(out.file),
                        host=c(fqdn()),
                        mimetype=c('text/csv'),
                        formatname=c('Sipnet.climna'),
                        startdate=c(paste0(start_year,"-01-01 00:00:00")),
                        enddate=c(paste0(end_year,"-12-31 23:59:59")))
  
  
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

## loop over files
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
  Tair <- ncvar_get(nc,"air_temperature")
  Qair <- ncvar_get(nc,"specific_humidity")  #humidity (kg/kg)
  U <- ncvar_get(nc,"eastward_wind")
  V <- ncvar_get(nc,"northward_wind")
  Rain <- ncvar_get(nc,"precipitation_flux")
  pres <- ncvar_get(nc,"air_pressure")
  SW   <- ncvar_get(nc,"surface_downwelling_shortwave_flux_in_air")

  PAR  <- try(ncvar_get(nc,"surface_downwelling_photosynthetic_photon_flux_in_air"))
  if(!is.numeric(PAR)) PAR = SW*0.45 

  soilT <- try(ncvar_get(nc,"soil_temperature"))
  if(!is.numeric(soilT)){
    #approximation borrowed from SIPNET CRUNCEPpreprocessing's tsoil.py
    tau = 15.0*tstep
    filt = exp(-(1:length(Tair))/tau)
    filt = (filt/sum(filt))
    soilT = convolve(Tair, filt) - 273.15
  }

  SVP = ud.convert(get.es(Tair-273.15),"millibar","Pa") ## Saturation vapor pressure
  VPD <- try(ncvar_get(nc,"water_vapor_saturation_deficit"))
  if(!is.numeric(VPD)){
    VPD = SVP*(1-qair2rh(Qair,Tair-273.15))
  }
  e_a = SVP - VPD
  VPDsoil = ud.convert(get.es(soilT),"millibar","Pa")*(1-qair2rh(Qair,soilT))

  nc_close(nc)
   
  ##build time variables (year, month, day of year)
  nyr <- floor(length(sec)/86400/365*dt)
  yr <- NULL
  doy <- NULL
  hr <- NULL
  asec <- sec
  for(y in year+1:nyr-1){
    ytmp <- rep(y,365*86400/dt)
    dtmp <- rep(1:365,each=86400/dt)
    if(y %% 4 == 0){  ## is leap
      ytmp <- rep(y,366*86400/dt)
      dtmp <- rep(1:366,each=86400/dt)
    }
    if(is.null(yr)){
      yr <- ytmp
      doy <- dtmp
      hr <- rep(NA,length(dtmp))
    } else {
      yr <- c(yr,ytmp)
      doy <- c(doy,dtmp)
      hr <- c(hr,rep(NA,length(dtmp)))
    }
    rng <- length(doy) - length(ytmp):1 + 1
    asec[rng] <- asec[rng] - asec[rng[1]]
    hr[rng] <- (asec[rng] - (dtmp-1)*86400)/86400*24
  }
  if(length(yr) < length(sec)){
    rng <- (length(yr)+1):length(sec)
    yr[rng] <- rep(y+1,length(rng))
    doy[rng] <- rep(1:366,each=86400/dt)[1:length(rng)]
    hr[rng] <- rep(seq(0,length=86400/dt,by=dt/86400*24),366)[1:length(rng)]
  }

##0 YEAR DAY HOUR TIMESTEP AirT SoilT PAR PRECIP VPD VPD_Soil AirVP(e_a) WIND SoilM   
  ## build data matrix
  n = length(Tair)
  tmp <- cbind(rep(0,n),yr,doy,hr,rep(dt/86400,n),
               Tair-273.15,
               soilT, 
               par2ppfd(PAR), #converts to mol/m2
               Rain*dt, ## converts from mm/s to mm
               VPD,
               VPDsoil,
               e_a,
               sqrt(U^2+V^2), ## wind
               rep(0.6,n) ## put soil water at a constant. Don't use, set SIPNET to MODEL_WATER = 1
               )

  if(is.null(out)){
    out = tmp
  } else {
    out = rbind(out,tmp)
  }

} ## end loop over years

## write output
write.table(out,out.file,quote = FALSE,sep="\t",row.names=FALSE,col.names=FALSE)

  invisible(results)


} ### end met2model.SIPNET
