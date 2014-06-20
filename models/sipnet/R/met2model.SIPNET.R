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


met2model.SIPNET <- function(in.path,in.prefix,outfolder,overwrite=FALSE){
  files = dir(in.path,in.prefix,full.names=TRUE)
  filescount = files[grep(pattern="*.nc",files)]
  
  require(ncdf4)
#  require(ncdf)

## check to see if the outfolder is defined, if not create directory for output
if(!file.exists(outfolder)){
  dir.create(outfolder)
}

out <- NULL

## loop over files
for(i in 1:length(filescount)){
  
  ## open netcdf
  nc <- nc_open(files[i])
  
  ## determine starting year
  base.time <- unlist(strsplit(files[i],'[.]'))
  base.time <- as.numeric(base.time[length(base.time)-1])
  if(is.na(base.time)){
      print(c("did not extract base time correctly",i))
      break
    }
  
##0 YEAR DAY HOUR TIMESTEP AirT SoilT PAR PRECIP VPD VPD_Soil AirVP(e_a) WIND SoilM
  
  ## extract variables
  lat  <- ncvar_get(nc,"lat")
  lon  <- ncvar_get(nc,"lon")
  sec   <- nc$dim$t$vals
  Tair <- ncvar_get(nc,"air_temperature")
  Qair <- ncvar_get(nc,"specific_humidity")  #humidity (kg/kg)
  U <- ncvar_get(nc,"eastward_wind")
  V <- ncvar_get(nc,"northward_wind")
  Rain <- ncvar_get(nc,"precipitation_flux")
  pres <- ncvar_get(nc,"air_pressure")
  SW   <- ncvar_get(nc,"surface_downwelling_shortwave_flux")
  PAR  <- try(ncvar_get(nc,"surface_downwelling_photosynthetic_photon_flux_in_air"))
  if(!is.numeric(PAR)) PAR = SW*0.43 

  ## convert time to seconds
  sec = udunits2::ud.convert(sec,unlist(strsplit(nc$dim$t$units," "))[1],"seconds")
  
  nc_close(nc)
  
  dt <- sec[2]-sec[1]
  toff <- -lst*3600/dt
   
  ##build time variables (year, month, day of year)
  nyr <- floor(length(sec)/86400/365*dt)
  yr <- NULL
  doy <- NULL
  hr <- NULL
  asec <- sec
  for(y in base.time+1:nyr-1){
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
  tmp <- cbind(rep(0,n),yr,doy,hr,
               Tair,
               Tair, #this should be soil T
               
               )

} ## end loop over years

## write output


} ### end met2model.SIPNET
