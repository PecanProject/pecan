#-------------------------------------------------------------------------------
# Copyright (c) 2015 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into NetCDF CLM met files.

##' met2model wrapper for CLM45
##' 
##' @title met2model for CLM45
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param lst timezone offset to GMT in hours
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbosefor(year in start_year:end_year)
met2model.CLM45 <- function(in.path,in.prefix,outfolder,start_date, end_date, lst=0,lat,lon,..., overwrite=FALSE,verbose=FALSE){
  
  #General Structure- CLM Uses Netcdf so for now just need to rename vars.(Many not is CF standard. Need to Check that out)
  #Get Met file from inpath.
  #Loop over years (Open nc.file,rename vars,change dimensions as needed,close/save .nc file)
  #close
  
  if(!require("PEcAn.utils")) print("install PEcAn.utils")
  require("lubridate")
  require("ncdf4")
  require("udunits2")
  
  ## Open netcdf file
  nc=nc_open(met.file)
  
  #Reformat start and end dates 
  start_date<-as.POSIXlt(start.date,tz="GMT")
  end_date<-as.POSIXlt(end.date,tz ="GMT")
  
  days=as.Date(start_date):as.Date(end_date)
  year = strftime(as.Date(days,origin="1970-01-01"),"%Y")
  years<-unique(year)
  num.years<- length(years)
  timestep.s<-86400
  
  ## convert time to seconds
  sec   <- nc$dim$time$vals  
  sec = udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")
  
  ##build day and  year
  ifelse(leap_year(as.numeric(year))==TRUE,
         dt <- (366*24*60*60)/length(sec), #leap year
         dt <- (365*24*60*60)/length(sec)) #non-leap year
  tstep = 86400/dt
  
  doy <- rep(1:365,each=86400/dt)
  if(as.numeric(year) %% 4 == 0){  ## is leap
    doy <- rep(1:366,each=86400/dt)
  }
  
  
## loop over nc files and rename vars
for(year in start_year:end_year) {
  ncfile <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))

  
  ## extract variables
  
  ncvar_rename(nc,varid="LONGXY")
  ncvar_rename(nc,varid="LATIXY")
  #     double ZBOT(time, lat, lon) ;
  #     ZBOT:long_name = "observational height" ;
  #     ZBOT:units = "m" ;
  ZBOT = ncvar_rename(nc,"ZBOT","ZBOT")
  #     
  #     double EDGEW(scalar) ;
  #     EDGEW:long_name = "western edge in atmospheric data" ;
  #     EDGEW:units = "degrees E" ;
  EDGEW = ncvar_rename(nc,"EDGEW","EDGEW")
  
  #     double EDGEE(scalar) ;
  #     EDGEE:long_name = "eastern edge in atmospheric data" ;
  #     EDGEE:units = "degrees E" ;
  EDGEE = ncvar_rename(nc,"EDGEE","EDGEE")
  
  #     double EDGES(scalar) ;
  #     EDGES:long_name = "southern edge in atmospheric data" ;
  #     EDGES:units = "degrees N" ;
  EDGES = ncvar_rename(nc,"EDGES","EDGES") 
  #     
  #     double EDGEN(scalar) ;
  #     EDGEN:long_name = "northern edge in atmospheric data" ;
  #     EDGEN:units = "degrees N" ;
  EDGEN = ncvar_rename(nc,"EDGEN","EDGEN")
  #     double TBOT(time, lat, lon) ;
  #     TBOT:long_name = "temperature at the lowest atm level (TBOT)" ;
  #     TBOT:units = "K" ;
  TBOT  = ncvar_rename(nc,"TBOT","TBOT")
  #     double RH(time, lat, lon) ;   
  #     RH:long_name = "relative humidity at the lowest atm level (RH)" ;
  #     relative_humidity
  #     RH:units = "%" ;
  RH    = ncvar_rename(nc,"RH","relative_humidity")
  #     double WIND(time, lat, lon) ;
  #     WIND:long_name = "wind at the lowest atm level (WIND)" ;
  #     wind_speed
  #     WIND:units = "m/s" ;
  WIND  = ncvar_rename(nc,"WIND","wind_speed")
  #     double FSDS(time, lat, lon) ;
  #     FSDS:long_name = "incident solar (FSDS)" ;
  #     FSDS:units = "W/m2" ;
  FSDS  = ncvar_rename(nc,"FSDS","FSDS")
  #     double FLDS(time, lat, lon) ;
  #     FLDS:long_name = "incident longwave (FLDS)" ;
  #     FLDS:units = "W/m2" ;
  FLDS  = ncvar_rename(nc,"FLDS","")
  #     double PSRF(time, lat, lon) ;   
  #     PSRF:long_name = "pressure at the lowest atm level (PSRF)" ;
  #     PSRF:units = "Pa" ;
  PSRF  = ncvar_rename(nc,"PSRF","air_pressure")
  #     double PRECTmms(time, lat, lon) ;
  #     PRECTmms:long_name = "precipitation (PRECTmms)" ;
  #     PRECTmms:units = "mm/s" ;
  PRECTmms =ncvar_rename(nc,"PRECTmmc","precipitation_flux")
  


} ### end loop over met files

print("Done with met2model.CLM4")

} ### end met2model.CLM4

