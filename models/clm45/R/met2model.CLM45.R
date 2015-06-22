#-------------------------------------------------------------------------------
# Copyright (c) 2015 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


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
##' @param verbose should the function be very verbose
met2model.CLM45 <- function(in.path,in.prefix,outfolder,start_date, end_date, lst=0,lat,lon,..., overwrite=FALSE,verbose=FALSE){
  overwrite = as.logical(overwrite)
  
  require(ncdf4)
#  require(lubridate)
  require(PEcAn.utils)

  # results are stored in folder prefix.start.end
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  met_folder <- outfolder

  results <- data.frame(file=c(met_header),
                        host=c(fqdn()),
                        mimetype=c('application/x-netcdf'),
                        formatname=c('CLM met'),
                        startdate=c(start_date),
                        enddate=c(end_date),
                        dbfile.name = paste("CLM met:",in.prefix),
                        stringsAsFactors = FALSE)

  ## check to see if the outfolder is defined, if not create directory for output
  dir.create(met_folder, recursive=TRUE, showWarnings = FALSE)
  
### FUNCTIONS
dm <- c(0,32,60,91,121,152,182,213,244,274,305,335,366)
dl <- c(0,32,61,92,122,153,183,214,245,275,306,336,367)
month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
mon_num <- c("01","02","03","04","05","06","07","08","09","10","11","12")
day2mo <- function(year,day){
  leap <- (year %% 4 == 0)
  mo <- rep(NA,length(day))
  mo[leap] <- findInterval(day[leap],dl)
  mo[!leap] <- findInterval(day[!leap],dm)
  return(mo)
}

# get start/end year since inputs are specified on year basis
start_year <- year(start_date)
end_year <- year(end_date)

## loop over files
for(year in start_year:end_year) {
  ncfile <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))

  ## open netcdf
  nc <- nc_open(ncfile)
  
  ## extract variables
  lat  <- eval(parse(text = lat))
  lon  <- eval(parse(text = lon))
  sec   <- nc$dim$time$vals
  Tair <- ncvar_get(nc,"air_temperature")
  Qair <- ncvar_get(nc,"specific_humidity")  #humidity (kg/kg)
  U <- ncvar_get(nc,"eastward_wind")
  V <- ncvar_get(nc,"northward_wind")
  Rain <- ncvar_get(nc,"precipitation_flux")
  pres <- ncvar_get(nc,"air_pressure")
  SW   <- ncvar_get(nc,"surface_downwelling_shortwave_flux_in_air")
  LW   <- ncvar_get(nc,"surface_downwelling_longwave_flux_in_air")
  CO2  <- try(ncvar_get(nc,"mole_fraction_of_carbon_dioxide_in_air"))
  
  useCO2 = is.numeric(CO2)  

  ## convert time to seconds
  sec = udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")
  
  nc_close(nc)
  
  ifelse(leap_year(year)==TRUE,
         dt <- (366*24*60*60)/length(sec), #leap year
         dt <- (365*24*60*60)/length(sec)) #non-leap year

  toff <- -lst*3600/dt

  ##buffer to get to GMT
  slen <- length(SW)
  Tair <- c(rep(Tair[1],toff),Tair)[1:slen]
  Qair <- c(rep(Qair[1],toff),Qair)[1:slen]
  U <- c(rep(U[1],toff),U)[1:slen]
  V <- c(rep(V[1],toff),V)[1:slen]
  Rain <- c(rep(Rain[1],toff),Rain)[1:slen]
  pres <- c(rep(pres[1],toff),pres)[1:slen]
  SW <- c(rep(SW[1],toff),SW)[1:slen]
  LW <- c(rep(LW[1],toff),LW)[1:slen]
  if(useCO2)  CO2 <- c(rep(CO2[1],toff),CO2)[1:slen]
 
### convert to CLM4.5 met variables


print("Done with met2model.CLM4")

} ### end loop over met files
  invisible(results)

} ### end met2model.CLM4
