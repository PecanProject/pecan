#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
.datatable.aware=TRUE
##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.MODEL
##' @title Write MODEL met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param ... can pass lat, lon, start.date and end.date
##' @return OK if everything was succesful.
##' @export
##' @author Rob Kooper, David LeBauer
##-------------------------------------------------------------------------------------------------#
met2model.BIOCRO <- function(in.path, in.prefix, outfolder, overwrite=FALSE, ...) {
  ncfiles = dir(in.path, full.names = TRUE, pattern = paste0(in.prefix, "*.nc$"),  
                all.files = FALSE, recursive = FALSE)
  metlist <- list()
  for(file in ncfiles){
    met.nc  <- nc_open(file)
    tmp.met <- load.cfmet(met.nc, lat = lat, lon = lon, start.date = start.date, end.date = end.date)
    metlist[[file]]     <- cf2biocro(tmp.met)
  }
  met <- rbindlist(metli)
  return(met)
}


cf2biocro <- function(met){
#   doy   2
#   hr    3
#   solar 4
#   temp  5
#   rh    6
#   windspeed  7
#   precip 8
  if(!"relative_humidity" %in% colnames(met)){
    if(all(c("air_temperature", "air_pressure", "specific_humidity") %in% colnames(met))){ 
      rh <- qair2rh(qair = met$specific_humidity, 
                    temp = ud.convert(met$air_temperature, "Kelvin", "Celsius"),
                    pres = ud.convert(met$air_pressure, "Pa", "hPa"))
      met <- cbind(met, relative_humidity = rh * 100)
    } else {
      logger.error("neither relative_humidity nor [air_temperature, air_pressure, and specific_humidity]",
                         "are in met data")
    }
  }
  if(!"ppfd" %in% colnames(met)){
    if("surface_downwelling_shortwave_flux_in_air" %in% colnames(met)){
      par <- sw2par(met$surface_downwelling_shortwave_flux_in_air)
      ppfd <- par2ppfd(par)
    } else {
      logger.error("Need either ppfd or surface_downwelling_shortwave_flux_in_air in met dataset")
    }
  }
  if(!"wind_speed" %in% colnames(met)){
    if(all(c("northward_wind", "eastward_wind") %in% colnames(met))){
      wind_speed <- sqrt(northward_wind^2 + eastward_wind^2)
    }
    logger.error("neither wind_speed nor both eastward_wind and northward_wind are present in met data")

  }
  
  newmet <- met[, list(year = year, doy = doy, hour = hour,
                       SolarR = ppfd,
                       Temp = ud.convert(air_temperature, "Kelvin", "Celsius"), 
                       RH = relative_humidity, 
                       WS = wind_speed, 
                       precip = ud.convert(precipitation_flux, "s-1", "h-1"))] 
  return(newmet)
}
