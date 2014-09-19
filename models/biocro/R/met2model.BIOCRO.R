#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
  met <- data.table(met)
  if(!"relative_humidity" %in% colnames(met)){
    met <- met[, `:=` (relative_humidity = qair2rh(qair = specific_humidity, 
                                                   temp = ud.convert(air_temperature, "Kelvin", "Celsius"),
                                                   pres = ud.convert(air_pressure, "Pa", "hPa")))]
  }
  
  newmet <- met[, list(year = year, doy = doy, hour = hour,
                       SolarR = ppfd,
                       Temp = ud.convert(air_temperature, "Kelvin", "Celsius"), 
                       RH = relative_humidity, 
                       WS = sqrt(northward_wind^2 + eastward_wind^2), 
                       precip = ud.convert(precipitation_flux, "s-1", "h-1"))] 
  return(newmet)
}
