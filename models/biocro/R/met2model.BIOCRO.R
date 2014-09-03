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
##' @return OK if everything was succesful.
##' @export
##' @author Rob Kooper, David LeBauer
##-------------------------------------------------------------------------------------------------#
met2model.BIOCRO <- function(in.path, in.prefix, outfolder, overwrite=FALSE) {
  ncfiles = dir(in.path, in.prefix, full.names = TRUE, pattern = "*.nc")
  metdata <- list()
  for(file in ncfiles){
    metdata[[file]] <- load.cfmet()
  }
  tullymet <- met[,list(year = year, doy = doy, hour = hour, 
                        #SolarR = surface_downwelling_shortwave_flux_in_air,
                        SolarR = ud.convert(surface_downwelling_photosynthetic_photon_flux_in_air, "mol", "umol") ,
                        Temp = ud.convert(air_temperature, "Kelvin", "Celsius"), 
                        RH = relative_humidity / 100, 
                        WS = wind_speed, 
                        precip = precipitation_flux)]
}
