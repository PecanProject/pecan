#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert SIBCASA output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.SIBCASA
##' @title Code to convert SIBCASA output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Tony Gardella
model2netcdf.SIBCASA <- function(outdir, sitelat, sitelon, start_date, end_date) {
  PEcAn.logger::logger.severe("NOT IMPLEMENTED")
  
} # model2netcdf.SIBCASA
