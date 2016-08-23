#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#

## R Code to convert NetCDF CF met files into MAAT model met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and 
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change 
##overwrite statement below to TRUE.

##-------------------------------------------------------------------------------------------------#
##' met2model wrapper for MAAT
##'
##' @name met2model.MAAT
##' @title Create MAAT met driver files
##' @param in.path location on disk where inputs (CF met drivers) are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where MAAT met outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @export
##' @author Shawn P. Serbin
##'
met2model.MAAT <- function(in.path, in.prefix, outfolder, start_date, end_date, ..., overwrite=FALSE,verbose=FALSE){
  if(!require(PEcAn.utils)) print("**Plesae install PEcAn.utils then retry**")
  
  ## MAAT driver format (.csv):
  ## Timestep,  Air Temp (°C), PAR (umols m-2 s-1), Precipitation( ??), Atmospheric CO2 (μmol mol-1) ... # STILL IN DEVELOPMENT
  
  print("START met2model.MAAT")
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date<- as.POSIXlt(end_date, tz = "GMT")
  
  out.file <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"),strptime(end_date, "%Y-%m-%d"),"clim", sep=".")
  out.file.full <- file.path(outfolder, out.file)
  
} # End of function
##-------------------------------------------------------------------------------------------------#
### EOF