#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into LPJ-GUESS met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and 
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change 
##overwrite statement below to TRUE.

##' met2model wrapper for LPJ-GUESS
##'
##' @title met2model.LPJGUESS
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @author Istem Fer
met2model.LPJGUESS <- function(in.path, in.prefix, outfolder, start_date, end_date, ..., overwrite=FALSE,verbose=FALSE){
  
  ## function arguments for development
  in.path='/fs/data5/pecan.models/LPJ-GUESS/build/CRUNCEP_site_0-622'
  in.prefix='CRUNCEP'
  outfolder='/fs/data5/pecan.models/LPJ-GUESS/build/CRUNCEP_LPJGUESS_site_0-622/'
  start_date='2002/01/01'
  end_date='2002/12/31'
  
  library(PEcAn.utils)
  
  print("START met2model.LPJGUESS")
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  
} ### end met2model.LPJGUESS
