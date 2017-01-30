#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# R Code to convert NetCDF CF met files into GDAY met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model for GDAY
##'
##' @title met2model.GDAY
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will
##'        only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use
##'        the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##'
##' @author Martin De Kauwe, Tony Gardella
##' 
met2model.GDAY <- function(in.path, in.prefix, outfolder, start_date, end_date, 
                           overwrite = FALSE, verbose = FALSE, ...) {
  
  ## GDAY driver format (.csv):
  ## 30min: year (-), doy (-; NB. leap years), hod (-), rainfall (mm 30 min-1),
  ##        par (umol m-2 s-1), tair (deg C), tsoil (deg C), vpd (kPa),
  ##        co2 (ppm), ndep (t ha-1 30 min-1), wind (m-2 s-1), press (kPa)
  ##
  ## Daily:
  ## 30min: year (-), doy (-; NB. leap years), tair (deg C),
  ##        rainfall (mm day-1), tsoil (deg C), tam (deg C), tpm (deg C),
  ##        tmin (deg C), tmax (deg C), tday (deg C), vpd_am (kPa),
  ##        vpd_pm (kPa), co2 (ppm), ndep (t ha-1 day-1), wind (m-2 s-1),
  ##        press (kPa), wind_am (m-2 s-1), wind_pm (m-2 s-1),
  ##        par_am (umol m-2 s-1), par_pm (umol m-2 s-1)
  

  
  
  ## write output
  write.table(out, out.file.full, quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  
  return(invisible(results))
} # met2model.GDAY
