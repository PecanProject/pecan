#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# R Code to convert NetCDF CF met files into PEcAn met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model for PEcAn
##'
##' @title met2model.PEcAn
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @importFrom ncdf4 ncvar_get
met2model.PEcAn <- function(in.path, in.prefix, outfolder, start_date, end_date,
                            overwrite = FALSE, verbose = FALSE, ...) {
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  out.file <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"), 
                    strptime(end_date, "%Y-%m-%d"), 
                    "dat", sep = ".")
  out.file.full <- file.path(outfolder, out.file)
  results <- data.frame(file = c(out.file.full), 
                        host = c(fqdn()),
                        mimetype = c("text/plain"), 
                        formatname = c("PEcAn"), 
                        startdate = c(start_date), 
                        enddate = c(end_date), 
                        dbfile.name = out.file, 
                        stringsAsFactors = FALSE)
  return(invisible(results))
  
} # met2model.PEcAn
