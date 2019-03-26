#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## split clim file into smaller time units to use in KF
##' @title split_inputs.SIPNET
##' @name  split_inputs.SIPNET
##' @author Mike Dietze and Ann Raiho
##' 
##' @param settings PEcAn settings object
##' @param start.time start date and time for each SDA ensemble
##' @param stop.time stop date and time for each SDA ensemble
##' @param inputs list of model inputs to use in write.configs.SIPNET
##' @param overwrite Default FALSE
##' @param outpath if specified, write output to a new directory. Default NULL writes back to the directory being read
##' @description Splits climate met for SIPNET
##' 
##' @return file split up climate file
##' @export
split_inputs.SIPNET <- function(settings, start.time, stop.time, inputs, overwrite = FALSE, outpath = NULL) {
  
  #### Lubridate start and end times
  start.day <- lubridate::yday(start.time)
  start.year <- lubridate::year(start.time)
  #end.day <- length(as.Date(start.time):as.Date(stop.time))
  end.day <- lubridate::yday(stop.time)
  end.year <- lubridate::year(stop.time)
  

  #### Get met paths
  met <- inputs
  path <- dirname(met)
  prefix <- sub(".clim", "", basename(met), fixed = TRUE)
  if(is.null(outpath)){
    outpath <- path
  }
  if(!dir.exists(outpath)) dir.create(outpath)
  

  file <- NA
  names(file) <- paste(start.time, "-", stop.time)
  file <- paste0(outpath, "/", prefix, ".", paste0(as.Date(start.time), "-", as.Date(stop.time)), ".clim")
  
  if(file.exists(file) & !overwrite){
    
    return(file)
  }

  dat <- read.table(met, header = FALSE)

  ###### Find Correct Met
  sel1 <- which(dat[, 2] == as.numeric(start.year) & dat[, 3] == as.numeric(start.day))[1]
  sel2 <- which(dat[, 2] == as.numeric(end.year) & 
                  dat[, 3] == as.numeric(end.day))[length(which(dat[, 2] == as.numeric(end.year) & 
                                                                  dat[, 3] == as.numeric(end.day)))]

  ###### Write Met to file

  write.table(dat[sel1:sel2, ], file, row.names = FALSE, col.names = FALSE)

  ###### Output input path to inputs
  #settings$run$inputs$met$path <- file
  return(file)
} # split_inputs.SIPNET
