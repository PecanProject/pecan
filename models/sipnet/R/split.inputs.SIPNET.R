#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## split clim file into smaller time units to use in KF
##' @title split.inputs.SIPNET
##' @name  split.inputs.SIPNET
##' @author Mike Dietze and Ann Raiho
##' 
##' @param multi.settings
##' @param start.time
##' @param stop.time
##' @description Splits climate met for SIPNET
##' 
##' @return file split up climate file
##' @export
split.inputs.SIPNET <- function(settings, start.time, stop.time) {
  
  start.day <- day(start.time)
  start.year <- lubridate::year(start.time)
  
  end.day <- day(stop.time)
  end.year <- lubridate::year(stop.time)
  
  met <- c(settings$run$inputs$met$path)
  
  path <- dirname(met)
  prefix <- sub(".clim", "", basename(met), fixed = TRUE)
  dat <- read.table(met, header = FALSE)
  file <- NA
  names(file) <- paste(start.time, "-", stop.time)
  
  sel1 <- which(dat[, 2] == as.numeric(start.year) & dat[, 3] == as.numeric(start.day))[1]
  sel2 <- which(dat[, 2] == as.numeric(end.year) & 
                  dat[, 3] == as.numeric(end.day))[length(which(dat[, 2] == as.numeric(end.year) & 
                                                                  dat[, 3] == as.numeric(end.day)))]
  
  file <- paste0(path, "/", prefix, ".", paste0(as.Date(start.time), "-", as.Date(stop.time)), ".clim")
  
  write.table(dat[sel1:sel2, ], file, row.names = FALSE, col.names = FALSE)
  
  settings$run$inputs$met$path <- file
  settings$run$inputs
} # split.inputs.SIPNET
