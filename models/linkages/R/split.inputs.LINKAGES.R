#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.  All rights reserved. This program and the
# accompanying materials are made available under the terms of the University of Illinois/NCSA
# Open Source License which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' @title split.inputs.LINKAGES
##' @name  split.inputs.LINKAGES
##' @author Ann Raiho
##' 
##' @param multi.settings
##' @param start.time
##' @param stop.time
##' @description Splits climate met for LINKAGES
##' 
##' @return files split up climate files
##' @export
##' 
split.inputs.LINKAGES <- function(settings, start.time, stop.time) {
  
  new.met <- paste0(settings$rundir, "/climate.Rdata")  # doesn't do anything but write stuff to README
  settings$run$inputs$met$path <- new.met  #HACK
  
  settings$run$inputs
} # split.inputs.LINKAGES
