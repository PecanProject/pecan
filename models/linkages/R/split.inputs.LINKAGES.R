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
split.inputs.LINKAGES <- function(settings, start.time, stop.time, inputs) {
  
  #### Load all temp and precip met for site
  met <- inputs$met$path
  load(met)
  
  #### Add years for indexing
  temp.mat.years <- cbind(temp.mat,seq(850,2010,1))
  precip.mat.years <- cbind(precip.mat,seq(850,2010,1))
  
  #### Grab needed years
  temp.mat <- temp.mat.years[temp.mat.years[,13]==year(start.time):year(stop.time),1:12]
  precip.mat <- precip.mat.years[precip.mat.years[,13]==year(start.time):year(stop.time),1:12]
  
  #### Save to climate file
  save(temp.mat, precip.mat, file = paste0(settings$rundir, "/climate.Rdata"))
  
  new.met <- paste0('',settings$rundir, "/climate.Rdata")
  settings$run$inputs$met$path <- new.met #Do I need to change the ID too? Idk what I would change it to. 
  
  inputs <- settings$run$inputs["met"]
  
  return(inputs)
} # split.inputs.LINKAGES
