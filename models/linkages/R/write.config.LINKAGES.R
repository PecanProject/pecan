#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a LINKAGES config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.LINKAGES
##' @title Write LINKAGES configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for LINKAGES for given run
##' @export
##' @author Rob Kooper
##-------------------------------------------------------------------------------------------------#
write.config.LINKAGES <- function(defaults, trait.values, settings, run.id){
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)

  #-----------------------------------------------------------------------
  # write LINKAGES settings file
  
  kprnt = 50 #year interval for output
  klast = 90 #number of plots
  nyear = 1150 #number of years to simulate
  ipolat_nums = seq(0,nyear,500) #years for climate interpolation
  ipolat = length(ipolat_nums)-1 #number of years for climate interpolation
  plat = settings$run$site$lat #latitude
  plong = settings$run$site$lon #longitude
  bgs = 127 #julian day to begin growing season
  egs = 275 #julian day to end growing season
  fc = 27 #field capacity
  dry = 17 #wilting point
  
  sink(file.path(rundir,"test_text.txt"))
  cat(kprnt,klast,nyear,sep=",")
  cat("\n")
  cat(ipolat)
  cat("\n")
  cat(ipolat_nums,sep=",")
  cat("\n")
  cat(plat,plong,bgs,egs,fc,dry,sep=",")
  sink()

  ## as initial hack, copy parameter file from inst to rundir
  param.file=system.file("SPP.DAT", package = "PEcAn.LINKAGES")
  file.copy(from = param.file,rundir)
  
  ## Replace this with creating a symlink from rundir to met
  temp_vec = c(c(-6.3,-4.7,-0.3,6.6,12.7,17.7,20.5,19.5,14.7,8.0,2.9,-3.0),c(-6.3,-4.7,-0.3,6.6,12.7,17.7,20.5,19.5,14.7,8.0,2.9,-3.0),c(-6.3,-4.7,-0.3,6.6,12.7,17.7,20.5,19.5,14.7,8.0,2.9,-3.0)) 
  temp_means = matrix(round(rnorm(12*ipolat,temp_vec,0),1),ipolat,12,byrow=TRUE)# monthly mean temperature
  temp_sd = matrix(1,ipolat,12) #monthly temperature standard deviation
  precip_vec = c(c( 8.5,7.9,9.9,9.8,9.7,11.1,11.7,9.4,9.4,11.5,10.7,9.9),c( 8.5,7.9,9.9,9.8,9.7,11.1,11.7,9.4,9.4,11.5,10.7,9.9),c( 8.5,7.9,9.9,9.8,9.7,11.1,11.7,9.4,9.4,11.5,10.7,9.9))#
  precip_means = matrix(round(rnorm(12*ipolat,precip_vec,0),1),ipolat,12,byrow=TRUE) #monthly mean precipitation
  precip_sd = temp_sd #monthly standard deviation precipitation
  write.table(file=file.path(rundir,"test_text1.txt"),rbind(temp_means,temp_sd,precip_means,precip_sd),sep=",",col.names=FALSE,row.names=FALSE)
  #file.show("test_text1.txt")
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.LINKAGES"), n=-1)
  }
  
  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  jobsh <- gsub('@SITE_MET@', settings$run$inputs$met, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
  
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
}
