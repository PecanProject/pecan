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
##' @author Ann Raiho
##-------------------------------------------------------------------------------------------------#
write.config.LINKAGES <- function(defaults=NULL, trait.values=NULL, settings, run.id){
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  if(!file.exists(rundir)) dir.create(rundir)
  outdir <- file.path(settings$run$host$outdir, run.id)
  if(!file.exists(outdir)) dir.create(outdir)  

  #-----------------------------------------------------------------------
  # write LINKAGES settings file
  start.year = as.numeric(strftime(settings$run$start.date,"%Y"))
  end.year = as.numeric(strftime(settings$run$end.date,"%Y"))
  year = seq(start.year,end.year,1)
  
  kprnt = 50 #year interval for output
  klast = 90 #number of plots
  nyear = length(year) #number of years to simulate
  ipolat_nums = seq(2,nyear,25) #years for climate interpolation #need to make break points generalizable someday
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
    
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.LINKAGES"), n=-1)
  }
  
  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
  
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
}
