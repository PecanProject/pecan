#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a config file for Maespa
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.MAESPA
##' @title Write MAESPA configuration files
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for MODEL for given run
##' @export
##' @author Tony Gardella
##-------------------------------------------------------------------------------------------------#
write.config.MAESPA <- function(settings,run.id){
  
  #This script stands alone from workflow right now.
  #You need to have a pecan.xml file that reads in as the settings argument
  
  require(PEcAn.settings)
  # TODO
  # add arguments that allow pecan met to be used 
  # In template file, met is hard coded. Once met2model works, revert back to 
  # using gsub to point to met file in database
  # add code that will format traitvalues as necessary .dat files.
  # 
 
  
  settings <- read.settings(settings)
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, as.character(run.id))
  outdir <- file.path(settings$run$host$outdir, as.character(run.id))
  if (is.null(settings$run$host$qsub) && (settings$run$host$name == "localhost")) {
    rundir <- file.path(settings$rundir, as.character(run.id))
    outdir <- file.path(settings$modeloutdir, as.character(run.id))
  }
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.MAESPA"), n=-1)
    
  }
  
  # create host specific settings
  hostspecific <- ""
  if (!is.null(settings$model$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$model$job.sh, collapse="\n"))
  }
  if (!is.null(settings$run$host$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$run$host$job.sh, collapse="\n"))
  }

  # create job.sh
  jobsh <- gsub('@HOSTSPECIFIC@', hostspecific, jobsh)

  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  #jobsh <- gsub('@SITE_MET@', settings$run$inputs$met, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
  
  writeLines(jobsh, con=file.path(settings$rundir, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
}

