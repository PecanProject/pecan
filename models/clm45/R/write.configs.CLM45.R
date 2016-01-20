##------------------------------------------------------------------------------
##Copyright (c) 2015 NCSA
##All rights reserved. This program and the accompanying materials
##are made available under the terms of the 
##University of Illinois/NCSA Open Source License
##which accompanies this distribution, and is available at
##http://opensource.ncsa.illinois.edu/license.html
##------------------------------------------------------------------------------


##-------------------------------------------------------------------------------------------------#
##' Writes config files for use with the Community Land Model model.
##'
##' @name write.config.CLM45
##' @title Write CLM4.5 configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return none
##' @export
##' @author Mike Dietze
##-------------------------------------------------------------------------------------------------#
write.config.CLM45 <- function(defaults, trait.values, settings, run.id){
  
  #Outline to get Dan's custom run to work
  # Run Dan's Python script in scripts directory
  # Point to scripts/run - run "id of run".build 
  # Edit datm.streams.txt.CLM1PT.CLM_USRDAT to make sure paths are correct
  # run cesm.exe from scripts/run_id/bld/
  #
  #Need- Figure how to configure runs and also figure out what preprocessing and ppost processing is done to inputs and outputs.
  #There might be optoins and utilities that we can use 

  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)
  
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.CLM45"), n=-1)
  }
  
  # create host specific setttings
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
  jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
  
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  ## Write PARAMETER file

  ## Write SETTINGS file
    
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
