#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a LPJ-GUESS config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.LPJGUESS
##' @title Write LPJ-GUESS configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for LPJ-GUESS for given run
##' @export
##' @author Istem Fer, Tony Gardella
##-------------------------------------------------------------------------------------------------#
write.config.LPJGUESS <- function(defaults, trait.values, settings, run.id){
  
  require(PEcAn.settings)
  
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  if(!file.exists(rundir)) dir.create(rundir)
  outdir <- file.path(settings$run$host$outdir, run.id)
  if(!file.exists(outdir)) dir.create(outdir)
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.LPJGUESS"), n=-1)
  }
  
  # create host specific setttings
  hostspecific <- ""
  if (!is.null(settings$model$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$model$job.sh, collapse="\n"))
  }
  if (!is.null(settings$run$host$job.sh)) {
    hostspecific <- paste(hostspecific, sep="\n", paste(settings$run$host$job.sh, collapse="\n"))
  }
  
  #MET FILE
  metfile<- settings$run$input$met$path 
  
  # create job.sh
  jobsh <- gsub('@HOSTSPECIFIC@', hostspecific, jobsh)

  jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
  jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
  jobsh <- gsub('@SITE_MET@', metfile, jobsh)
  
  jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
  jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
  
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
  
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
#   if (!is.null(settings$model$config) && file.exists(settings$model$config)) {
#     config.text <- readLines(con=settings$model$config, n=-1)
#   } else {
#     filename <- system.file(settings$model$config, package = "PEcAn.LPJGUESS")
#     if (filename == "") {
#       if (!is.null(settings$model$revision)) {
#         filename <- system.file(paste0("config.", settings$model$revision), package = "PEcAn.LPJGUESS")
#       } else {
#         model <- db.query(paste("SELECT * FROM models WHERE id =", settings$model$id), params=settings$database$bety)
#         filename <- system.file(paste0("config.r", model$revision), package = "PEcAn.LPJGUESS")
#       }
#     }
#     if (filename == "") {
#       logger.severe("Could not find config template")
#     }
#     logger.info("Using", filename, "as template")
#     config.text <- readLines(con=filename, n=-1)
#   }
#   
#   config.text <- gsub('@SITE_LAT@', settings$run$site$lat, config.text)
#   config.text <- gsub('@SITE_LON@', settings$run$site$lon, config.text)
#   config.text <- gsub('@SITE_MET@', settings$run$inputs$met$path, config.text)
#   config.text <- gsub('@MET_START@', settings$run$site$met.start, config.text)
#   config.text <- gsub('@MET_END@', settings$run$site$met.end, config.text)
#   config.text <- gsub('@START_MONTH@', format(startdate, "%m"), config.text)
#   config.text <- gsub('@START_DAY@', format(startdate, "%d"), config.text)
#   config.text <- gsub('@START_YEAR@', format(startdate, "%Y"), config.text)
#   config.text <- gsub('@END_MONTH@', format(enddate, "%m"), config.text)
#   config.text <- gsub('@END_DAY@', format(enddate, "%d"), config.text)
#   config.text <- gsub('@END_YEAR@', format(enddate, "%Y"), config.text)
#   config.text <- gsub('@OUTDIR@', settings$run$host$outdir, config.text)
#   config.text <- gsub('@ENSNAME@', run.id, config.text)
#   config.text <- gsub('@OUTFILE@', paste('out', run.id, sep=''), config.text)
#  
#   #-----------------------------------------------------------------------
#   config.file.name <- paste0('CONFIG.',run.id, ".txt")
#   writeLines(config.text, con = paste(outdir, config.file.name, sep=''))
}
