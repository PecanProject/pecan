#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a BASGRA config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.BASGRA
##' @title Write BASGRA configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for BASGRA for given run
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
write.config.BASGRA <- function(defaults, trait.values, settings, run.id) {

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  # load default(!) BASGRA params
  run_params <- PEcAn.utils::load_local(system.file("BASGRA_params.Rdata",package = "PEcAn.BASGRA"))$default_params
  
  run_params[which(names(default_params) == "LAT")] <- as.numeric(settings$run$site$lat)
  
  #### write run-specific PFT parameters here #### Get parameters being handled by PEcAn
  for (pft in seq_along(trait.values)) {
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names <- names(pft.traits)
    
    # N-C ratio of roots (g N g-1 C)
    if ("c2n_fineroot" %in% pft.names) {
      run_params[which(names(default_params) == "NCR")] <- 1/pft.traits[which(pft.names == "c2n_fineroot")]
    }
    
    # PAR extinction coefficient (m2 m-2)
    if ("extinction_coefficient" %in% pft.names) {
      run_params[which(names(default_params) == "K")] <- pft.traits[which(pft.names == "extinction_coefficient")]
    }
    
    # Transpiration coefficient (mm d-1)
    if ("transpiration_coefficient" %in% pft.names) {
      run_params[which(names(default_params) == "TRANCO")] <- pft.traits[which(pft.names == "transpiration_coefficient")]
    }
    
    # Temperature that kills half the plants in a day (degrees Celcius)
    if ("plant_min_temp" %in% pft.names) {
      run_params[which(names(default_params) == "LT50")] <- pft.traits[which(pft.names == "plant_min_temp")]
    }
    
    if ("phyllochron" %in% pft.names) {
      run_params[which(names(default_params) == "PHY")] <- pft.traits[which(pft.names == "phyllochron")]
    }
    
  }


  #-----------------------------------------------------------------------
  # write job.sh
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.BASGRA"), n = -1)
  }
  
  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", settings$run$inputs$met$path, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  
  jobsh <- gsub("@RUN_PARAMS@", paste0("c(",listToArgString(run_params),")"), jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  

} # write.config.MODEL
