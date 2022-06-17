#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a LDNDC config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.LDNDC
##' @title Write LDNDC configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for LDNDC for given run
##' @export
##' @author Henri Kajasilta
##-------------------------------------------------------------------------------------------------#
write.config.LDNDC <- function(defaults, trait.values, settings, run.id) {

  
  # Check minimum package required
  # This is LDNDC specific and just hard-coded for now
  # Probably now reason to change
  MinPackReq <- "1.9"
  
  
  # Create Schedule time
  if(!is.null(settings$run$start.date) & !is.null(settings$run$end.date)){
    
    steps <- 48 # Hard-coded for now
    ScheduleTime <- paste0(format(as.POSIXlt(settings$run$start.date), "%Y-%m-%d"), "/",
                           steps, " -> ", as.Date(format(as.POSIXlt(settings$run$end.date), "%Y-%m-%d"))+1)
                          # One day added to end day of simulations, because the simulations will stop the first
                          # timestep in that day and wouldn't simulate the whole last day otherwise.
                          # This one observations is taken out it model2netcdf to not include extra observations
                          # in netcdf file
  }

  
  # Find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  # Source
  if(!is.null(settings$run$inputs$met$path)){
    # For climate data
    MetPath <- settings$run$inputs$met$path
    # Info for project file from which directory to read the inputs
    SourcePrefix <- paste0(rundir, "/")
    # Model outputs are written into own directory
    OutputPrefix <- file.path(outdir, "Output/")
  }
  
  #-----------------------------------------------------------------------
  ## Fill .ldndc template with the given settings
  projectfile <- readLines(con = system.file("project.ldndc", package = "PEcAn.LDNDC"), n = -1)
  
  
  # Changes
  projectfile <- gsub('@PackMinVerReq@', MinPackReq, projectfile)
  
  projectfile <- gsub('@ScheduleTime@', ScheduleTime, projectfile)
  
  projectfile <- gsub('@SourcePrefix@', SourcePrefix, projectfile)
  projectfile <- gsub('@OutputPrefix@', OutputPrefix, projectfile)
  
  # Write project file to rundir
  writeLines(projectfile, con = file.path(settings$rundir, run.id, "project.ldndc"))
  
  
  
  
  #-----------------------------------------------------------------------
  # Create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.LDNDC"), n = -1)
  }
  
  # Create host specific settings
  # NOT MEANINGFUL HERE -----
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
  
  #------
  
  
  # Create job.sh based on the given settings
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  jobsh <- gsub("@METPATH@", MetPath, jobsh)
  
  jobsh <- gsub("@BINARY@", paste(settings$model$binary, paste0(rundir, "/project.ldndc")), jobsh)
  
  # if(is.null(settings$model$delete.raw)){
  #   settings$model$delete.raw <- FALSE
  # }
  # 
  # jobsh <- gsub("@DELETE.RAW@", settings$model$delete.raw, jobsh)
  
  # Write job.sh file to rundir
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh")) # Permissions
  
  
  
  #### write run-specific PFT parameters here #### Get parameters being handled by PEcAn
  speciesparfile <- readLines(con = system.file("speciesparameter_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  
  #----------------------
    
  ## Set-up the necessary files in to the run directory so
  ## model is able to function properly. Later on, these
  ## files should be populated with initial values.
    
  
  ###### THIS NEEDS TO BE FUNCTION AT SOME POINT
  ### PROBABLY SIMILAR FUNCTION FOR siteparameters as well
  #
  mnemonic_1 <- "__grasses__"
  group <- "grass"
  mnemonic_2 <- "perennialgrass"
  
  a.1 <- paste0("<species mnemonic='", mnemonic_1, "' group='", group, "' > \n")
  b.1 <- paste0("\t\t\t\t\t<species mnemonic='", mnemonic_2, "' > \n")
  b.2 <- apply(trait.values[[1]], 1, function(x){paste0("\t\t\t\t\t\t<par name='", names(x), "' value='", x, "' /> \n")})
  b.3 <- paste0("\t\t\t\t</species> \n")
  a.2 <- paste0("\t\t\t</species>")
  
  speciesparfile <- gsub("@Info@", paste(a.1,b.1,b.2,b.3,a.2), speciesparfile)
  
  writeLines(speciesparfile, con = file.path(settings$rundir, run.id, "speciesparameters.xml"))
  

  
  # Default events file, need to change later on. Only takes care of some initial biomass and
  # then some random events. Not made for real use, but testing.
  eventsfile <- readLines(con = system.file("events_template.xml", package = "PEcAn.LDNDC"), n = -1)
  
  eventsfile <- gsub("@Startdate@", as.Date(settings$run$start.date, format = "%Y/%m/%d"), eventsfile)
  eventsfile <- gsub("@Event_1_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + sample(120:160, 1), eventsfile)
  eventsfile <- gsub("@Event_2_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + sample(170:180, 1), eventsfile)
  eventsfile <- gsub("@Event_3_Time@", as.Date(settings$run$start.date, format = "%Y/%m/%d") + sample(250:300, 1), eventsfile)
  
  writeLines(eventsfile, con = file.path(settings$rundir, run.id, "events.xml"))



  # Default setup file, need to change later on
  setupfile <- readLines(con = system.file("setup.xml", package = "PEcAn.LDNDC"), n = -1)
  writeLines(setupfile, con = file.path(settings$rundir, run.id, "setup.xml"))

  
  # Default site file, need to change later on
  sitefile <- readLines(con = system.file("site.xml", package = "PEcAn.LDNDC"), n = -1)
  writeLines(sitefile, con = file.path(settings$rundir, run.id, "site.xml"))
  
  # Default site file, need to change later on
  siteparfile <- readLines(con = system.file("siteparameters.xml", package = "PEcAn.LDNDC"), n = -1)
  writeLines(siteparfile, con = file.path(settings$rundir, run.id, "siteparameters.xml"))
  
  
  # Use ready airchemistry file for now
  airchemistry <- readLines(con = system.file("airchemistry.txt", package = "PEcAn.LDNDC"), n = -1)
  writeLines(airchemistry, con = file.path(settings$rundir, run.id, "airchemistry.txt"))
  
  
  #------------------------
  
} # write.config.LDNDC
