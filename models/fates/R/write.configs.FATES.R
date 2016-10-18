#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes config files for use with FATES.
##'
##' @name write.config.FATES
##' @title Write FATES configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return none
##' @export
##' @author Mike Dietze
##-------------------------------------------------------------------------------------------------#
write.config.FATES <- function(defaults, trait.values, settings, run.id) {
  library(PEcAn.utils)
  #  
  # #OUTLINE OF MODULES
  #   # Copy Case and Build
  #   #  -symbolic link to refernce case that is already completed
  #   # Edit user_nl_* files to add site info
  #   # make Jobs.sh -case_submit
  #   # call met2model and add to namelists
  #   #
  
  # find out where things are
  local.rundir <- file.path(settings$rundir, run.id)  ## this is on local machine for staging
  rundir       <- file.path(settings$host$rundir, run.id)  ## this is on remote machine for execution
  casedir      <- file.path(rundir, "case")
  outdir       <- file.path(settings$host$outdir, run.id)
  refcase      <- settings$model$binary
  bld          <- file.path(refcase, "bld")
  binary       <- file.path(bld, "cesm.exe")
  indir        <- file.path(rundir, "input")  ## input directory
  default      <- settings$run$inputs$default$path  ## reference inputs file structure
  
  ## DATES
  ## CLM is a bit odd and takes a start date and length, so we need to precompute
  ## this needs to be generalized to fractional years, but accounting for 365 day year  start_date <- as.Date(settings$run$start.date)
  end_date   <- as.Date(settings$run$end.date)
  stop_n     <- as.numeric(end_date - start_date, units = "days") - 
    n_leap_day(start_date, end_date) + 1
  
  ##-----------------------------------------------------------------------##
  ##                                                                       ##
  ##                             INPUTS                                    ##
  ##                                                                       ##
  ##-----------------------------------------------------------------------##
  
  ## SITE INFO --> DOMAIN FILE (lat/lon)
  gridres <- 0.125  ## ultimately this should be a variable
  lat     <- settings$run$site$lat
  lon     <- (settings$run$site$lon + 360) %% 360  ## make sure coords in 0-360 range, not negative
  domain.default <- system.file("domain.lnd.1x1pt-brazil_navy.090715.nc", package = "PEcAn.FATES")
  file.copy(domain.default, local.rundir)
  domain.nc <- nc_open(file.path(local.rundir, basename(domain.default)), write = TRUE)
  ncvar_put <- ncdf4::ncvar_put
  ncvar_put(nc = domain.nc, varid = "xc", vals = lon)
  ncvar_put(nc = domain.nc, varid = "yc", vals = lat)
  ncvar_put(nc = domain.nc, varid = "xv", vals = lon + c(-1, 1, 1, -1) * gridres)
  ncvar_put(nc = domain.nc, varid = "yv", vals = lat + c(-1, -1, 1, 1) * gridres)
  ncvar_put(nc = domain.nc, varid = "area", vals = (2 * gridres * pi/180) ^ 2)
  nc_close(domain.nc)
  
  ## MET HEADERS
  if (!is.null(settings$run$inputs$met)) {
    
    ## DATM HEADER: datm_atm_in
    datm <- readLines(con = system.file("datm_atm_in.template", package = "PEcAn.FATES"), n = -1)
    datm <- gsub("@DOMAIN@", file.path(indir, "share/domains/domain.clm", 
                                       basename(domain.default)), datm)
    datm <- gsub("@START_YEAR@", lubridate::year(start_date), datm)
    datm <- gsub("@END_YEAR@", lubridate::year(end_date), datm)
    writeLines(datm, con = file.path(local.rundir, "datm_atm_in"))
    
    ## DATM STREAM MET
    met <- readLines(con = system.file("datm.streams.txt.PEcAn_met.template", 
                                       package = "PEcAn.FATES"), n = -1)
    met <- gsub("@INDIR@", indir, met)
    met <- gsub("@MET_PATH@", settings$run$inputs$met$path, met)
    met.files <- dir(settings$run$inputs$met$path, "*.nc")
    met <- gsub("@MET_FILES@", paste(met.files, collapse = "\n            "), met)
    writeLines(met, con = file.path(local.rundir, "datm.streams.txt.PEcAn_met"))
  }
  
  #   ... fill in this template, the met template, and then have jobs.sh put them in the right place. 
  #   ... Test, then adjust DB to have met required
  
  
  ##-----------------------------------------------------------------------##
  ##                                                                       ##
  ##                             JOB.SH                                    ##
  ##                                                                       ##
  ##-----------------------------------------------------------------------##
  
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.FATES"), n = -1)
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
  
  ## PATHS
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  jobsh <- gsub("@CASEDIR@", casedir, jobsh)
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@REFCASE@", refcase, jobsh)
  jobsh <- gsub("@BLD@", bld, jobsh)
  jobsh <- gsub("@BINARY@", binary, jobsh)
  jobsh <- gsub("@INDIR@", indir, jobsh)
  jobsh <- gsub("@DEFAULT@", default, jobsh)
  
  ## DATES -> ENV_RUN
  jobsh <- gsub("@START_DATE@", start_date, jobsh)
  jobsh <- gsub("@STOP_N@", stop_n, jobsh)
  
  ## MET --> DATM
  #   jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
  ## FOR FIRST STEP, CAN USE DEFAULT
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  # ## Write PARAMETER file ## Write SETTINGS file
} # write.config.FATES
