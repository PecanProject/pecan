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
##' @author Mike Dietze, Shawn Serbin
##-------------------------------------------------------------------------------------------------#
write.config.FATES <- function(defaults, trait.values, settings, run.id){

   ## site information
   site <- settings$run$site
   site.id <- as.numeric(site$id)
  
   # find out where things are
   local.rundir <- file.path(settings$rundir, run.id) ## this is on local machine for staging
   rundir       <- file.path(settings$host$rundir, run.id)  ## this is on remote machine for execution
   casedir      <- file.path(rundir,"case") 
   outdir       <- file.path(settings$host$outdir, run.id)
   refcase      <- settings$model$binary
   bld          <- file.path(refcase,"bld")
   binary       <- file.path(bld,"cesm.exe")
   indir        <- file.path(rundir,"input") ## input directory
   default      <- settings$run$inputs$default$path ## reference inputs file structure
   site_name    <- paste0(site.id %/% 1000000000, "-", site.id %% 1000000000)
   
   ## DATES
   ## CLM is a bit odd and takes a start date and length, so we need to precompute
   ## this needs to be generalized to fractional years, but accounting for 365 day year
   start_date <- as.Date(settings$run$start.date)
   end_date   <- as.Date(settings$run$end.date)
   stop_n     <- as.numeric(end_date - start_date, units="days") - PEcAn.utils::n_leap_day(start_date,end_date) + 1  
   
   ##-----------------------------------------------------------------------##
   ##                                                                       ##
   ##                             INPUTS                                    ##
   ##                                                                       ##
   ##-----------------------------------------------------------------------##

   ## SITE INFO --> DOMAIN FILE (lat/lon)
   #  - should also store this in the refcase directory for PEcAn so we can grab from there, and not the PEcAn package
   gridres <- 0.125  ## ultimately this should be a variable
   lat <- as.numeric(site$lat)
   lon <- (as.numeric(site$lon) + 360) %% 360 ## make sure coords in 0-360 range, not negative
   domain.default <- system.file("domain.lnd.1x1pt-brazil_navy.090715.nc",package="PEcAn.FATES")
   domain.file <- file.path(local.rundir,paste0("domain.lnd.",site_name,".nc"))
   file.copy(domain.default,domain.file)
   domain.nc <- ncdf4::nc_open(domain.file,write=TRUE)
   ncdf4::ncvar_put(nc=domain.nc, varid='xc', vals=lon)
   ncdf4::ncvar_put(nc=domain.nc, varid='yc', vals=lat)
   ncdf4::ncvar_put(nc=domain.nc, varid='xv', vals=lon+c(-1,1,1,-1)*gridres)
   ncdf4::ncvar_put(nc=domain.nc, varid='yv', vals=lat+c(-1,-1,1,1)*gridres)
   ncdf4::ncvar_put(nc=domain.nc, varid='area', vals=(2*gridres*pi/180)^2)   
   ncdf4::nc_close(domain.nc)
   
   ## SURF - should also store this in the refcase directory for PEcAn so we can grab from there, and not the PEcAn package
   surf.default <- system.file("surfdata_1x1_brazil_16pfts_Irrig_CMIP6_simyr2000_c171214.nc",package = "PEcAn.FATES")
   surf.file    <- file.path(local.rundir,paste0("surfdata_",site_name,"_simyr2000.nc"))
   file.copy(surf.default,surf.file)
   Sys.chmod(surf.file)
   surf.nc <- ncdf4::nc_open(surf.file,write=TRUE)
   ncdf4::ncvar_put(nc=surf.nc, varid='LONGXY', vals=lon)
   ncdf4::ncvar_put(nc=surf.nc, varid='LATIXY', vals=lat)
   ncdf4::nc_close(surf.nc)   
   
   ## MET HEADERS
   if(!is.null(settings$run$inputs$met)){

     ## DATM HEADER: datm_atm_in
     datm <- readLines(con=system.file("datm_atm_in.template",package = "PEcAn.FATES"),n=-1)
     datm <- gsub('@DOMAIN@', file.path(indir,"share/domains/domain.clm",basename(domain.file)), datm)
     datm <- gsub('@START_YEAR@',lubridate::year(start_date), datm)
     datm <- gsub('@END_YEAR@',lubridate::year(end_date), datm)
     writeLines(datm, con=file.path(local.rundir, "datm_atm_in"))
     
     ## DATM STREAM MET
     met <- readLines(con=system.file("datm.streams.txt.PEcAn_met.template",package = "PEcAn.FATES"),n=-1)
     met <- gsub('@INDIR@',indir, met)
     #domain.file.name <- paste0("domain.lnd.",site_name,".nc")
     #met <- gsub('@DOMAIN@',domain.file.name, met)  # attempting to provide correct domain file name
     met <- gsub('@MET_PATH@',settings$run$inputs$met$path, met)
     met.files <- dir(settings$run$inputs$met$path,"*.nc")
     met <- gsub('@MET_FILES@',paste(met.files,collapse = "\n            "), met)
     writeLines(met, con=file.path(local.rundir, "datm.streams.txt.PEcAn_met"))
     
   }
#   ... need to set this up so that if MET is blank it can run with default CLM met
#   ... fill in this template, the met template, and then have jobs.sh put them in the right place. 
#   ... Test, then adjust DB to have met required
   
   
   ##-----------------------------------------------------------------------##
   ##                                                                       ##
   ##                             JOB.SH                                    ##
   ##                                                                       ##
   ##-----------------------------------------------------------------------##
   
# create launch script (which will create symlink)
   if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
     jobsh <- readLines(con=settings$model$jobtemplate, n=-1)
   } else {
     jobsh <- readLines(con=system.file("template.job", package = "PEcAn.FATES"), n=-1)
   }
   
 # create host specific setttings
   hostsetup <- ""
   if (!is.null(settings$model$prerun)) {
     hostsetup <- paste(hostsetup, sep="\n", paste(settings$model$prerun, collapse="\n"))
   }
   if (!is.null(settings$host$prerun)) {
     hostsetup <- paste(hostsetup, sep="\n", paste(settings$host$prerun, collapse="\n"))
   }

   hostteardown <- ""
   if (!is.null(settings$model$postrun)) {
     hostteardown <- paste(hostteardown, sep="\n", paste(settings$model$postrun, collapse="\n"))
   }
   if (!is.null(settings$host$postrun)) {
     hostteardown <- paste(hostteardown, sep="\n", paste(settings$host$postrun, collapse="\n"))
   }

# create job.sh
   jobsh <- gsub('@HOST_SETUP@', hostsetup, jobsh)
   jobsh <- gsub('@HOST_TEARDOWN@', hostteardown, jobsh)
   
   ## Machine configs
   #   ./create_newcase -case @CASEDIR@ -res 1x1_brazil -compset ICLM45ED -mach @MACHINE@ -compiler @COMPILER@ -project @PROJECT@
   if (!is.null(settings$model$machine)) {
     machine <- paste(settings$model$machine, collapse="\n") 
   } else {
     machine <- "eddi"
   }
   jobsh <- gsub('@MACHINE@', machine, jobsh)
   if (!is.null(settings$model$compiler)) {
     compiler <- paste(settings$model$compiler, collapse="\n")
   } else {
     compiler <- "gnu"
   }
   jobsh <- gsub('@COMPILER@', compiler, jobsh)
   if (!is.null(settings$model$resolution)) {
     resolution <- paste(settings$model$resolution, collapse="\n")
   } else {
     resolution <- "1x1_brazil"
   }
   jobsh <- gsub('@RES@', resolution, jobsh)
   if (!is.null(settings$model$compset)) {
     compset <- paste(settings$model$compset, collapse="\n")
   } else {
     compset <- "I2000Clm50FatesGs"
   }
   jobsh <- gsub('@COMPSET@', compset, jobsh)
   if (!is.null(settings$model$project)) {
     project <- paste(settings$model$project, collapse="\n")
   } else {
     project <- "pecan"
   }
   jobsh <- gsub('@PROJECT@', project, jobsh)
   
   ## PATHS
   jobsh <- gsub('@RUNDIR@', rundir, jobsh)
   jobsh <- gsub('@CASEDIR@', casedir, jobsh)
   jobsh <- gsub('@OUTDIR@', outdir, jobsh)
   jobsh <- gsub('@REFCASE@', refcase, jobsh)
   jobsh <- gsub('@BLD@', bld, jobsh)
   jobsh <- gsub('@BINARY@', binary, jobsh)
   jobsh <- gsub('@INDIR@', indir, jobsh)
   jobsh <- gsub('@DEFAULT@', default, jobsh)
   jobsh <- gsub('@SITE_NAME@', site_name, jobsh) 
  
   ## DATES -> ENV_RUN
   jobsh <- gsub('@START_DATE@', start_date, jobsh)
   jobsh <- gsub('@STOP_N@', stop_n, jobsh)
   jobsh <- gsub('@RUN_ID@', run.id, jobsh)
   
   ## MET --> DATM
#   jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
   ## FOR FIRST STEP, CAN USE DEFAULT
   
   writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
   Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
#   
   # Write PARAMETER file for CTSM or ELM 
   do.call(paste('PEcAn.', modeltype, "::write_params.", modeltype))
   write_params_fates(trait.values = trait.values, settings = settings, run.id = run.id)   
   
   # Write SETTINGS file
#     
}
#---------------------------------------------------------------------------------------------------------------------#
### EOF
