#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a JULES config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.JULES
##' @title Write JULES configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for JULES for given run
##' @author Mike Dietze, Rob Kooper
##' 
##' @export
##-------------------------------------------------------------------------------------------------#
write.config.JULES <- function(defaults, trait.values, settings, run.id){
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)

  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$run$jobtemplate) && file.exists(settings$run$jobtemplate)) {
    jobsh <- readLines(con=settings$run$jobtemplate, n=-1)
  } else {
    jobsh <- readLines(con=system.file("template.job", package = "PEcAn.JULES"), n=-1)
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
  jobsh <- gsub('@OUTDIR@', outdir, jobsh)
  jobsh <- gsub('@RUNDIR@', rundir, jobsh)
  jobsh <- gsub('@RUNID@', run.id, jobsh)
  jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  #-----------------------------------------------------------------------
  ### Copy templated NAMELIST files to rundir
  if(!is.null(settings$model$config) && dir.exists(settings$model$config)){
    template.dir = settings$model$config
  } else {
    template.dir = file.path(system.file(package = "PEcAn.JULES"),paste0("template_nml_", settings$model$revision))
  }
  system2("cp",args = paste0(template.dir,"/* ",rundir))
  
  ## ------------------ Detect time step of met data ------------------
  met.file = dir(dirname(settings$run$inputs$met$path),pattern = basename(settings$run$inputs$met$path),full.names = TRUE)[1]
  met.header = system(paste("ncdump -h ",met.file),intern = TRUE)
  id = grep("time:delta_t",met.header) 
  if(length(id)>0){
    delta_t = met.header[id]
    #Example: delta_t = '		time:delta_t = "0000-00-00 03:00:00" ;'
    dt = diff(strptime(c("00:00:00",substring(strsplit(delta_t,'"')[[1]][2],11)),format="%T"))
    units(dt) <- "secs"
  } else {
    id = grep("time_coverage_resolution",met.header)
    if(length(id)>0){
      delta_t = met.header[id]
      dt = strsplit(strsplit(delta_t,'"')[[1]][2]," ")[[1]]
      if(dt[2]=="min") {dt[1] = as.numeric(dt[1])*60; dt[2]="sec"}
      if(dt[2]=="sec") {
        dt = dt[1]
      } else {
        print(c("write.config.JULES timestep not detected",dt))
        dt = 1800
      }
    } else {
      print(c("write.config.JULES timestep not detected",dt))
      dt = 1800
    }
  }
  ## --------------------  END DETECT TIMESTEP --------------------
  
  ## Edit DRIVE.NML to set met variables
  drive.file <- file.path(rundir,"drive.nml")
  drive.text <- readLines(con=drive.file, n=-1)
  drive.text <- gsub('@MET_START@', settings$run$site$met.start, drive.text)
  drive.text <- gsub('@MET_END@', settings$run$site$met.end, drive.text)
  drive.text <- gsub('@SITE_MET@', settings$run$inputs$met$path, drive.text)
  drive.text <- gsub('@DT@',as.numeric(dt),drive.text)
  writeLines(drive.text, con = drive.file)

  ## Edit TIMESTEPS.NML to set start/end date
  timesteps.file <- file.path(rundir,"timesteps.nml")
  timesteps.text <- readLines(con=timesteps.file, n=-1)
  timesteps.text <- gsub('@START_DATE@', format(as.Date(settings$run$start.date),"%F %H:%M:%S"), timesteps.text)
  timesteps.text <- gsub('@END_DATE@', format(as.Date(settings$run$end.date),"%F %H:%M:%S"), timesteps.text)
  writeLines(timesteps.text, con = timesteps.file)
  
  ## Edit MODEL_GRID.NML to set lat/lon
  grid.file <- file.path(rundir,"model_grid.nml")
  grid.text <- readLines(con=grid.file, n=-1)
  grid.text <- gsub('@SITE_LAT@', settings$run$site$lat, grid.text)
  grid.text <- gsub('@SITE_LON@', settings$run$site$lon, grid.text)  
  writeLines(grid.text, con = grid.file)
  
  ## Edit OUTPUT.NML to set run.id
  output.file <- file.path(rundir,"output.nml")
  output.text <- readLines(con=output.file, n=-1)
  output.text <- gsub('@RUNID@', run.id, output.text)
  output.text <- gsub('@OUTDIR@', outdir, output.text)
  writeLines(output.text, con = output.file)
 
  ## Edit ANCILLARIES.NML
  # tile frac
  # soil physical parameters
  
  ## Edit INITIAL_CONDITIONS.NML
  # soil carbon
  # LAI
  
  
  ## Edit PFT_PARAMS.NML to set model parameters
  ## ******************************************************
  
 

}
