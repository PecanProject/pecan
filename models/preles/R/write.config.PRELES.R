#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a PRELES config file.
##'
##' @name write.config.PRELES
##' @title Write PRELES configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for PRELES for given run
##' @export
##' @author Tony Gardella, Micheal Dietze
##-------------------------------------------------------------------------------------------------#
write.config.PRELES<- function(defaults, trait.values, settings, run.id){
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)

  #-----------------------------------------------------------------------
  ### WRITE JOB.SH
  jobsh = paste0("#!/bin/bash\n",
                 'echo "',
                 ' require(PEcAn.PRELES); runPRELES.jobsh.R(',
                 "'",outdir,"',",
                 settings$run$site$lat,",",
                 settings$run$site$lon,", '",
                 settings$run$start.date,"', '",
                 settings$run$end.date,"') ",
                 '" | R --vanilla'
  )
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
}
