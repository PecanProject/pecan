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
write.config.PRELES <- function(defaults, trait.values, settings, run.id) {
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  ### Define PARAMETERS
  filename <- paste(rundir, "/", "PRELES_params.", run.id, ".Rdata", sep = "")
  preles.params <- save(trait.values, file = filename)
  
  #-----------------------------------------------------------------------
  
  ### WRITE JOB.SH
  jobsh <- paste0("#!/bin/bash\n",
                  'echo "',
                  ' library(PEcAn.PRELES); runPRELES.jobsh(',
                  "'",settings$run$inputs$met$path,"',",
                  "'",outdir,"',",
                  "'",filename,"',",
                  "'",settings$run$site$lat,"',",
                  "'",settings$run$site$lon,"',",
                  "'",settings$run$start.date,"',",
                  "'",settings$run$end.date,"') ",
                  '" | R --vanilla'
  )
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
} # write.config.PRELES
