##' Writes config files for use with the Community Land Model model.
##'
##' @name write.config.CLM45
##' @title Write CLM4.5 configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return none
##' @export
##' @author Mike Dietze
##-------------------------------------------------------------------------------------------------#
 write.config.CLM45 <- function(defaults, trait.values, settings, run.id){
#  
# #OUTLINE OF MODULES
#   # Copy Case and Build
#   #  -symbolic link to refernce case that is already completed
#   # Edit user_nl_* files to add site info
#   # make Jobs.sh -case_submit
#   # call met2model and add to namelists
#   #
# 
#   # find out where to write run/ouput
#   rundir <- file.path(settings$host$rundir, run.id)
#   outdir <- file.path(settings$host$outdir, run.id)
#   
#   # create launch script (which will create symlink)
#   if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
#     jobsh <- readLines(con=settings$model$jobtemplate, n=-1)
#   } else {
#     jobsh <- readLines(con=system.file("template.job", package = "PEcAn.CLM45"), n=-1)
#   }
#   
  # # create host specific setttings
  # hostsetup <- ""
  # if (!is.null(settings$model$prerun)) {
  #   hostsetup <- paste(hostsetup, sep="\n", paste(settings$model$prerun, collapse="\n"))
  # }
  # if (!is.null(settings$host$prerun)) {
  #   hostsetup <- paste(hostsetup, sep="\n", paste(settings$host$prerun, collapse="\n"))
  # }

  # hostteardown <- ""
  # if (!is.null(settings$model$postrun)) {
  #   hostteardown <- paste(hostteardown, sep="\n", paste(settings$model$postrun, collapse="\n"))
  # }
  # if (!is.null(settings$host$postrun)) {
  #   hostteardown <- paste(hostteardown, sep="\n", paste(settings$host$postrun, collapse="\n"))
  # }

  # # create job.sh
  # jobsh <- gsub('@HOST_SETUP@', hostsetup, jobsh)
  # jobsh <- gsub('@HOST_TEARDOWN@', hostteardown, jobsh)

# 
#   jobsh <- gsub('@SITE_LAT@', settings$run$site$lat, jobsh)
#   jobsh <- gsub('@SITE_LON@', settings$run$site$lon, jobsh)
#   jobsh <- gsub('@SITE_MET@', settings$run$inputs$met$path, jobsh)
#   
#   jobsh <- gsub('@START_DATE@', settings$run$start.date, jobsh)
#   jobsh <- gsub('@END_DATE@', settings$run$end.date, jobsh)
#   
#   jobsh <- gsub('@OUTDIR@', outdir, jobsh)
#   jobsh <- gsub('@RUNDIR@', rundir, jobsh)
#   
#   jobsh <- gsub('@BINARY@', settings$model$binary, jobsh)
#   
#   writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
#   Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
#   
#   ## Write PARAMETER file
# 
#   ## Write SETTINGS file
#     
 }
