#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##------------------------------------------------------------------------------------------------#
##' convert parameters, do unit conversions and update parameter names from PEcAn database default 
##' to units/names within dvmdostem
##'
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' 
##' @name convert.samples.dvmdostem
##' @title Convert samples for dvmdostem
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @export
##' @author Shawn Serbin, Tobey Carman
##' @importFrom udunits2 ud.convert

convert.samples.dvmdostem <- function(trait.samples) {
  
  ### Convert object
  if (is.list(trait.samples)) {
    trait.samples <- as.data.frame(trait.samples)
  }

  ### first rename variables (needed??)

  ### Conversions (for example, convert SLA to m2/g?)
  
  ### Return trait.samples as modified by function
  return(trait.samples)
} # convert.samples.dvmdostem
##-------------------------------------------------------------------------------------------------#


##-------------------------------------------------------------------------------------------------#
##' Writes a dvmdostem PEcAn config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.dvmdostem
##' @title Write dvmdostem model configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return none
##' @export
##' @author Tobey Carman, Shawn Serbin
##' @importFrom ncdf4 ncvar_put ncvar_get
##'
write.config.dvmdostem <- function(defaults = NULL, trait.values, settings, run.id) {

   ## site information
   site <- settings$run$site
   site.id <- as.numeric(site$id)
   
   # find out where things are
   local.rundir <- file.path(settings$rundir, run.id) ## this is on local machine for staging
   rundir     <- file.path(settings$host$rundir, run.id)  ## this is on remote machine for execution
   outdir <- file.path(settings$host$outdir, run.id)
   
   npft <- length(trait.values)
   PEcAn.utils::logger.debug(npft)
   PEcAn.utils::logger.debug(dim(trait.values))
   PEcAn.utils::logger.debug(names(trait.values))

   # 1) Read in a parameter data block from dvmdostem
   # system2(command, args = character(),
   #         stdout = "", stderr = "", stdin = "", input = NULL,
   #         env = character(), wait = TRUE,
   #         minimized = FALSE, invisible = TRUE, timeout = 0)
   # Not sure how to check exit code??
   # Not sure what we need to do with stderr...
   dvmpath <- '/data/software/dvm-dos-tem'
   params <- paste(dvmpath,"parameters",'cmt_dimvegetation.txt',sep="/")
   json_file <- '/tmp/junk.json'
   community_type <- '04'
   system2(paste0(dvmpath,"/scripts/param_util.py"), args=(c("--dump-block-to-json",params,community_type)), 
                                                   stdout=json_file, wait=TRUE)
   
   # 2) Overwrite certain parameter values with (ma-posterior) trait data from pecan
   # In R, need to open "some/tmp/file.json" as a json object, overwrite some values, and write it back out
   library("rjson")
   # Read in the file
   json_data <- fromJSON(paste(readLines(json_file), collapse=""))
   # Overwrite the data
   #var <- "sla"
   #json_data$pft1[[var]]
   
   for (i in names(json_data)){
     if (grepl("pft",i)){
       pft_name <- json_data[[i]]$name
       print(pft_name)
       short_bety_pft <- strip(...settings$pfts$pft$name)
       if (pft_name==short_bety_pft){
         print(i)
         parameter <- "sla"
         json_data[[i]][[parameter]] = 
       }
     } else {
       "Not the droid we are looking for"
     }
   }
   

   
   json_data$pft9$sla = 
   # Write it back out to disk (overwriting ok??)
   exportJson <- toJSON(json_data)
   write(exportJson, "some/tmp/file.json")

   # Get a copy of the config file written into the run directory with the 
   # appropriate template parameters substituted.
   if (!is.null(settings$model$configtemplate) && file.exists(settings$model$configtemplate)) {
     config_template <- readLines(con=settings$model$config_template, n=-1)
   } else {
     config_template <- readLines(con=system.file("config.js.template", package = "PEcAn.dvmdostem"), n=-1)
   }
   # 3) Write parameter file back out to dvmdostem parameter file
   # Need to 
   system2("dvmdostem/scripts/param_util.py" args=("--fmt-block-from-json some/tmp/file.json ?<REF FILE>?", stdout="<some parameter file for dvmdostem to runwith ...>", wait=TRUE,)
 

   config_template <- gsub("@INPUT_DATA_DIR@", file.path(dirname(binary), "DATA/SewardPen_10x10"), config_template)
   config_template <- gsub("@MODEL_OUTPUT_DIR@", outdir, config_template)

   if (! file.exists(file.path(settings$rundir, run.id,"config"))) dir.create(file.path(settings$rundir, run.id,"config"), recursive = TRUE)

   writeLines(config_template, con=file.path(settings$rundir, run.id,"config/config.js"))

   ### create launch script (which will create symlink) - needs to be created
   # if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
   #   jobsh <- readLines(con=settings$model$jobtemplate, n=-1)
   # } else {
   #   jobsh <- readLines(con=system.file("template.job", package = "PEcAn.FATES"), n=-1)
   # }

   ### create host specific setttings
   # hostsetup <- ""
   # if (!is.null(settings$model$prerun)) {
   #   hostsetup <- paste(hostsetup, sep="\n", paste(settings$model$prerun, collapse="\n"))
   # }
   # if (!is.null(settings$host$prerun)) {
   #   hostsetup <- paste(hostsetup, sep="\n", paste(settings$host$prerun, collapse="\n"))
   # }
   # 
   # hostteardown <- ""
   # if (!is.null(settings$model$postrun)) {
   #   hostteardown <- paste(hostteardown, sep="\n", paste(settings$model$postrun, collapse="\n"))
   # }
   # if (!is.null(settings$host$postrun)) {
   #   hostteardown <- paste(hostteardown, sep="\n", paste(settings$host$postrun, collapse="\n"))
   # }



}






