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

   # create launch script (which will create symlink) - needs to be created
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



}






