#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a MAAT config file.
##'
##' NOTE: !Under development!
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.MAAT
##' @title Write MAAT model configuration files
##' @param defaults list of defaults to process
##' @param trait.samples vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for MAAT for given run
##' @export
##' @author Shawn Serbin, Anthony Walker
##-------------------------------------------------------------------------------------------------#
write.config.MAAT <- function(defaults=NULL, trait.values, settings, run.id){
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)
  
  ### Define run parameters for MAAT
  print(rundir)
  print(outdir)
  print(" Under development ")
  
}
