#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##-------------------------------------------------------------------------------------------------#
## Functions to prepare and write out MAAT model xml files for MA, SA, and Ensemble runs
PREFIX_XML <- '<?xml version="1.0"?>'
##-------------------------------------------------------------------------------------------------#


##------------------------------------------------------------------------------------------------#
##' convert parameters and parameter names from PEcAn database default units/names with MAAT
##'
##' Performs model specific unit conversions on a a list of trait values,
##' such as those provided to write.config
##' @name convert.samples.MAAT
##' @title Convert samples for MAAT
##' @param trait.samples a matrix or dataframe of samples from the trait distribution
##' @return matrix or dataframe with values transformed
##' @export
##' @author Shawn Serbin, Anthony Walker
convert.samples.MAAT <- function(trait.samples){
    
    if(is.list(trait.samples)) trait.samples <- as.data.frame(trait.samples)
    ## first rename variables
    trait.names <- colnames(trait.samples)
    print(trait.names)
}
##-------------------------------------------------------------------------------------------------#
    
##-------------------------------------------------------------------------------------------------#
##' Writes a MAAT config file.
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
write.config.MAAT <- function(defaults=NULL, trait.values, settings, run.id){
  
  # find out where to write run/ouput
  rundir <- file.path(settings$run$host$rundir, run.id)
  outdir <- file.path(settings$run$host$outdir, run.id)

  ### Define run parameters for MAAT
  #print(rundir) # for debugging. turn off when working
  #print(outdir) # for debugging. turn off when working
  #print(run.id) # for debugging. turn off when working
  #print(" Under development ") # for debugging. turn off when working

  ## Move model files to run dirs. Use built-in MAAT script setup_MAAT_project.bs
  system(paste0(settings$model$binary,'./run_scripts/setup_MAAT_project.bs'," ",rundir," ",
  settings$model$binary,"/run_scripts"," ",settings$model$binary,"/src"))
  
  ### Read in XML defaults
  xml.file <- paste0(rundir,"/leaf_default.xml")  # could move this up to the call and use where defaults=NULL
  leaf.defaults <- xmlParse(xml.file)
  
  ### Overwrite XML defaults
  leaf.defaults.list <- xmlToList(leaf.defaults)
  #print(leaf.defaults.list)
  
  #names(trait.values)
  #str(trait.values)
  
  #pft.traits <- which(!(names(trait.values) %in% 'env'))[1]
  #pft.traits <- unlist(trait.values[[pft.traits]])
  #pft.names  <- names(pft.traits)
  #print(pft.names)
}
##-------------------------------------------------------------------------------------------------#