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
PREFIX_XML <- '<?xml version="1.0"?>\n'
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
    
    ### Convert object
    if(is.list(trait.samples)) trait.samples <- as.data.frame(trait.samples)

    ### first rename variables
    trait.names <- colnames(trait.samples)
    trait.names[trait.names == "Vcmax"] <- "atref.vcmax"
    trait.names[trait.names == "Jmax"] <- "atref.jmax"
    colnames(trait.samples) <- trait.names
    
    ### Return trait.samples as modified by function
    return(trait.samples)
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

  ### Move model files to run dirs. Use built-in MAAT script setup_MAAT_project.bs
  #  system(paste0(settings$model$binary,'./run_scripts/setup_MAAT_project.bs'," ",rundir," ",
  #settings$model$binary,"/run_scripts"," ",settings$model$binary,"/src"))
  
  # changed to below as advised by Rob Kooper, 20160405
  system2(file.path(settings$model$binary, 'run_scripts/setup_MAAT_project.bs'),
  c(rundir, file.path(settings$model$binary, "run_scripts"),  file.path(settings$model$binary, "src")))
  
  ### Parse config options to XML
  xml <- listToXml(settings$model$config, "default")

  ### Run rename and conversion function on PEcAn trait values
  traits  <- convert.samples.MAAT(trait.samples = trait.values[[settings$pfts$pft$name]])

  ### Convert traits to list
  traits.list <- as.list(traits)
  traits.xml <- listToXml(traits.list, 'pars')
  
  ### Finalize XML
  #xml <- append.xmlNode(xml, traits.xml) # append new child?  insert node?
  xml[[1]] <- addChildren(xml[[1]], traits.xml)
  
  ### Write out new XML  _ NEED TO FIX THIS BIT. NEED TO CONVERT WHOLE LIST TO XML
  #saveXML(xml, file = file.path(settings$rundir, run.id, "leaf_default.xml"), indent=TRUE, prefix = PREFIX_XML)
  saveXML(xml, file = file.path(settings$rundir, run.id, "leaf_user_static.xml"), indent=TRUE, prefix = PREFIX_XML)
  
  ### Write out the job.sh file - will be used to run the model code in the correct PEcAn run folder
  jobsh <- paste0("#!/bin/bash\n","Rscript ",rundir,"/run_MAAT.R"," ",
                  "\"odir <- ","'",outdir,"'","\""," > ",rundir,
                  "/logfile.txt","\n",'echo "',
                  ' require(PEcAn.MAAT); model2netcdf.MAAT(',
                  "'",outdir,"',",
                  settings$run$site$lat,",",
                  settings$run$site$lon,", '",
                  settings$run$start.date,"', '",
                  settings$run$end.date,"') ",
                  '" | R --vanilla')
                
  writeLines(jobsh, con=file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  #print(warnings())
}
##-------------------------------------------------------------------------------------------------#
