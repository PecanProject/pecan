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
PREFIX_XML <- "<?xml version=\"1.0\"?>\n"
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
##' @importFrom udunits2 ud.convert
convert.samples.MAAT <- function(trait.samples) {
  
  ### Convert object
  if (is.list(trait.samples)) {
    trait.samples <- as.data.frame(trait.samples)
  }
  
  ### first rename variables
  trait.names <- colnames(trait.samples)
  trait.names[trait.names == "leaf_respiration_rate_m2"]   <- "atref.rd"
  trait.names[trait.names == "Vcmax"]                      <- "atref.vcmax"
  trait.names[trait.names == "Jmax"]                       <- "atref.jmax"
  trait.names[trait.names == "Ev_Arrhenius"]               <- "Ha.vcmax"  # Arrhenius activation energy
  trait.names[trait.names == "Ej_Arrhenius"]               <- "Ha.jmax"  # Arrhenius activation energy
  trait.names[trait.names == "Ha_Modified_Arrhenius_Jmax"] <- "Ha.jmax"  # !!TODO: Allow for the same prior to update both Vcmax and Jmax
  trait.names[trait.names == "Hd_Modified_Arrhenius_Jmax"] <- "Hd.jmax"  # !!TODO: Allow for the same prior to update both Vcmax and Jmax
  trait.names[trait.names == "stomatal_slope"]             <- "g1_leuning"
  trait.names[trait.names == "stomatal_slope.g1"]          <- "g1_medlyn"
  trait.names[trait.names == "stomatal_slope.BB"]          <- "g1_ball"
  colnames(trait.samples) <- trait.names
  
  ### Conversions -- change to only use if Collatz, should also provide standard Rd oputput
  if ("atref.rd" %in% names(trait.samples)) {
    ## Calculate dark_resp_factor - rd as a proportion of Vcmax, Williams & Flannagan 1998 ~ 0.1
    ## (unitless)
    trait.samples[["rd_prop_vcmax"]] <- trait.samples[["atref.rd"]] / trait.samples[["atref.vcmax"]]
  }
  if ("Ha.vcmax" %in% names(trait.samples)) {
    ## Convert from kJ mol-1 to J mol-1
    trait.samples <- transform(trait.samples, Ha.vcmax = ud.convert(Ha.vcmax, "kJ", "J"))
  }
  if ("Ha.jmax" %in% names(trait.samples)) {
    ## Convert from kJ mol-1 to J mol-1
    trait.samples <- transform(trait.samples, Ha.jmax = ud.convert(Ha.jmax, "kJ", "J"))
  }
  if ("Hd.jmax" %in% names(trait.samples)) {
    ## Convert from kJ mol-1 to J mol-1
    trait.samples <- transform(trait.samples, Hd.jmax = ud.convert(Hd.jmax, "kJ", "J"))
  }
  
  ### Return trait.samples as modified by function
  return(trait.samples)
} # convert.samples.MAAT
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
##' @author Shawn Serbin, Anthony Walker, Rob Kooper
##' @importFrom XML saveXML addChildren
write.config.MAAT <- function(defaults = NULL, trait.values, settings, run.id) {
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  ### Move model files to run dirs. Use built-in MAAT script setup_MAAT_project.bs changed to below as
  ### advised by Rob Kooper, 20160405
  system2(file.path(settings$model$binary, "run_scripts/setup_MAAT_project.bs"), 
          c(rundir, file.path(settings$model$binary, "run_scripts"), 
            file.path(settings$model$binary, "src")))
  
  ### Parse config options to XML
  xml <- PEcAn.settings::listToXml(settings$model$config, "default")
  
  ### Run rename and conversion function on PEcAn trait values
  traits <- convert.samples.MAAT(trait.samples = trait.values[[settings$pfts$pft$name]])
  
  ### Convert traits to list
  traits.list <- as.list(traits)
  traits.xml <- PEcAn.settings::listToXml(traits.list, "pars")
  
  ### Finalize XML
  xml[[1]] <- addChildren(xml[[1]], traits.xml)
  
  ### Write out new XML _ NEED TO FIX THIS BIT. NEED TO CONVERT WHOLE LIST TO XML saveXML(xml, file =
  ### file.path(settings$rundir, run.id, 'leaf_default.xml'), indent=TRUE, prefix = PREFIX_XML)
  saveXML(xml, 
          file = file.path(settings$rundir, run.id, "leaf_user_static.xml"), 
          indent = TRUE, 
          prefix = PREFIX_XML)
  
  ### Write out new XML  _ NEED TO FIX THIS BIT. NEED TO CONVERT WHOLE LIST TO XML
  #saveXML(xml, file = file.path(settings$rundir, run.id, "leaf_default.xml"), indent=TRUE, prefix = PREFIX_XML)
  if (is.null(settings$run$inputs$met)) {
    PEcAn.logger::logger.info("-- No met selected. Running without a met driver --")
    jobsh <- paste0("#!/bin/bash\n","Rscript ",rundir,"/run_MAAT.R"," ",
                    "\"odir <- ","'",outdir,"'","\""," > ",rundir,
                    "/logfile.txt","\n",'echo "',
                    ' library(PEcAn.MAAT); model2netcdf.MAAT(',
                    "'",outdir,"',",
                    settings$run$site$lat,",",
                    settings$run$site$lon,", '",
                    settings$run$start.date,"', '",
                    settings$run$end.date,"') ",
                    '" | R --vanilla')
    # Run with met drivers 
    # !!Need to update for running with met, needs to paste mdir (met dir) to command !!
  } else if (!is.null(settings$run$inputs$met)) {
    met.dir <- dirname(settings$run$inputs$met$path)
    met.file <- basename(settings$run$inputs$met$path)
    file.copy(file.path(met.dir, list.files(met.dir, "*.xml")), 
              rundir, 
              overwrite = TRUE, 
              recursive = FALSE, 
              copy.mode = TRUE, 
              copy.date = TRUE)
    jobsh <- paste0("#!/bin/bash\n","Rscript ",rundir,"/run_MAAT.R"," ",
                    "\"odir <- ","'",outdir,"'","\""," ","\"mdir <- ","'",met.dir,"'",
                    "\""," ","\"metdata <- ","'",met.file,"'","\""," > ",rundir,
                    "/logfile.txt","\n",'echo "',
                    ' library(PEcAn.MAAT); model2netcdf.MAAT(',
                    "'",outdir,"',",
                    settings$run$site$lat,",",
                    settings$run$site$lon,", '",
                    settings$run$start.date,"', '",
                    settings$run$end.date,"') ",
                    '" | R --vanilla')
  }  #End if/else
  
  # Write the job.sh script
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
} # write.config.MAAT

##-------------------------------------------------------------------------------------------------#
## EOF
