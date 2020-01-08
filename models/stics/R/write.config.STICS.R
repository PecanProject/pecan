#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes STICS configurations.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.STICS
##' @title Write STICS configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @return configuration file for STICS for given run
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
write.config.STICS <- function(defaults, trait.values, settings, run.id) {
  
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  # read in template plt file, has all the formalisms
  plt_xml  <- XML::xmlParse(system.file("crop_plt.xml", package = "PEcAn.STICS"))
  plt_list <- XML::xmlToList(plt_xml)
  
  
  for (pft in seq_along(trait.values)) {
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names  <- names(pft.traits)
    
    # go over each formalism and replace params following the order in crop_plt
    # for now I vary only one parameter under roots.
    
    # plant name and group
    # effect of atmospheric CO2 concentration
    # phasic development
    # emergence and starting
    # leaves
    # radiation interception
    # shoot biomass growth
    # partitioning of biomass in organs
    # yield formation
    
    # roots
    
    # specific root length (cm g-1)
    # plt_list[[10]][[6]][[2]][[4]] position
    if ("SRL" %in% pft.names) {
      srl_val  <- udunits2::ud.convert(pft.traits[which(pft.names == "SRL")], "m", "cm")
      plt_list <- plt_list %>% modify_depth(-1, ~if(all(.x == "@longsperac@")) srl_val else .x)
      
    }
    
    # frost
    # water
    # nitrogen
    # correspondance code BBCH
    # cultivar parameters
    
    # write back
    if(names(trait.values)[pft] != "env"){
      
      saveXML(PEcAn.settings::listToXml(plt_list, "fichierplt"), 
              file = file.path(rundir, paste0(names(trait.values)[pft], "_plt.xml")), 
              prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
      
    }
    
  } # pft-loop ends
  

  

  
  data_dir <- "/fs/data3/istfer/STICS/example/"
  stics_path <- "/fs/data3/istfer/STICS/bin/stics_modulo"
  obs <- SticsRFiles::read_obs_to_list(data_dir)
  stics_options_no_par <- SticsOnR::stics_wrapper_options(stics_path = stics_path, data_dir = data_dir, time_display = TRUE)
  res <- SticsOnR::stics_wrapper(model_options =  stics_options_no_par, sit_var_dates_mask = obv)
  
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.STICS"), n = -1)
  }
  
  # create host specific setttings
  hostsetup <- ""
  if (!is.null(settings$model$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$model$prerun, collapse = "\n"))
  }
  if (!is.null(settings$host$prerun)) {
    hostsetup <- paste(hostsetup, sep = "\n", paste(settings$host$prerun, collapse = "\n"))
  }
  
  hostteardown <- ""
  if (!is.null(settings$model$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$model$postrun, collapse = "\n"))
  }
  if (!is.null(settings$host$postrun)) {
    hostteardown <- paste(hostteardown, sep = "\n", paste(settings$host$postrun, collapse = "\n"))
  }
  
  # create job.sh
  jobsh <- gsub("@HOST_SETUP@", hostsetup, jobsh)
  jobsh <- gsub("@HOST_TEARDOWN@", hostteardown, jobsh)
  
  jobsh <- gsub("@SITE_LAT@", settings$run$site$lat, jobsh)
  jobsh <- gsub("@SITE_LON@", settings$run$site$lon, jobsh)
  jobsh <- gsub("@SITE_MET@", settings$run$site$met, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  #-----------------------------------------------------------------------
  ### Edit a templated config file for runs
  if (!is.null(settings$model$config) && file.exists(settings$model$config)) {
    config.text <- readLines(con = settings$model$config, n = -1)
  } else {
    filename <- system.file(settings$model$config, package = "PEcAn.MODEL")
    if (filename == "") {
      if (!is.null(settings$model$revision)) {
        filename <- system.file(paste0("config.", settings$model$revision), package = "PEcAn.STICS")
      } else {
        model <- PEcAn.DB::db.query(paste("SELECT * FROM models WHERE id =", settings$model$id), params = settings$database$bety)
        filename <- system.file(paste0("config.r", model$revision), package = "PEcAn.STICS")
      }
    }
    if (filename == "") {
      PEcAn.logger::logger.severe("Could not find config template")
    }
    PEcAn.logger::logger.info("Using", filename, "as template")
    config.text <- readLines(con = filename, n = -1)
  }
  
  config.text <- gsub("@SITE_LAT@", settings$run$site$lat, config.text)
  config.text <- gsub("@SITE_LON@", settings$run$site$lon, config.text)
  config.text <- gsub("@SITE_MET@", settings$run$inputs$met$path, config.text)
  config.text <- gsub("@MET_START@", settings$run$site$met.start, config.text)
  config.text <- gsub("@MET_END@", settings$run$site$met.end, config.text)
  config.text <- gsub("@START_MONTH@", format(settings$run$start.date, "%m"), config.text)
  config.text <- gsub("@START_DAY@", format(settings$run$start.date, "%d"), config.text)
  config.text <- gsub("@START_YEAR@", format(settings$run$start.date, "%Y"), config.text)
  config.text <- gsub("@END_MONTH@", format(settings$run$end.date, "%m"), config.text)
  config.text <- gsub("@END_DAY@", format(settings$run$end.date, "%d"), config.text)
  config.text <- gsub("@END_YEAR@", format(settings$run$end.date, "%Y"), config.text)
  config.text <- gsub("@OUTDIR@", settings$host$outdir, config.text)
  config.text <- gsub("@ENSNAME@", run.id, config.text)
  config.text <- gsub("@OUTFILE@", paste0("out", run.id), config.text)
  
  #-----------------------------------------------------------------------
  config.file.name <- paste0("CONFIG.", run.id, ".txt")
  writeLines(config.text, con = paste(outdir, config.file.name, sep = ""))
} # write.config.STICS
