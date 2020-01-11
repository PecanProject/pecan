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
##' @importFrom dplyr "%>%"
##' @return configuration file for STICS for given run
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
write.config.STICS <- function(defaults, trait.values, settings, run.id) {
  
  ## simulation days, used later
  dseq <- seq(lubridate::as_date(settings$run$start.date), lubridate::as_date(settings$run$end.date), by = "day")
  
  # find out where to write run/ouput
  rundir  <- file.path(settings$host$rundir, run.id)
  pltdir  <- file.path(settings$host$rundir, run.id, "plant")
  cfgdir  <- file.path(settings$host$rundir, run.id, "config")
  bindir  <- file.path(settings$host$rundir, run.id, "bin")
  outdir  <- file.path(settings$host$outdir, run.id)
  
  ## make sure rundir and outdir exist
  dir.create(rundir, showWarnings = FALSE, recursive = TRUE)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  ## create plant, config and bin dirs
  dir.create(pltdir, showWarnings = FALSE, recursive = TRUE)
  dir.create(cfgdir, showWarnings = FALSE, recursive = TRUE)
  dir.create(bindir, showWarnings = FALSE, recursive = TRUE)
  
  # write preferences
  prf_xml  <- XML::xmlParse(system.file("preferences.xml", package = "PEcAn.STICS"))
  prf_list <- XML::xmlToList(prf_xml)
  prf_list$entry$text <- rundir
  
  XML::saveXML(PEcAn.settings::listToXml(prf_list, "properties"), 
          file = file.path(cfgdir, "preferences.xml"), 
          prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n<!DOCTYPE properties SYSTEM "http://java.sun.com/dtd/properties.dtd">\n')
  
  # read in template USM (Unit of SiMulation) file, has the master settings, file names etc.
  # TODO: more than one usm
  usm_xml  <- XML::xmlParse(system.file("usms.xml", package = "PEcAn.STICS"))
  usm_list <- XML::xmlToList(usm_xml)
  
  
  ################################# Prepare Plant File #######################################
  
  ## this is where we overwrite model parameters
  
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
      plt_list <- plt_list %>% purrr::modify_depth(-1, ~if(all(.x == "@longsperac@")) srl_val else .x)
      
    }
    
    # frost
    # water
    # nitrogen
    # correspondance code BBCH
    # cultivar parameters
    
    # write back
    if(names(trait.values)[pft] != "env"){
      
      XML::saveXML(PEcAn.settings::listToXml(plt_list, "fichierplt"), 
              file = file.path(pltdir, paste0(names(trait.values)[pft], "_plt.xml")), 
              prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
      
    }
    
  } # pft-loop ends
  
  
  
  
  ############################ Prepare Initialization File ##################################
  
  ## this is where we overwrite model initial conditions
  
  # read in template ini file
  ini_xml  <- XML::xmlParse(system.file("pecan_ini.xml", package = "PEcAn.STICS"))
  ini_list <- XML::xmlToList(ini_xml)
  
  # DO NOTHING FOR NOW
  
  # write the ini file
  XML::saveXML(PEcAn.settings::listToXml(ini_list, "initialisations"), 
          file = file.path(rundir, paste0(defaults$pft$name, "_ini.xml")), 
          prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
  
  
  
  ############################ Prepare Soils ##################################
  
  ## this is where we modify soil characteristics
  
  # read in template sols file
  sols_xml  <- XML::xmlParse(system.file("sols.xml", package = "PEcAn.STICS"))
  sols_list <- XML::xmlToList(sols_xml)
  
  sols_list$sol$.attrs[["nom"]] <- paste0("sol", defaults$pft$name)
  
  # DO NOTHING FOR NOW
  
  # write the tec file
  XML::saveXML(PEcAn.settings::listToXml(sols_list, "sols"), 
          file = file.path(rundir, "sols.xml"), 
          prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
  
  
  ######################### Prepare Weather Station File ###############################
  
  ## this is where we modify more initial conditions and site characteristics
  
  # read in template sta file
  sta_xml  <- XML::xmlParse(system.file("pecan_sta.xml", package = "PEcAn.STICS"))
  sta_list <- XML::xmlToList(sta_xml)
  
  # change latitute
  sta_list[[1]][[3]]$text <- settings$run$site$lat
  
  # DO NOTHING ELSE FOR NOW
  
  # Should these be prepared by met2model.STICS?
  
  # write the sta file
  XML::saveXML(PEcAn.settings::listToXml(sta_list, "fichiersta"), 
          file = file.path(rundir, paste0(tolower(sub(" .*", "", settings$run$site$name)), "_sta.xml")), 
          prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
  

  
  
  
  ############################## Prepare LAI forcing ####################################
  ## skipping for now
  
  
  ############################## Param gen / newform ####################################
  
  ## DO NOTHING
  gen_xml  <- XML::xmlParse(system.file("param_gen.xml", package = "PEcAn.STICS"))
  gen_list <- XML::xmlToList(gen_xml)
  XML::saveXML(PEcAn.settings::listToXml(gen_list, "fichierpar"), 
               file = file.path(rundir, "param_gen.xml"), 
               prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
  
  newf_xml  <- XML::xmlParse(system.file("param_newform.xml", package = "PEcAn.STICS"))
  newf_list <- XML::xmlToList(newf_xml)
  XML::saveXML(PEcAn.settings::listToXml(newf_list, "fichierparamgen"), 
               file = file.path(rundir, "param_newform.xml"), 
               prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')

  
  ############################ Prepare Technical File ##################################
  
  ## this is where we modify management practices
  
  # read in template tec file
  tec_xml  <- XML::xmlParse(system.file("pecan_tec.xml", package = "PEcAn.STICS"))
  tec_list <- XML::xmlToList(tec_xml)
  
  # If harvest file is given, use given dates 
  # this will need  more complicated checks and file formats
  if(!is.null(settings$run$inputs$harvest)){
    
    h_days <- as.matrix(utils::read.table(settings$run$inputs$harvest$path, header = TRUE, sep = ","))
    
    # probably should use nicer list manipulation techniques, but these template files are static for now
    # save last list to add at the end
    attr_sublist <- tec_list[[8]][[1]][[1]][[2]][[2]][[1]][[4]] 
    tec_list[[8]][[1]][[1]][[2]][[2]][[1]][[4]] <- NULL
    
    list_no <- 2
    
    for(hrow in seq_len(nrow(h_days))){
      
     
     harvest_list <- tec_list[[8]][[1]][[1]][[2]][[2]][[1]][[2]] # refreshing the "template"
     intervention_names <- names(tec_list[[8]][[1]][[1]][[2]][[2]][[1]][[2]])
     
     # If given harvest date is within simulation days
     if(as.Date(h_days[hrow, 2], origin = paste0(h_days[hrow, 1], "-01-01")) %in% dseq){
       
       # STICS needs cutting days in cumulative julian days 
       # e.g. first cutting day of the first simulation year can be 163 (2018-06-13)
       # in following years it should be cumulative, meaning a cutting day on 2019-06-12 is 527, not 162
       # the following code should give that
       harvest_list$colonne$text <- which(dseq == as.Date(h_days[hrow, 2], origin = paste0(h_days[hrow, 1], "-01-01"))) + lubridate::yday(dseq[1]) - 2
       
       tec_list[[8]][[1]][[1]][[2]][[2]][[1]][[list_no]] <- harvest_list
       
       list_no <- list_no + 1
     }
     
    }
    
    # this means we have prescribed more than 2 cutting days
    if(list_no > 4){
      names(tec_list[[8]][[1]][[1]][[2]][[2]][[1]])[3:(list_no-1)] <- "intervention"
      
      #add the last sublist back
      attr_sublist["nb_interventions"] <- list_no - 2
      tec_list[[8]][[1]][[1]][[2]][[2]][[1]][[".attrs"]] <- attr_sublist
     
    }
    

  }
  
  # OTHERWISE DO NOTHING FOR NOW
  
  # write the tec file
  XML::saveXML(PEcAn.settings::listToXml(tec_list, "fichiertec"), 
          file = file.path(rundir, paste0(defaults$pft$name, "_tec.xml")), 
          prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
  
  
  
  
  ################################ Prepare USM file ######################################

  # TODO: more than 1 USM and PFTs (STICS can run 2 PFTs max: main crop + intercrop)
  
  # pft name
  usm_list$usm$.attrs[["nom"]] <- defaults$pft$name
  
  # beginning day of the simulation (julian.d)
  usm_list$usm$datedebut <- lubridate::yday(settings$run$start.date)
  
  # end day of the simulation (julian.d) (at the end of consecutive years, i.e. can be greater than 366)
  usm_list$usm$datefin <- usm_list$usm$datedebut + length(dseq) - 1
  
  # name of the initialization file
  usm_list$usm$finit <- paste0(defaults$pft$name, "_ini.xml")
  
  # name of the soil in the sols.xml file
  usm_list$usm$nomsol <- paste0("sol", defaults$pft$name)
  
  # name of the weather station file
  usm_list$usm$fstation <- paste0(tolower(sub(" .*", "", settings$run$site$name)), "_sta.xml")
  
  # name of the first climate file
  usm_list$usm$fclim1 <- paste0(tolower(sub(" .*", "", settings$run$site$name)), ".", lubridate::year(settings$run$start.date))
  
  
  # name of the last climate file
  usm_list$usm$fclim2 <- paste0(tolower(sub(" .*", "", settings$run$site$name)), ".", lubridate::year(settings$run$end.date))
  
  # number of calendar years involved in the crop cycle
  # 1 = 1 year e.g. for spring crops, 0 = two years, e.g. for winter crops
  usm_list$usm$culturean <- trait.values$timothy$crop_cycle
  
  # number of simulated plants (sole crop=1; intercropping=2)
  usm_list$usm$nbplantes <- 1 # hardcode for now
  
  # Type of LAI simulation 
  # 0 = culture (LAI calculated by the model), 1 = feuille (LAI forced)
  usm_list$usm$codesimul <- 0 # hardcode for now
  
  # name of the plant file for main plant 
  usm_list[[1]][[11]]$fplt <- paste0(defaults$pft$name, "_plt.xml")
  
  # name of the technical file for main plant
  usm_list[[1]][[11]]$ftec <- paste0(defaults$pft$name, "_tec.xml")
  
  # name of the LAI forcing file for main plant (null if none)
  usm_list[[1]][[11]]$flai <- "null" # hardcode for now
  
  # name of the plant file for associated plant (intercropping)
  usm_list[[1]][[12]]$fplt <- "null" # hardcode for now
  
  # name of the technical file for associated plant (intercropping)
  usm_list[[1]][[12]]$ftec <- "null" # hardcode for now
  
  # name of the LAI forcing file for associated plant (intercropping) (null if none)
  usm_list[[1]][[12]]$flai <- "null" # hardcode for now
  
  # write USMs
  XML::saveXML(PEcAn.settings::listToXml(usm_list, "usms"), 
          file = file.path(rundir, "usms.xml"), 
          prefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n')
  
  
  ################################ Prepare Run ######################################
  
  # symlink climate files
  met_path <- settings$run$inputs$met$path
  for(clim in seq(lubridate::year(settings$run$start.date), lubridate::year(settings$run$end.date))){
    met_file  <- gsub(paste0(lubridate::year(settings$run$start.date), ".climate"), paste0(clim, ".climate"), met_path)
    clim_file <- file.path(rundir, paste0(tolower(sub(" .*", "", settings$run$site$name)), ".", clim))
    file.symlink(met_file, clim_file)
  }

  # stics path
  stics_path <- settings$model$binary
  
  # symlink to binary
  file.symlink(stics_path, bindir)
  
  # generate STICS input files using JavaStics
  jexe <- file.path(gsub("bin","", dirname(stics_path)), "JavaSticsCmd.exe")
  
  usm_name <- defaults$pft$name
  
  cmd_generate <- paste("java -jar", jexe,"--generate-txt", rundir, usm_name)
  
  # copy *.mod files
  mod_files <- c(file.path(gsub("bin","example", dirname(stics_path)), "var.mod"),
                 file.path(gsub("bin","example", dirname(stics_path)), "rap.mod"),
                 file.path(gsub("bin","example", dirname(stics_path)), "prof.mod"))
  file.copy(mod_files, rundir)
  
  cmd_run <- paste("java -jar", jexe,"--run", rundir, usm_name)
  


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
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub("@MODFILE@", paste0("mod_s", usm_name, ".sti"), jobsh)
  
  jobsh <- gsub("@CMD_GENERATE@", cmd_generate, jobsh)
  jobsh <- gsub("@CMD_RUN@", cmd_run, jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  

} # write.config.STICS
