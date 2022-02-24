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
  
  
  # stics and javastics path
  stics_path <- settings$model$binary
  javastics_path <-  gsub("bin","", dirname(stics_path))

  
  # Per STICS development team, there are two types of STICS inputs
  # Global input: _plt.xml, param_gen.xml, param_newform.xml
  # Local input: _ini.xml (initialization), sols.xml (soils), _tec.xml (crop management), (climate files) _sta.xml, *.year
  
  # NOTE: however, it's the text files, not the xml files that are read by the STICS executable.
  
  ################################# Prepare Plant File #######################################
  
  ## this is where we overwrite model parameters
  
  # read in template plt file, has all the formalisms
  plt_xml  <- XML::xmlParse(system.file("crop_plt.xml", package = "PEcAn.STICS"))
  #plt_list <- XML::xmlToList(plt_xml)
  
  for (pft in seq_along(trait.values)) {
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names  <- names(pft.traits)
    
    plant_file <- file.path(pltdir, paste0(names(trait.values)[pft], "_plt.xml"))
    
    if(names(trait.values)[pft] != "env"){
      # save the template, will be overwritten below
      XML::saveXML(plt_xml, file = plant_file)
    }
    
    # to learn the parameters in a plant file
    # SticsRFiles::get_param_info(file_path = plant_file)
    
    # go over each formalism and replace params following the order in crop_plt
    # TODO: vary more params
    
    # plant name and group
    # effect of atmospheric CO2 concentration
    
    # phasic development
    # to see parameters per formalism
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "phasic development")
    # unlist(values)
    
    # minimum temperature below which development stops (degree C)
    if ("tdmin" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tdmin", pft.traits[which(pft.names == "tdmin")], overwrite = TRUE)
    }
    
    # maximum temperature above which development stops (degree C)
    if ("tdmax" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tdmax", pft.traits[which(pft.names == "tdmax")], overwrite = TRUE)
    }
    
    
    # emergence and starting
    # leaves
    
    # phyllotherme, thermal duration between the apparition of two successive leaves on the main stem (degree day)
    # assuming this is the same as phyllochron
    if ("phyllochron" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "phyllotherme", pft.traits[which(pft.names == "phyllochron")], overwrite = TRUE)
    }
    
    # minimal density above which interplant competition starts (m-2)
    if ("dens_comp" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "bdens", pft.traits[which(pft.names == "dens_comp")], overwrite = TRUE)
    }
    
    # LAI above which competition between plants starts (m2 m-2)
    if ("lai_comp" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "laicomp", pft.traits[which(pft.names == "lai_comp")], overwrite = TRUE)
    }
    
    # basal height of crop (m)
    if ("height" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "hautbase", pft.traits[which(pft.names == "height")], overwrite = TRUE)
    }
    
    # maximum height of crop
    if ("HTMAX" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "hautmax", pft.traits[which(pft.names == "HTMAX")], overwrite = TRUE)
    }
    
    # radiation interception
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "radiation interception")
    
    # shoot biomass growth
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "shoot biomass growth")
    
    # optimal temperature (1/2) for plant growth
    if ("teopt" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "teopt", pft.traits[which(pft.names == "teopt")], overwrite = TRUE)
    }
    
    # optimal temperature (2/2) for plant growth
    if ("teoptbis" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "teoptbis", pft.traits[which(pft.names == "teoptbis")], overwrite = TRUE)
    }
    
    # partitioning of biomass in organs
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "partitioning of biomass in organs")
    
    # maximum SLA (specific leaf area) of green leaves (cm2 g-1)
    if ("SLAMAX" %in% pft.names) {
      slamax <- pft.traits[which(pft.names == "SLAMAX")]
      slamax <- udunits2::ud.convert(udunits2::ud.convert(slamax, "m2", "cm2"), "kg-1", "g-1") # m2 kg-1 to cm2 g-1
      SticsRFiles::set_param_xml(plant_file, "slamax", slamax, overwrite = TRUE)
    }
    
    # minimum SLA (specific leaf area) of green leaves (cm2 g-1)
    if ("SLAMIN" %in% pft.names) {
      slamin <- pft.traits[which(pft.names == "SLAMIN")]
      slamin <- udunits2::ud.convert(udunits2::ud.convert(slamin, "m2", "cm2"), "kg-1", "g-1") # m2 kg-1 to cm2 g-1
      SticsRFiles::set_param_xml(plant_file, "slamin", slamin, overwrite = TRUE)
    }
    
    # yield formation
    
    # roots
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "roots")
    
    # longsperac - specific root length (cm g-1)
    if ("SRL" %in% pft.names) {
      srl_val  <- udunits2::ud.convert(pft.traits[which(pft.names == "SRL")], "m", "cm")
      SticsRFiles::set_param_xml(plant_file, "longsperac", srl_val, overwrite = TRUE)
    }
    
    # frost
    # water
    # nitrogen
    # correspondance code BBCH
    
    # cultivar parameters
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "cultivar parameters")
    
    # there are multiple cultivars (varietes) in plt file
    # for now I assume we will always use only #1 in simulations 
    # hence, _tec file will always say variete==1, if you change the logic don't forget to update handling of the _tec file accordingly
    
    # maximal lifespan of an adult leaf expressed in summation of Q10=2 (2**(T-Tbase))
    if ("leaf_lifespan_max" %in% pft.names) {
      # this will modifies all varietes' durvieFs by default
      SticsRFiles::set_param_xml(plant_file, "durvieF", pft.traits[which(pft.names == "leaf_lifespan_max")], overwrite = TRUE)
      # see example for setting a particular (the Grindstad) cultivar param
      # SticsRFiles::set_param_xml(plant_file, "durvieF", pft.traits[which(pft.names == "leaf_lifespan_max")], select = "Grindstad", overwrite = TRUE)    
    }
    
    # cumulative thermal time between the stages LEV (emergence) and AMF (maximum acceleration of leaf growth, end of juvenile phase) 
    if ("cum_thermal_juvenile" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "stlevamf", pft.traits[which(pft.names == "cum_thermal_juvenile")], overwrite = TRUE)
    }
    
    # cumulative thermal time between the stages AMF (maximum acceleration of leaf growth, end of juvenile phase)  and LAX (maximum leaf area index, end of leaf growth)
    if ("cum_thermal_growth" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "stamflax", pft.traits[which(pft.names == "cum_thermal_growth")], overwrite = TRUE)
    }
    
    # convert xml2txt
    if(names(trait.values)[pft] != "env"){
      SticsRFiles::convert_xml2txt(xml_file = plant_file, java_dir = javastics_path)
      # do I also need to move the file out of the plant folder to main rundir?
    }
    
  } # pft-loop ends
  
  
  ############################## Param gen / newform ####################################
  
  ## DO NOTHING FOR NOW
  gen_xml  <- XML::xmlParse(system.file("param_gen.xml", package = "PEcAn.STICS"))
  gen_file <- file.path(rundir, "param_gen.xml")
  XML::saveXML(gen_xml, file = gen_file)
  SticsRFiles::convert_xml2txt(xml_file = gen_file, java_dir = javastics_path)
  # may delete the xml after this
  
  newf_xml  <- XML::xmlParse(system.file("param_newform.xml", package = "PEcAn.STICS"))
  newf_file <- file.path(rundir, "param_newform.xml")
  XML::saveXML(newf_xml, file = newf_file)  
  SticsRFiles::convert_xml2txt(xml_file = newf_file, java_dir = javastics_path)
  
  
  
  ############################ Prepare Initialization File ##################################
  
  ## this is where we overwrite model initial conditions
  
  # read in template ini file
  ini_xml  <- XML::xmlParse(system.file("pecan_ini.xml", package = "PEcAn.STICS"))
  ini_file <- file.path(rundir, paste0(defaults$pft$name, "_ini.xml"))
  
  # write the ini file 
  XML::saveXML(ini_xml, file = ini_file)
  
  # DO NOTHING FOR NOW
  # but when you do note that this also has multiple options, e.g.
  # SticsRFiles::set_param_xml(xml_file = ini_file, param_name = "lai0", param_value = 1, select = "plante", value = "1", overwrite = TRUE)  
  
  SticsRFiles::convert_xml2txt(xml_file = ini_file, java_dir = javastics_path)
  
  
  ############################ Prepare Soils ##################################
  
  ## this is where we modify soil characteristics
  
  #### THERE IS SOME BUG IN SticsRFiles::convert_xml2txt FOR SOLS.XML
  #### I NOW PUT TXT VERSION TO THE MODEL PACKAGE: param.sol
  #### TODO: revise others to have txt templates directly in the package
  
  # read in template sols file (xml)
  # sols_xml  <- XML::xmlParse(system.file("sols.xml", package = "PEcAn.STICS"))
  sols_file <- file.path(rundir, "param.sol")
  
  # cp template sols file (txt)
  file.copy(system.file("param.sol", package = "PEcAn.STICS"), sols_file)
  
  # check param names
  # sols_vals  <- SticsRFiles::get_soil_txt(sols_file)
  
  SticsRFiles::set_soil_txt(filepath = sols_file, param="typsol", value=paste0("sol", defaults$pft$name))
  
  # DO NOTHING ELSE FOR NOW

  # this has some bug for sols.xml
  # SticsRFiles::convert_xml2txt(xml_file = sols_file, java_dir = javastics_path)
  
  ######################### Prepare Weather Station File ###############################
  
  ## this is where we modify more initial conditions and site characteristics
  
  # read in template sta file
  sta_xml  <- XML::xmlParse(system.file("pecan_sta.xml", package = "PEcAn.STICS"))
  sta_file <- file.path(rundir, settings$pfts$pft$name, paste0(tolower(sub(" .*", "", settings$run$site$name)), "_sta.xml"))
  
  XML::saveXML(sta_xml, file = sta_file)
  
  # change latitude
  SticsRFiles::set_param_xml(sta_file, "latitude", settings$run$site$lat, overwrite = TRUE)
  
  SticsRFiles::convert_xml2txt(xml_file = sta_file, java_dir = javastics_path)
  
  # another way to change latitute
  # sta_txt <- file.path(rundir, "station.txt")
  # SticsRFiles::set_station_txt(sta_txt, param = "latitude", value = settings$run$site$lat)
  
  # DO NOTHING ELSE FOR NOW
  # Should these be prepared by met2model.STICS?

  
  ############################## Prepare LAI forcing ####################################
  ## skipping for now
  
  

  
  ############################ Prepare Technical File ##################################
  
  ## this is where we modify management practices
  
  ## instead of using a template, this could be easier if we prepare a dataframe and use SticsRFiles::gen_tec_xml
  tec_df <- data.frame(Tec_name = paste0(defaults$pft$name, "_tec.xml")) # note more than one PFT cases
  

  # the following formalisms exist in the tec file:
  ## supply of organic residus
  ## soil tillage
  ## sowing
  ## phenological stages
  ## irrigation
  ## fertilisation
  ## harvest
  ## special techniques
  ## soil modification by techniques (compaction-fragmentation)
  
  # if a harvest file is given, most (all?) of our harvest cases are actually fall under special techniques - cut crop
  if(!is.null(settings$run$inputs$harvest)){
    
    h_days <- as.matrix(utils::read.table(settings$run$inputs$harvest$path, header = TRUE, sep = ","))
    
    #temporary dev hack, give 2-years of intervention until multiple usms
    h_days <- h_days[1:4,]
    
    # param names
    h_param_names <- c("julfauche"  , # date of each cut for forage crops, julian.d
                       "hautcoupe"  , # cut height for forage crops, m
                       "lairesiduel", # residual LAI after each cut of forage crop, m2 m-2
                       "msresiduel" , # residual aerial biomass after a cut of a forage crop, t.ha-1
                       "anitcoupe")   # amount of mineral N added by fertiliser application at each cut of a forage crop, kg.ha-1
                         
    
    harvest_list <- list()
    for(hrow in seq_len(nrow(h_days))){
      
      # empty
      harvest_df <- data.frame(julfauche = NA, hautcoupe = NA, lairesiduel = NA,  msresiduel = NA, anitcoupe = NA) 
      
      
      # If given harvest date is within simulation days
      # probably need to break down >2 years into multiple usms
      if(as.Date(h_days[hrow, 2], origin = paste0(h_days[hrow, 1], "-01-01")) %in% dseq){
        
        # STICS needs cutting days in cumulative julian days 
        # e.g. first cutting day of the first simulation year can be 163 (2018-06-13)
        # in following years it should be cumulative, meaning a cutting day on 2019-06-12 is 527, not 162
        # the following code should give that
        harvest_df$julfauche   <- which(dseq == as.Date(h_days[hrow, 2], origin = paste0(h_days[hrow, 1], "-01-01"))) + lubridate::yday(dseq[1]) - 2
        harvest_df$lairesiduel <- h_days[hrow, 3]
        # also pass cutting height
      }
      
      colnames(harvest_df) <- paste0(h_param_names, "_", hrow)
      harvest_list[[hrow]] <- harvest_df
    }
    harvest_tec <- do.call("cbind", harvest_list) 
    
    harvest_tec$codefauche <- 2 # use calendar days
  }
  # TODO: if no harvest file is given modify harvest parameters, e.g. harvest decision
  
  # DO NOTHING ELSE FOR NOW
  # TODO: ADD OTHER MANAGEMENT, E.G. FERTILIZATION
  
  # same usm -> continue columns
  tec_df <- cbind(tec_df, harvest_tec)
  
  # these shouldn't be empty even if we don't use them (values from timothy example in STICS)
  tec_df$iplt0 <- 999 # date of sowing
  tec_df$profsem <- 2 # depth of sowing
  tec_df$densitesem <- 100 # plant sowing density
  tec_df$variete <- 1 # cultivar number corresponding to the cultivar name in the plant file (could be passed via a field activity file)
  tec_df$irecbutoir <- 999 #latest date of harvest (imposed if the crop cycle is not finished at this date)
  tec_df$profmes <- 120 # depth of measurement of the soil water reserve (cm)
  tec_df$engrais <- 1 # fertilizer type
  tec_df$concirr <- 0.11 # concentration of mineral N in irrigation water (kg ha-1 mm-1)
  tec_df$ressuite <- 'straw+roots' # type of crop residue
  tec_df$h2ograinmax <- 0.32 # maximal water content of fruits at harvest
    
  SticsRFiles::gen_tec_xml(out_path = file.path(rundir, defaults$pft$name), param_table = tec_df)
  
  # TODO: more than 1 USM, rbind
  
  SticsRFiles::convert_xml2txt(xml_file = file.path(rundir, defaults$pft$name, paste0(defaults$pft$name, "_tec.xml")), 
                               java_dir = javastics_path)
  
  
  ################################ Prepare USM file ######################################

  # read in template USM (Unit of SiMulation) file, has the master settings, file names etc.
  usm_xml  <- XML::xmlParse(system.file("usms.xml", package = "PEcAn.STICS"))
  usm_file <- file.path(rundir, "usms.xml")
  XML::saveXML(usm_xml, file = usm_file)
  
  # This may also be easier to generate from a data frame instead of overwriting a template
  usms_param_df <- data.frame(usm_name = defaults$pft$name,  # pft name
                              datedebut = lubridate::yday(settings$run$start.date), # beginning day of the simulation (julian.d)
                              datefin = usm_list$usm$datedebut + length(dseq) - 1, # end day of the simulation (julian.d) (at the end of consecutive years, i.e. can be greater than 366)
                              finit = paste0(defaults$pft$name, "_ini.xml"), # name of the initialization file
                              nomsol = paste0("sol", defaults$pft$name), # name of the soil in the sols.xml file
                              fstation = paste0(tolower(sub(" .*", "", settings$run$site$name)), "_sta.xml"), # name of the weather station file
                             )
  

  
  # TODO: more than 1 USM and PFTs 
  # STICS can run 2 PFTs max: main crop + intercrop
  

  
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
  usm_list[[1]][[11]]$flai <- "default.lai" # hardcode for now, doesn't matter when codesimul==0
  
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
  # stics_path <- settings$model$binary
  
  # symlink to binary
  file.symlink(stics_path, bindir)
  
  # generate STICS input files using JavaStics
  jexe <- file.path(gsub("bin","", dirname(stics_path)), "JavaSticsCmd.exe")
  
  usm_name <- defaults$pft$name
  
  # if this script can already create the txts, bypass this step
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
