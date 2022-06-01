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
  cfgdir  <- file.path(settings$host$rundir, run.id, "config")
  bindir  <- file.path(settings$host$rundir, run.id, "bin")
  outdir  <- file.path(settings$host$outdir, run.id)
  
  # will think about multiple PFT case, currently considering successive (>2 years) case only
  usmdir_root  <- paste0(file.path(settings$host$rundir, run.id, settings$pfts$pft$name), "_") 
  # In STICS, it is 1 UMS per crop cycle, where the cycle can be 2-years max
  # If we have a consecutive monoculture for > 2 years, we still need to divide it into 2-year USMs
  years_requested <- unique(lubridate::year(dseq))
  if(length(years_requested) %%2 == 1) years_requested <- c(years_requested, years_requested[length(years_requested)])
  
  if(length(years_requested) > 2){
    years_indices <- rep(seq(1, length(years_requested), by=2), each=2)
    usmdirs <- tapply(years_requested, years_indices, function(x)  paste0(usmdir_root, paste(x, collapse = '-')))
  }else{
    usmdirs <- paste0(usmdir_root, paste(years_requested, collapse = '-'))
  } 
  
  ## make sure rundir and outdir exist
  dir.create(rundir, showWarnings = FALSE, recursive = TRUE)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  ## create usm, config and bin dirs
  dir.create(cfgdir,  showWarnings = FALSE, recursive = TRUE)
  dir.create(bindir,  showWarnings = FALSE, recursive = TRUE)
  sapply(usmdirs, dir.create, showWarnings = FALSE, recursive = TRUE)
  
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
    
    plant_file <- file.path(rundir, paste0(names(trait.values)[pft], "_plt.xml"))
    
    if(names(trait.values)[pft] != "env"){
      # save the template, will be overwritten below
      XML::saveXML(plt_xml, file = plant_file)
    }else{
      next
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
    
    # maximum phasic delay allowed due to stresses
    if ("phasic_delay_max" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "stressdev", pft.traits[which(pft.names == "phasic_delay_max")], overwrite = TRUE)
    }
    
    # minimum number of vernalising days (d) [0,7]
    if ("vernalization_days_min" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "jvcmini", round(pft.traits[which(pft.names == "vernalization_days_min")]), overwrite = TRUE)
    }
    
    # day of initiation of vernalisation in perennial crops (julian d) [1,731]
    if ("vernalization_init" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "julvernal", round(pft.traits[which(pft.names == "vernalization_init")]), overwrite = TRUE)
    }
    
    # optimal temperature for vernalisation (degreeC)
    if ("vernalization_TOpt" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tfroid", pft.traits[which(pft.names == "vernalization_TOpt")], overwrite = TRUE)
    }
    
    # skipping amfroid for now, seems to be relevant for winter barley only, come back for barley 
    # semi thermal amplitude for vernalising effect (degreeC)
    #if ("vernalization_TAmp" %in% pft.names) { # haven't entered this variable to DB, this is only a suggestion
    #  SticsRFiles::set_param_xml(plant_file, "ampfroid", pft.traits[which(pft.names == "vernalization_TAmp")], overwrite = TRUE)
    #}
    
    
    # emergence and starting
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "emergence and starting")
    # unlist(values)
    
    # minimum temperature below which emergence is stopped (degreeC)
    if ("emergence_Tmin" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "tgmin", pft.traits[which(pft.names == "emergence_Tmin")], overwrite = TRUE)
    }
    
    # nbfeuilplant, leaf number per plant when planting, default 0, skipping for now
    
    
    # this is a switch, for now hardcoding to be direct start (2)
    # should probably be passed via json
    # codegermin, option of simulation of a germination phase or a delay at the beginning of the crop (1) or direct starting (2)
    SticsRFiles::set_param_xml(plant_file, "codegermin", 2, overwrite = TRUE)
    # hence, skipping the other parameters related to this switch
    # stpltger: cumulative thermal time allowing germination
    # potgermi: soil water potential under which seed imbibition is impeded
    # nbjgerlim: maximum number of days after grain imbibition allowing full germination
    # propjgermin: minimal proportion of the duration nbjgerlim when the temperature is higher than the temperature threshold Tdmax
    
    
    # parameter of the curve of coleoptile elongation
    if ("belong" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "belong", pft.traits[which(pft.names == "belong")], overwrite = TRUE)
    }
    
    # parameter of the plantlet elongation curve
    if ("celong" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "celong", pft.traits[which(pft.names == "celong")], overwrite = TRUE)
    }
    
    # maximum elongation of the coleoptile in darkness condition
    if ("coleoptile_elong_dark_max" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "elmax", pft.traits[which(pft.names == "coleoptile_elong_dark_max")], overwrite = TRUE)
    }
    
    # number of days after germination after which plant emergence is reduced
    if ("days_reduced_emergence_postgerm" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "nlevlim1", round(pft.traits[which(pft.names == "days2reduced_emergence_postgerm")]), overwrite = TRUE)
    }
    
    # number of days after germination after which plant emergence is impossible
    if ("days2stopped_emergence_postgerm" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "nlevlim2", round(pft.traits[which(pft.names == "days2stopped_emergence_postgerm")]), overwrite = TRUE)
    }
    
    # plant vigor index allowing to emerge through a soil crust, vigueurbat == 1 inactivates some soil crust related parameters, skipping for now
    
    # there are also "planting" related parameters
    
    # leaves
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "leaves")
    # unlist(values)
    
    
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
    
    # minimum temperature at which growth ceases
    if ("tcmin_growth" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tcmin", pft.traits[which(pft.names == "tcmin_growth")], overwrite = TRUE)
    }
    
    # maximum temperature at which growth ceases
    if ("tcmax_growth" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tcmax", pft.traits[which(pft.names == "tcmax_growth")], overwrite = TRUE)
    }
    
    # temperature beyond which foliar growth stops
    if ("tcmax_foliar_growth" %in% pft.names) {
      #  tcxstop must be > tdmax, priors should be set that way, and we can let the simulation fail afterwards, but putting a warning here
      tdmax   <- SticsRFiles::get_param_xml(plant_file, param_name="tdmax", select = "formalisme", value = "phasic development")[[1]][[1]]
      tcxstop <- pft.traits[which(pft.names == "tcmax_foliar_growth")]
      if(tcxstop < tdmax){
        PEcAn.logger::logger.warn("tcmax_foliar_growth value (", tcxstop, ") should be greater than tdmax (", tdmax, ").")
      }
      SticsRFiles::set_param_xml(plant_file, "tcxstop", tcxstop, overwrite = TRUE)
      
    }
    
    # ulai at the inflexion point of the function DELTAI=f(ULAI)
    if ("vlaimax" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "vlaimax", pft.traits[which(pft.names == "vlaimax")], overwrite = TRUE)
    }
    
    # parameter of the logistic curve of LAI growth
    if ("pentlaimax" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "pentlaimax", pft.traits[which(pft.names == "pentlaimax")], overwrite = TRUE)
    }
    
    # ulai from which the rate of leaf growth decreases
    if ("udlaimax" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "udlaimax", pft.traits[which(pft.names == "udlaimax")], overwrite = TRUE)
    }
    
    # life span of early leaves expressed as a fraction of the life span of the last leaves emitted DURVIEF
    if ("early2last_leaflife" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "ratiodurvieI", pft.traits[which(pft.names == "early2last_leaflife")], overwrite = TRUE)
    }
    
    # fraction of senescent biomass (relative to total biomass)
    if ("senes2total_biomass" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "ratiosen", pft.traits[which(pft.names == "senes2total_biomass")], overwrite = TRUE)
    }
    
    # fraction of senescent leaves falling to the soil
    # not sure if this is supposed to be a fraction or a percentage in STICS, values look like a fraction but min-max is given as 0-100
    # treating it like a fraction for now
    if ("fracLeafFall" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "abscission", pft.traits[which(pft.names == "fracLeafFall")], overwrite = TRUE)
    }
    
    # parameter relating the C/N of dead leaves and the INN
    if ("parazofmorte" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "parazofmorte", pft.traits[which(pft.names == "parazofmorte")], overwrite = TRUE)
    }
    
    # parameter of the N stress function active on leaf expansion (INNLAI), bilinear function vs INN passing through the point (INNmin, INNturgmin)
    if ("innturgmin" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "innturgmin", pft.traits[which(pft.names == "innturgmin")], overwrite = TRUE)
    }
    
    # accelerating parameter for the lai growth rate
    if ("lai_growth_rate_accelerating" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "dlaimin", pft.traits[which(pft.names == "lai_growth_rate_accelerating")], overwrite = TRUE)
    }
    
    # maximum rate of the setting up of LAI
    if ("lai_max_rate" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "dlaimaxbrut", pft.traits[which(pft.names == "lai_max_rate")], overwrite = TRUE)
    } 
    
    # relative additional lifespan due to N excess in plant (INN > 1)
    if ("relative_addlifespan_DT_excessN" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "durviesupmax", pft.traits[which(pft.names == "relative_addlifespan_DT_excessN")], overwrite = TRUE)
    } 
    
    # parameter of the N stress function active on senescence (INNsenes), bilinear function vs INN passing through the point (INNmin, INNsen)
    if ("innsen" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "innsen", pft.traits[which(pft.names == "innsen")], overwrite = TRUE)
    } 
    
    
    # radiation interception
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "radiation interception")
    
    # extinction coefficient of photosynthetic active radiation in the canopy
    if ("extinction_coefficient_diffuse" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "extin", pft.traits[which(pft.names == "extinction_coefficient_diffuse")], overwrite = TRUE)
    } 
    
    # shoot biomass growth
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "shoot biomass growth")
    
    # optimal temperature (1/2) for plant growth
    if ("teopt" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "teopt", pft.traits[which(pft.names == "teopt")], overwrite = TRUE)
      # instead of putting own priors, trying this
      SticsRFiles::set_param_xml(plant_file, "temin", pft.traits[which(pft.names == "teopt")]-11, overwrite = TRUE)
      SticsRFiles::set_param_xml(plant_file, "temax", pft.traits[which(pft.names == "teopt")]+11, overwrite = TRUE)
    }
    
    # optimal temperature (2/2) for plant growth
    if ("teoptbis" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "teoptbis", pft.traits[which(pft.names == "teoptbis")], overwrite = TRUE)
    }
    
    # maximum radiation use efficiency during the juvenile phase
    if ("RUE_juv" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "efcroijuv", pft.traits[which(pft.names == "RUE_juv")], overwrite = TRUE)
    }
    
    # maximum radiation use efficiency during the vegetative stage
    if ("RUE_veg" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "efcroiveg", pft.traits[which(pft.names == "RUE_veg")], overwrite = TRUE)
    }
    
    # maximum radiation use efficiency during the grain filling phase
    if ("RUE_rep" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "efcroirepro", pft.traits[which(pft.names == "RUE_rep")], overwrite = TRUE)
    }
    
    # fraction of daily remobilisable C reserves
    if ("remobres" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "remobres", pft.traits[which(pft.names == "remobres")], overwrite = TRUE)
    }
    
    # ratio biomass / useful height cut of crops (t.ha-1.m-1)
    if ("biomass2usefulheight" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "coefmshaut", pft.traits[which(pft.names == "biomass2usefulheight")], overwrite = TRUE)
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
    
    
    # ratio stem (structural part)/leaf
    if ("stem2leaf" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tigefeuil", pft.traits[which(pft.names == "stem2leaf")], overwrite = TRUE)
    }
    
    # skipping: envfruit, fraction of envelop in grainmaxi (w:w)
    # skipping: sea, specific area of fruit envelops
    
    # yield formation, will get back
    
    # roots
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", value = "roots")
  
    
    # sensanox, index of anoxia sensitivity (0 = insensitive), 0 for now
    # stoprac, stage when root growth stops (LAX= maximum leaf area index, end of leaf growth or SEN=beginning of leaf senescence)
    
    # sensrsec, index of root sensitivity to drought (1=insensitive)
    if ("rootsens2drought" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "sensrsec", pft.traits[which(pft.names == "rootsens2drought")], overwrite = TRUE)
    }
    
    # contrdamax, maximal reduction in root growth rate due to soil strengthness (high bulk density)
    if ("db_reduc_rgr_max" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "contrdamax", pft.traits[which(pft.names == "db_reduc_rgr_max")], overwrite = TRUE)
    }    
    
    # draclong, maximum rate of root length production per plant (cm plant-1 degreeD-1)
    if ("rootlength_prod_max" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "draclong", pft.traits[which(pft.names == "rootlength_prod_max")], overwrite = TRUE)
    }   
    
    # debsenrac, sum of degrees-days defining the beginning of root senescence (root life time) (degreeD)
    if ("root_sen_dday" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "debsenrac", round(pft.traits[which(pft.names == "root_sen_dday")]), overwrite = TRUE)
    }  
    
    #lvfront, root density at the root apex (cm cm-3)
    if ("rootdens_at_apex" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "lvfront", pft.traits[which(pft.names == "rootdens_at_apex")], overwrite = TRUE)
    }  
    
    # longsperac - specific root length (cm g-1)
    if ("SRL" %in% pft.names) {
      srl_val  <- udunits2::ud.convert(pft.traits[which(pft.names == "SRL")], "m", "cm")
      SticsRFiles::set_param_xml(plant_file, "longsperac", srl_val, overwrite = TRUE)
    }
    
    # frost
    
    # formalism - water
    
    # psisto, potential of stomatal closing (absolute value) (bars)
    # note: units in betyDB are m, but my prior is for testing 
    if ("psi_stomata_closure" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "psisto", pft.traits[which(pft.names == "psi_stomata_closure")], overwrite = TRUE)
    }  
    
    # psiturg, potential of the beginning of decrease of the cellular extension (absolute value) (bars)
    # may or may not be leaf_psi_tlp in betyDB
    if ("leaf_psi_tlp" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "psiturg", pft.traits[which(pft.names == "leaf_psi_tlp")], overwrite = TRUE)
    }  
    
    # h2ofeuilverte, water content of green leaves (relative to fresh matter) (g g-1)
    # may or may not be water_content_TLP_leaf in betyDB
    if ("water_content_TLP_leaf" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "h2ofeuilverte", pft.traits[which(pft.names == "water_content_TLP_leaf")], overwrite = TRUE)
    }  
    
    # skipping:
    # h2ofeuiljaune
    # h2otigestruc
    # h2otigestruc
    # h2ofrvert
    # deshydbase
    # tempdeshyd
    
    # kmax, maximum crop coefficient for water requirements (=MET/PET)
    if ("crop_water_max" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "kmax", pft.traits[which(pft.names == "crop_water_max")], overwrite = TRUE)
    } 
    
    # nitrogen
    # masecNmax
    if ("masecNmax" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "masecNmax", pft.traits[which(pft.names == "masecNmax")], overwrite = TRUE)
    } 
    
    # Nreserve
    if ("Nreserve" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "Nreserve", pft.traits[which(pft.names == "Nreserve")], overwrite = TRUE)
    } 
    
    
    # Kmabs1
    if ("Kmabs1" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "Kmabs1", pft.traits[which(pft.names == "Kmabs1")], overwrite = TRUE)
    } 
    
    # adil
    if ("adil" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "adil", pft.traits[which(pft.names == "adil")], overwrite = TRUE)
    } 
    
    # bdil
    if ("bdil" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "bdil", pft.traits[which(pft.names == "bdil")], overwrite = TRUE)
    } 
    
    # INNmin
    if ("INNmin" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "INNmin", pft.traits[which(pft.names == "INNmin")], overwrite = TRUE)
    } 
    
    # Nmeta
    if ("Nmeta" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "Nmeta", pft.traits[which(pft.names == "Nmeta")], overwrite = TRUE)
    } 
    
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
    
    # temporary hack?
    if ("leaf_maintenance_respiration_mass" %in% pft.names & "leaf_respiration_Q10" %in% pft.names) {
      leaf_maintenance_respiration_mass <- pft.traits[which(pft.names == "leaf_maintenance_respiration_mass")]
      leaf_respiration_Q10 <- pft.traits[which(pft.names == "leaf_respiration_Q10")]
      save(leaf_maintenance_respiration_mass, leaf_respiration_Q10, file=file.path(outdir, "RespPars.Rdata"))
    }
    
    
    # convert xml2txt
    if(names(trait.values)[pft] != "env"){
      SticsRFiles::convert_xml2txt(xml_file = plant_file, java_dir = javastics_path)
      # do I also need to move the file out of the plant folder to main rundir?
    }
    
  } # pft-loop ends
  
  file.copy(file.path(rundir, "ficplt1.txt"), file.path(usmdirs, "ficplt1.txt"))
  
  ############################## Param gen / newform ####################################
  
  ## DO NOTHING FOR NOW
  gen_xml  <- XML::xmlParse(system.file("param_gen.xml", package = "PEcAn.STICS"))
  gen_file <- file.path(rundir, "param_gen.xml")
  XML::saveXML(gen_xml, file = gen_file)
  SticsRFiles::convert_xml2txt(xml_file = gen_file, java_dir = javastics_path)
  # may delete the xml after this
  file.copy(file.path(rundir, "tempopar.sti"), file.path(usmdirs, "tempopar.sti"))
  
  newf_xml  <- XML::xmlParse(system.file("param_newform.xml", package = "PEcAn.STICS"))
  newf_file <- file.path(rundir, "param_newform.xml")
  XML::saveXML(newf_xml, file = newf_file)  
  SticsRFiles::convert_xml2txt(xml_file = newf_file, java_dir = javastics_path)
  file.copy(file.path(rundir, "tempoparv6.sti"), file.path(usmdirs, "tempoparv6.sti"))
  
  
  
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
  file.copy(file.path(rundir, "ficini.txt"), file.path(usmdirs, "ficini.txt"))
  
  ############################ Prepare Soils ##################################
  
  ## this is where we modify soil characteristics
  
  #### THERE IS SOME BUG IN SticsRFiles::convert_xml2txt FOR SOLS.XML
  #### I NOW PUT TXT VERSION TO THE MODEL PACKAGE: param.sol
  #### TODO: revise others to have txt templates directly in the package
  
  sols_file <- file.path(rundir, "param.sol")
  
  # cp template sols file (txt)
  file.copy(system.file("param.sol", package = "PEcAn.STICS"), sols_file)
  
  # check param names
  # sols_vals  <- SticsRFiles::get_soil_txt(sols_file)
  
  SticsRFiles::set_soil_txt(filepath = sols_file, param="typsol", value=paste0("sol", defaults$pft$name))
  file.copy(sols_file, file.path(usmdirs, "param.sol"))
  
  # DO NOTHING ELSE FOR NOW

  # this has some bug for sols.xml
  # SticsRFiles::convert_xml2txt(xml_file = sols_file, java_dir = javastics_path)
  
  ######################### Prepare Weather Station File ###############################
  
  ## this is where we modify more initial conditions and site characteristics
  
  # read in template sta file
  sta_xml  <- XML::xmlParse(system.file("pecan_sta.xml", package = "PEcAn.STICS"))
  sta_file <- file.path(rundir, paste0(tolower(sub(" .*", "", settings$run$site$name)), "_sta.xml"))
  
  XML::saveXML(sta_xml, file = sta_file)
  
  # change latitude
  SticsRFiles::set_param_xml(sta_file, "latitude", settings$run$site$lat, overwrite = TRUE)
  
  SticsRFiles::convert_xml2txt(xml_file = sta_file, java_dir = javastics_path)
  file.copy(file.path(rundir, "station.txt"), file.path(usmdirs, "station.txt"))
  
  # another way to change latitute
  # sta_txt <- file.path(rundir, "station.txt")
  # SticsRFiles::set_station_txt(sta_txt, param = "latitude", value = settings$run$site$lat)
  
  # DO NOTHING ELSE FOR NOW
  # Should these be prepared by met2model.STICS?

  
  ############################## Prepare LAI forcing ####################################
  ## skipping for now
  
  

  
  ############################ Prepare Technical File ##################################
  
  ## this is where we modify management practices
  ## TODO: use ICASA compatible json file
  
  ## instead of using a template, this could be easier if we prepare a dataframe and use SticsRFiles::gen_tec_xml
  tec_df <- data.frame(Tec_name = paste0(defaults$pft$name, "_tec.xml")) # note more than one PFT cases
  
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
  
  # if a field activity file is given, most (all?) of our harvest cases are actually fall under special techniques - cut crop
  if(!is.null(settings$run$inputs$fielddata)){
    
    events_file <- jsonlite::read_json(settings$run$inputs$fielddata$path, simplifyVector = TRUE)[[1]]
    # loop for each USM
    for(usmi in seq_along(usmdirs)){
      
      usm_years <- years_requested[(usmi*2-1):(usmi*2)]
      dseq_sub <- dseq[lubridate::year(dseq) %in% usm_years]
      
      events_sub <- events_file$events[lubridate::year(events_file$events$date) %in% usm_years, ]
      
      if("planting" %in% events_sub$mgmt_operations_event){
        
        pl_date <- events_sub$date[events_sub$mgmt_operations_event == "planting"]
        tec_df$iplt0 <- lubridate::yday(as.Date(pl_date))
      }
      
      if("harvest" %in% events_sub$mgmt_operations_event){
        # param names
        h_param_names <- c("julfauche"  , # date of each cut for forage crops, julian.d
                           "hautcoupe"  , # cut height for forage crops, m
                           "lairesiduel", # residual LAI after each cut of forage crop, m2 m-2
                           "msresiduel" , # residual aerial biomass after a cut of a forage crop, t.ha-1
                           "anitcoupe")   # amount of mineral N added by fertiliser application at each cut of a forage crop, kg.ha-1

      
        harvest_sub <- events_sub[events_sub$mgmt_operations_event == "harvest",]
 
        harvest_list <- list()
        for(hrow in seq_len(nrow(harvest_sub))){
          
          # empty
          harvest_df <- data.frame(julfauche = NA, hautcoupe = NA, lairesiduel = NA,  msresiduel = NA, anitcoupe = NA) 
          
          
          # If given harvest date is within simulation days
          # probably need to break down >2 years into multiple usms
          if(as.Date(harvest_sub$date[hrow]) %in% dseq_sub){
            
            # STICS needs cutting days in cumulative julian days 
            # e.g. first cutting day of the first simulation year can be 163 (2018-06-13)
            # in following years it should be cumulative, meaning a cutting day on 2019-06-12 is 527, not 162
            # the following code should give that
            harvest_df$julfauche   <- which(dseq_sub == as.Date(harvest_sub$date[hrow])) + lubridate::yday(dseq_sub[1]) - 1
            harvest_df$hautcoupe <- as.numeric(harvest_sub$harvest_cut_height[harvest_sub$date==harvest_sub$date[hrow]]) # # cut height for forage crops
            harvest_df$hautcoupe <- ifelse(harvest_df$hautcoupe == -99, 0.05, harvest_df$hautcoupe)
            harvest_df$lairesiduel <- ifelse(harvest_df$hautcoupe < 0.08, 0.2, 0.8) # hardcode for now
            harvest_df$msresiduel <- ifelse(harvest_df$hautcoupe < 0.08, 0.05, 0.3) # residual aerial biomass after a cut of a forage crop (t ha-1)
            harvest_df$anitcoupe <- 21 # amount of mineral N added by fertiliser application at each cut of a forage crop (kg ha-1)
          }
          
          colnames(harvest_df) <- paste0(h_param_names, "_", hrow)
          harvest_list[[hrow]] <- harvest_df
        }
        harvest_tec <- do.call("cbind", harvest_list) 
        
        harvest_tec$codefauche <- 1  # cut crop - 1:yes, 2:no
        harvest_tec$mscoupemini <- 0 # min val of aerial biomass to make a cut
        harvest_tec$codemodfauche <- 2 # use calendar days
        harvest_tec$hautcoupedefaut <- 0.05 # cut height for forage crops (calendar calculated)
        harvest_tec$stadecoupedf <- "rec"
        
        
      } #harvest-if end
      
      if("organic_material" %in% events_sub$mgmt_operations_event |
         "fertilizer" %in% events_sub$mgmt_operations_event){
        # param names
        f_param_names <- c("julapN", # date of fertilization, julian.d
                           "absolute_value/%")   # cut height for forage crops, m
        
        
        fert_sub <- events_sub[events_sub$mgmt_operations_event %in% c("organic_material", "fertilizer"),]
        
        fert_list <- list()
        for(frow in seq_len(nrow(fert_sub))){
          
          # empty
          fert_df <- data.frame(jul = NA, val = NA) 

          # If given fertilization date is within simulation days
          if(as.Date(fert_sub$date[frow]) %in% dseq_sub){
            
            fert_df$jul   <- which(dseq_sub == as.Date(fert_sub$date[frow])) + lubridate::yday(dseq_sub[1]) - 1
            
            if(fert_sub$mgmt_operations_event[frow] == "organic_material"){
              Nprcnt <- ifelse(as.numeric(fert_sub$organic_material_N_conc[frow]) < 0, 5, as.numeric(fert_sub$organic_material_N_conc[frow]))
              fert_df$val <- as.numeric(fert_sub$org_material_applic_amnt[frow]) * (Nprcnt/100)
            }else{
              fert_df$val <- as.numeric(fert_sub$N_in_applied_fertilizer[frow])
            }
            
          }
          
          colnames(fert_df) <- paste0(f_param_names, "_", frow)
          fert_list[[frow]] <- fert_df
        }
        fert_tec <- do.call("cbind", fert_list) 
      } #fertilizer-if end
        
        
        # DO NOTHING ELSE FOR NOW
        # TODO: ADD OTHER MANAGEMENT
        
        # same usm -> continue columns
        usm_tec_df <- cbind(tec_df, harvest_tec, fert_tec)
        
        SticsRFiles::gen_tec_xml(out_path = usmdirs[usmi], param_table = usm_tec_df)
        
        # TODO: more than 1 USM, rbind
        
        SticsRFiles::convert_xml2txt(xml_file = file.path(usmdirs[usmi], paste0(defaults$pft$name, "_tec.xml")), 
                                     java_dir = javastics_path)
        
      
     } # end-loop over usms
    } # TODO: if no events file is given modify other harvest parameters, e.g. harvest decision
  
  ################################ Prepare USM file ######################################

  # loop for each USM
  #ncodesuite <- ifelse(length(usmdirs) > 1, 1,0)
  
  for(usmi in seq_along(usmdirs)){
    
    usm_years <- years_requested[(usmi*2-1):(usmi*2)]
    dseq_sub <- dseq[lubridate::year(dseq) %in% usm_years]
    
    # read in template USM (Unit of SiMulation) file, has the master settings, file names etc.
    usm_file <- file.path(usmdirs[usmi], "new_travail.usm")
    
    # cp template usm file 
    file.copy(system.file("template.usm", package = "PEcAn.STICS"), usm_file)
    
    # Type of LAI simulation 
    # 0 = culture (LAI calculated by the model), 1 = feuille (LAI forced)
    SticsRFiles::set_usm_txt(usm_file, "codesimul", 0, add = FALSE) # hardcode for now
    
    # use optimization
    # 0 = no;  1 = yes main plant; 2 = yes associated plant
    SticsRFiles::set_usm_txt(usm_file, "codeoptim", 0, add = FALSE) 
    
    # option to simulate several
    # successive USM (0 = no, 1 = yes)
    if(usmi == 1){
      SticsRFiles::set_usm_txt(usm_file, "codesuite", 0, add = FALSE)
    }else{
      SticsRFiles::set_usm_txt(usm_file, "codesuite", 1, add = FALSE)
    }
     

    # number of simulated plants (sole crop=1; intercropping=2)
    SticsRFiles::set_usm_txt(usm_file, "nbplantes", 1, add = FALSE) # hardcode for now
    
    # pft name
    SticsRFiles::set_usm_txt(usm_file, "nom", basename(usmdirs[usmi]), add = FALSE)
    
    
    ## handle dates, also for partial year(s)
    if(usmi == 1){
      # beginning day of the simulation (julian.d)
      # end day of the simulation (julian.d) (at the end of consecutive years, i.e. can be greater than 366)
      SticsRFiles::set_usm_txt(usm_file, "datedebut", lubridate::yday(settings$run$start.date), add = FALSE)
      SticsRFiles::set_usm_txt(usm_file, "datefin", (lubridate::yday(settings$run$start.date) + length(dseq_sub) - 1), add = FALSE)
    }else{
      SticsRFiles::set_usm_txt(usm_file, "datedebut", 1, add = FALSE)
      SticsRFiles::set_usm_txt(usm_file, "datefin", length(dseq_sub), add = FALSE)
    }
    
    # name of the initialization file
    SticsRFiles::set_usm_txt(usm_file, "finit", paste0(defaults$pft$name, "_ini.xml"), add = FALSE)
    
    # soil number
    SticsRFiles::set_usm_txt(usm_file, "numsol", 1, add = FALSE)
    
    # name of the soil in the sols.xml file
    SticsRFiles::set_usm_txt(usm_file, "nomsol", paste0("sol", defaults$pft$name), add = FALSE)
    
    # name of the weather station file
    SticsRFiles::set_usm_txt(usm_file, "fstation", paste0(tolower(sub(" .*", "", settings$run$site$name)), "_sta.xml"), add = FALSE)
    
    # name of the first climate file
    SticsRFiles::set_usm_txt(usm_file, "fclim1", paste0(tolower(sub(" .*", "", settings$run$site$name)), ".", usm_years[1]), add = FALSE)
    
    # name of the last climate file
    SticsRFiles::set_usm_txt(usm_file, "fclim2", paste0(tolower(sub(" .*", "", settings$run$site$name)), ".", usm_years[2]), add = FALSE)
    
    # number of simulation years
    SticsRFiles::set_usm_txt(usm_file, "nbans", 2, add = FALSE) # hardcode for now
    
    # number of calendar years involved in the crop cycle
    # 1 = 1 year e.g. for spring crops, 0 = two years, e.g. for winter crops
    SticsRFiles::set_usm_txt(usm_file, "culturean", 0, add = FALSE) #hardcoding this for now, if passed as a trait from priors it breaks sensitivity analysis
    # probably best to pass this via the json file
    
    # name of the plant file for main plant 
    SticsRFiles::set_usm_txt(usm_file, "fplt1", paste0(defaults$pft$name, "_plt.xml"), add = FALSE) 
    
    # name of the technical file for main plant
    SticsRFiles::set_usm_txt(usm_file, "ftec1", paste0(defaults$pft$name, "_tec.xml"), add = FALSE) 
    
    # name of the LAI forcing file for main plant (null if none)
    SticsRFiles::set_usm_txt(usm_file, "flai1", "default.lai", add = FALSE) # hardcode for now, doesn't matter when codesimul==0
    
    # TODO: more than 1 PFTs 
    # STICS can run 2 PFTs max: main crop + intercrop
  }

  

  
  ################################ Prepare Run ######################################
  
  # symlink climate files
  met_path <- settings$run$inputs$met$path
  
  for(usmi in seq_along(usmdirs)){
    
    usm_years <- unique(years_requested[(usmi*2-1):(usmi*2)])
    dseq_sub <- dseq[lubridate::year(dseq) %in% usm_years]
    
    clim_list <- list() # temporary solution
    for(clim in seq_along(usm_years)){
      # currently assuming only first year file has been passed to the settings, modify met2model if changing the logic
      met_file  <- gsub(paste0(lubridate::year(settings$run$start.date), ".climate"), paste0(usm_years[clim], ".climate"), met_path)
      clim_list[[clim]] <- read.table(met_file)
    }
    clim_run <- do.call("rbind", clim_list)
    write.table(clim_run, file.path(usmdirs[usmi], "climat.txt"), col.names = FALSE, row.names = FALSE)
    
  }
  
  # symlink to binary
  file.symlink(stics_path, bindir)
  stics_exe <- file.path(bindir, basename(stics_path))
  
  # symlink *.mod files
  file.symlink(system.file("var.mod",  package = "PEcAn.STICS"), file.path(usmdirs, "var.mod"))
  file.symlink(system.file("rap.mod",  package = "PEcAn.STICS"), file.path(usmdirs, "rap.mod"))
  file.symlink(system.file("prof.mod", package = "PEcAn.STICS"), file.path(usmdirs, "prof.mod"))
  
  #cmd_run <- paste("java -jar", jexe,"--run", rundir, usm_name)
  
  # using SticsOnR wrapper in job.sh now - SticsOnR::stics_wrapper(model_options = wrapper_options)
  # used to be:
  # cmd_generate <- paste("java -jar", jexe,"--generate-txt", rundir, usm_name)
  # cmd_run <- paste("java -jar", jexe,"--run", rundir, usm_name)
  

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
  
  if(length(usmdirs)>1){
    jobsh <- gsub("@SUCCESSIVE_USMS@", paste0("list(c('", paste(basename(usmdirs), collapse="','"), "'))"), jobsh)
  }else{
    jobsh <- gsub("@SUCCESSIVE_USMS@", 'NULL', jobsh)
  }
  
  jobsh <- gsub("@USMDIR@", usmdirs[1], jobsh) # for now
  
  jobsh <- gsub("@MODFILE@", paste0("mod_s", basename(usmdirs[1]), ".sti"), jobsh)
  jobsh <- gsub("@STICSEXE@", stics_exe, jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  

} # write.config.STICS
