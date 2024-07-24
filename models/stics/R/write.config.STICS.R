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

  ## the rest of the code assumes only plant PFTs
  ## little modification here as not to have a bigger re-write for now
  if(any(grepl("soil", names(trait.values)))){
    soil_params <- trait.values[[grep("soil", names(trait.values))]]
    settings$pfts[[grep("soil", names(trait.values))]] <- NULL
    trait.values[[grep("soil", names(trait.values))]] <- NULL
  }else{
    soil_params <- NULL
  }
  
  ## simulation days, used later
  dseq <- seq(lubridate::as_date(settings$run$start.date), lubridate::as_date(settings$run$end.date), by = "day")
  
  # find out where to write run/ouput
  rundir  <- file.path(settings$host$rundir, run.id)
  cfgdir  <- file.path(settings$host$rundir, run.id, "config")
  bindir  <- file.path(settings$host$rundir, run.id, "bin")
  outdir  <- file.path(settings$host$outdir, run.id)
  
  
  ########## Determining number of USMs (could be made its own function)
  
  # In STICS, it is 1 USM per crop cycle, where each cycle can be 2-years max
  # If we have a consecutive monoculture for > 2 years, we still need to divide it into 2-year USMs
  # If there are multiple pfts, this is a strong clue that there are multiple crop cycles
  # but it can also be the case that there is one cycle with intercropping
  
  years_requested <- unique(lubridate::year(dseq))
  # we always pass two climate files to STICS, repeat the same year twice if the last crop cycle has 1 year only
  if(length(years_requested) %%2 == 1) years_requested <- c(years_requested, years_requested[length(years_requested)])
  
  # Could the events file hierarchy be organized by crop cycle? Need to check how ACE-json does
  if(!is.null(settings$run$inputs$fielddata)){
    events_file <- jsonlite::read_json(settings$run$inputs$fielddata$path, simplifyVector = TRUE)[[1]]
    
    # testing new approach
    if(!is.null(events_file$rotation)){
      usmdirs <- rep(NA, nrow(events_file$rotation))
      for(uic in seq_along(usmdirs)){
        p1 <- tolower(events_file$rotation$planted_crop1[uic])
        p2 <- ifelse(events_file$rotation$planted_crop2[uic] != "-99.0", tolower(events_file$rotation$planted_crop2[uic]), "")
        uname <- paste0(p1,p2)
        usmdirs[uic] <- paste0(file.path(settings$host$rundir, run.id, uname), "_",
                          lubridate::year(events_file$rotation$rotation_begin[uic]), "-",
                          lubridate::year(events_file$rotation$rotation_end[uic])) 
      }
    }else{
      
      # events file can have info from other years, subset
      sub_events <- events_file$events[(lubridate::year(events_file$events$date) %in% years_requested),]
      
      
      crops <- c(sub_events$planted_crop, sub_events$harvest_crop)
      if(!is.null(crops)){
        crops <- crops[!is.na(crops)] # filter NAs caused by flattening the json
        # for now taking a simplistic assumption that if there are more than 1 harvested + planted crops, there are multiple crop cycles
        if(length(unique(crops)) > 1){
          # we probably have multiple pfts passed via settings, usmdir_root will be an array
          usmdir_root  <- paste0(file.path(settings$host$rundir, run.id, sapply(settings$pfts, `[[`, "name")), "_") 
          # !!! IMPORTANT: document also elsewhere
          # I'm making STICS PFT names to match fieldactivity names, or more broadly whatever is in the events json file!!!
          # e.g. barley is not barley but bar
          # alternatively I can start a LUT to match bety-pft names to match events species codes
          # we need to pass right parameters under right USM!
          
          if(length(years_requested) <= 2){
            # multiple usms due to crop rotation only
            # associate spp and year
            usmdirs <- sapply(crops, function(x){
              crop_yr <- lubridate::year(sub_events$date[(sub_events$planted_crop %in% x) | (sub_events$harvest_crop %in% x)])
              crop_usm <- paste0(usmdir_root[grep(tolower(x), usmdir_root)], crop_yr)
              return(crop_usm)
            })
            
            # make sure the usmdir order is the same as the rotation order 
            # this may need to get more sophisticated in the future 
            # but keeping the usmdirs in chronological order will come handy in the rest of this function
            usmdirs <- usmdirs[order(sapply(strsplit(sub(".*_", "", basename(usmdirs)), "-"), function(x) min(as.numeric(x))))]
            
          }else{
            # multiple usms due to crop rotation and multiple cropping seasons per rotation
            # not implemented yet
            PEcAn.logger::logger.severe("write.config.STICS is under development for this case.")
          }
          
        }else{
          # single crop, single usmdir_root
          usmdir_root  <- paste0(file.path(settings$host$rundir, run.id, settings$pfts$pft$name), "_") 
          if(length(years_requested) > 2){
            # multiple usms because more than 2 years of simulation
            years_indices <- rep(seq(1, length(years_requested), by=2), each=2)
            usmdirs <- tapply(years_requested, years_indices, function(x)  paste0(usmdir_root, paste(x, collapse = '-')))
          }else{
            # single usm because less than 2 years of simulation
            usmdirs <- paste0(usmdir_root, paste(years_requested, collapse = '-'))
          } 
        }
        
      }else{
        # somehow events have no crop identifiers, e.g. only fertilization and tilling events are passed 
        # most likely a partial year & crop cycle
        usmdir_root  <- paste0(file.path(settings$host$rundir, run.id, settings$pfts$pft$name), "_") 
        # single usm
        usmdirs <- paste0(usmdir_root, paste(years_requested, collapse = '-'))
      }
      
    }

  }
  
  # TODO: have a better way to determine USMs
  
  ########################## finish usmdirs

  
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

  
  # Per STICS development team, there are two types of STICS inputs
  # Global input: _plt.xml, param_gen.xml, param_newform.xml
  # Local input: _ini.xml (initialization), sols.xml (soils), _tec.xml (crop management), (climate files) _sta.xml, *.year
  
  # NOTE: however, it's the text files, not the xml files that are read by the STICS executable.
  
  ################################# Prepare Plant File #######################################
  
  ## this is where we overwrite model parameters
  
  # read in template plt file, has all the formalisms
  plt_xml  <- XML::xmlParse(system.file("crop_plt.xml", package = "PEcAn.STICS"))
  #plt_list <- XML::xmlToList(plt_xml)
  plt_files <- list()
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
    
    plt_files[[pft]] <- plant_file
    
    # to learn the parameters in a plant file
    # SticsRFiles::get_param_info(file_path = plant_file)
    
    # go over each formalism and replace params following the order in crop_plt
    # TODO: vary more params
    
    # plant name and group
    # effect of atmospheric CO2 concentration
    
    # phasic development
    # to see parameters per formalism
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "phasic development")
    # unlist(values)
    
    # name code of plant in 3 letters
    # a handful of plants have to have specific codes, e.g. forages need to be 'fou' and vine needs to be 'vig'
    # but others can be anything? if not, either consider a LUT or passing via settings
    if(names(trait.values)[pft] %in% c("frg", "wcl", "alf")){
      codeplante <- 'fou'
      codeperenne <- 2
    }else{
      codeplante <- base::substr(names(trait.values)[pft],1,3)
      codeperenne <- 1
    }
    codebfroid <- 2 # vernalization requirement, hardcoding for now, 2==yes
    SticsRFiles::set_param_xml(plant_file, "codeplante", codeplante, overwrite = TRUE)
    SticsRFiles::set_param_xml(plant_file, "codeperenne", codeperenne, overwrite = TRUE)
    SticsRFiles::set_param_xml(plant_file, "codebfroid", codebfroid, overwrite = TRUE)
    
    # minimum temperature below which development stops (degree C)
    if ("tdmin" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tdmin", pft.traits[which(pft.names == "tdmin")], overwrite = TRUE)
    }
    
    # maximum temperature above which development stops (degree C)
    if ("tdmax" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tdmax", pft.traits[which(pft.names == "tdmax")], overwrite = TRUE)
    }
    
    # basal photoperiod
    if ("phobase" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "phobase", pft.traits[which(pft.names == "phobase")], overwrite = TRUE)
    }
    
    # saturating photoperiod
    if ("phosat" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "phosat", pft.traits[which(pft.names == "phosat")], overwrite = TRUE)
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
    # this only takes effect for perennial crops
    if ("vernalization_init" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "julvernal", round(pft.traits[which(pft.names == "vernalization_init")]), overwrite = TRUE)
    }
    
    # optimal temperature for vernalisation (degreeC)
    if ("vernalization_TOpt" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "tfroid", pft.traits[which(pft.names == "vernalization_TOpt")], overwrite = TRUE)
    }
    
    # semi thermal amplitude for vernalising effect (degreeC)
    if ("vernalization_TAmp" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "ampfroid", pft.traits[which(pft.names == "vernalization_TAmp")], overwrite = TRUE)
    }
    
    if ("coeflevamf" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "coeflevamf", pft.traits[which(pft.names == "coeflevamf")], overwrite = TRUE)
    }
    
    if ("coefamflax" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "coefamflax", pft.traits[which(pft.names == "coefamflax")], overwrite = TRUE)
    }
    
    if ("coeflaxsen" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "coeflaxsen", pft.traits[which(pft.names == "coeflaxsen")], overwrite = TRUE)
    }
    
    if ("coefsenlan" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "coefsenlan", pft.traits[which(pft.names == "coefsenlan")], overwrite = TRUE)
    }
    
    if ("coeflevdrp" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "coeflevdrp", pft.traits[which(pft.names == "coeflevdrp")], overwrite = TRUE)
    }
    
    if ("coefdrpmat" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "coefdrpmat", pft.traits[which(pft.names == "coefdrpmat")], overwrite = TRUE)
    }
    
    if ("coefflodrp" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "coefflodrp", pft.traits[which(pft.names == "coefflodrp")], overwrite = TRUE)
    }
    
    
    # emergence and starting
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "emergence and starting")
    # unlist(values)
    
    # minimum temperature below which emergence is stopped (degreeC)
    if ("emergence_Tmin" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "tgmin", pft.traits[which(pft.names == "emergence_Tmin")], overwrite = TRUE)
    }
    
    # nbfeuilplant, leaf number per plant when planting, default 0, skipping for now
    
    
    # this is a switch, for now hardcoding to have delay at the beginning of the crop (1)
    # if starting the simulation from a later stage (e.g. lev) this has no effect
    # codegermin, option of simulation of a germination phase or a delay at the beginning of the crop (1) or direct starting (2)
    SticsRFiles::set_param_xml(plant_file, "codegermin", 1, overwrite = TRUE)
    
    # cumulative thermal time allowing germination (degree-d)
    if ("cum_thermal_germin" %in% pft.names) { 
      SticsRFiles::set_param_xml(plant_file, "stpltger", pft.traits[which(pft.names == "cum_thermal_germin")], overwrite = TRUE)
    }
    
    # skipping the other parameters related to this switch, they don't seem influential, at least on NPP and LAI
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
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "leaves")
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
      tdmax   <- SticsRFiles::get_param_xml(plant_file, param="tdmax", select = "formalisme", select_value = "phasic development")[[1]][[1]]
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
    
    # threshold soil water content active to simulate water senescence stress as a proportion of the turgor stress
    if ("rapsenturg" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "rapsenturg", pft.traits[which(pft.names == "rapsenturg")], overwrite = TRUE)
    } 
    
    
    # radiation interception
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "radiation interception")
    
    # extinction coefficient of photosynthetic active radiation in the canopy
    if ("extinction_coefficient_diffuse" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "extin", pft.traits[which(pft.names == "extinction_coefficient_diffuse")], overwrite = TRUE)
    } 
    
    # shoot biomass growth
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "shoot biomass growth")
    
    # minimum temperature for development
    if ("temin" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "temin", pft.traits[which(pft.names == "temin")], overwrite = TRUE)
    }
    
    # maximal temperature above which plant growth stops
    if ("temax" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "temax", pft.traits[which(pft.names == "temax")], overwrite = TRUE)
    }
    
    # optimal temperature (1/2) for plant growth
    if ("teopt" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "teopt", pft.traits[which(pft.names == "teopt")], overwrite = TRUE)
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
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "partitioning of biomass in organs")
    
    # maximum SLA (specific leaf area) of green leaves (cm2 g-1)
    if ("SLAMAX" %in% pft.names) {
      slamax <- pft.traits[which(pft.names == "SLAMAX")]
      slamax <- PEcAn.utils::ud_convert(PEcAn.utils::ud_convert(slamax, "m2", "cm2"), "kg-1", "g-1") # m2 kg-1 to cm2 g-1
      SticsRFiles::set_param_xml(plant_file, "slamax", slamax, overwrite = TRUE)
    }
    
    # minimum SLA (specific leaf area) of green leaves (cm2 g-1)
    if ("SLAMIN" %in% pft.names) {
      slamin <- pft.traits[which(pft.names == "SLAMIN")]
      slamin <- PEcAn.utils::ud_convert(PEcAn.utils::ud_convert(slamin, "m2", "cm2"), "kg-1", "g-1") # m2 kg-1 to cm2 g-1
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
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "roots")
  
    
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
      srl_val  <- PEcAn.utils::ud_convert(pft.traits[which(pft.names == "SRL")], "m", "cm")
      SticsRFiles::set_param_xml(plant_file, "longsperac", srl_val, overwrite = TRUE)
    }
    
    # option to activate the N influence on root partitioning within the soil profile (1 = yes, 2 = no)
    SticsRFiles::set_param_xml(plant_file, "codazorac", 1, overwrite = TRUE)
    
    # reduction factor on root growth when soil mineral N is limiting (< minazorac)
    if ("minefnra" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "minefnra", pft.traits[which(pft.names == "minefnra")], overwrite = TRUE)
    }  
    
    # mineral N concentration in soil below which root growth is reduced (kg.ha-1.cm-1)
    if ("minazorac" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "minazorac", pft.traits[which(pft.names == "minazorac")], overwrite = TRUE)
    }  
    
    # mineral N concentration in soil above which root growth is maximum (kg.ha-1.cm-1)
    if ("maxazorac" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "maxazorac", pft.traits[which(pft.names == "maxazorac")], overwrite = TRUE)
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
      SticsRFiles::set_param_xml(plant_file, "Nmeta", pft.traits[which(pft.names == "Nmeta")]*100, overwrite = TRUE)
    } 
    
    # correspondance code BBCH
    
    # cultivar parameters
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "cultivar parameters")
    
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
    
    # cumulative thermal time between the stages LEV (emergence) and DRP (starting date of filling of harvested organs)
    if ("cum_thermal_filling" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "stlevdrp", pft.traits[which(pft.names == "cum_thermal_filling")], overwrite = TRUE)
    }
    
    if ("adens" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "adens", pft.traits[which(pft.names == "adens")], overwrite = TRUE)
    }
    
    if ("croirac" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "croirac", pft.traits[which(pft.names == "croirac")], overwrite = TRUE)
    }
    
    # extinction coefficient connecting LAI to crop height
    if ("LAI2height" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "khaut", pft.traits[which(pft.names == "LAI2height")], overwrite = TRUE)
    }
    
    # average root radius
    if ("rayon" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "rayon", pft.traits[which(pft.names == "rayon")], overwrite = TRUE)
    }
    
    # minimal value for drought stress index
    if ("swfacmin" %in% pft.names) {
      SticsRFiles::set_param_xml(plant_file, "swfacmin", pft.traits[which(pft.names == "swfacmin")], overwrite = TRUE)
    }
    
    # convert xml2txt
    if(names(trait.values)[pft] != "env"){
      SticsRFiles::convert_xml2txt(file = plant_file)
      # do I also need to move the file out of the plant folder to main rundir?
    }
    
    this_usm <- grep(names(trait.values)[pft], usmdirs)
    sapply(this_usm, function(x){
      file.copy(file.path(rundir, "ficplt1.txt"), file.path(usmdirs[x], "ficplt1.txt"), overwrite = TRUE)
    })
    
  } # pft-loop ends
  
  
  
  ############################## Param gen / newform ####################################
  
  ## these also have plant parameters as well as soil 
  ## at the moment everything is treated as params, but some could be IC or come from the events file
  
  # these parameters won't change as crop changes in a continous rotation
  soil.names <- names(soil_params)
  
  for (pft in seq_along(trait.values)) {
    
    if(names(trait.values)[pft] == "env"){
      next
    }
    
    gen_xml  <- XML::xmlParse(system.file("param_gen.xml", package = "PEcAn.STICS"))
    gen_file <- file.path(rundir, "param_gen.xml")
    XML::saveXML(gen_xml, file = gen_file)
    codeinitprec <- ifelse(length(usmdirs>1), 1, 2) 
    SticsRFiles::set_param_xml(gen_file, "codeinitprec", codeinitprec, overwrite = TRUE)
    
    newf_xml  <- XML::xmlParse(system.file("param_newform.xml", package = "PEcAn.STICS"))
    newf_file <- file.path(rundir, "param_newform.xml")
    XML::saveXML(newf_xml, file = newf_file)  

    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names  <- names(pft.traits)
    
    ### Shoot growth
    # parameter defining radiation effect on conversion efficiency
    if ("rad_on_conversion_eff" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "coefb", pft.traits[which(pft.names == "rad_on_conversion_eff")], overwrite = TRUE)
    }
    
    # ratio of root mass to aerial mass at harvest
    if ("root2aerial_harvest" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "proprac", pft.traits[which(pft.names == "root2aerial_harvest")], overwrite = TRUE)
    }

    # minimal amount of root mass at harvest (when aerial biomass is nil) t.ha-1
    if ("rootmin_harvest" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "y0msrac", pft.traits[which(pft.names == "rootmin_harvest")], overwrite = TRUE)
    }
    
    ### Root growth
    
    # bulk density of soil below which root growth is reduced due to a lack of soil cohesion (g.cm-3)
    if ("bd_rootgrowth_reduced" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "dacohes", pft.traits[which(pft.names == "bd_rootgrowth_reduced")], overwrite = TRUE)
    }
    
    # bulk density of soil above which root growth is maximal (g.cm-3)
    if ("bd_rootgrowth_maximal" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "daseuilbas", pft.traits[which(pft.names == "bd_rootgrowth_maximal")], overwrite = TRUE)
    }
    
    # bulk density of soil above which root growth becomes impossible (g.cm-3)
    if ("bd_rootgrowth_impossible" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "daseuilhaut", pft.traits[which(pft.names == "bd_rootgrowth_impossible")], overwrite = TRUE)
    }
    
    ### Water absorption and nitrogen content of the plant
    
    # parameter of increase of maximal transpiration when a water stress occurs
    if ("maxTPincrease_waterstress" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "beta", pft.traits[which(pft.names == "maxTPincrease_waterstress")], overwrite = TRUE)
    }
    
    # root length density (RLD) above which water and N uptake are maximum and independent of RLD
    if ("lvopt" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "lvopt", pft.traits[which(pft.names == "lvopt")], overwrite = TRUE)
    }

    # diffusion coefficient of nitrate N in soil at field capacity
    if ("difN_FC" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "difN", soil_params[which(soil.names == "difN_FC")], overwrite = TRUE)
    }
    
    # skipping
    # concrr: inorganic N concentration (NH4+NO3-N) in the rain
    
    # minimal amount of rain required to start an automatic fertilisation (N mm.d-1)
    if ("plNmin" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "plNmin", soil_params[which(soil.names == "plNmin")], overwrite = TRUE)
    }

    # skipping, irrlev:
    # amount of irrigation applied automatically on the sowing day to allow germination when the model calculates automaticaly 
    # the amount of irrigations or when the irrigation dates are calculated by sum of temperature
    
    # minimal amount of N in the plant required to compute INN (kg.ha-1)
    if ("QNpltminINN" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "QNpltminINN", pft.traits[which(pft.names == "QNpltminINN")], overwrite = TRUE)
    }
    
    ### Soil C and N processes and fertiliser losses
    
    # minimal temperature for decomposition of humified organic matter (degreeC)
    if ("tmin_mineralisation" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "tmin_mineralisation", soil_params[which(soil.names == "tmin_mineralisation")], overwrite = TRUE)
    }
    
    # parameter (1/2) of the temperature function on humus decomposition rate
    if ("T_p1_Hdecomp_rate" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "FTEMh", soil_params[which(soil.names == "T_p1_Hdecomp_rate")], overwrite = TRUE)
    }
    
    # parameter (2/2) of the temperature function on humus decomposition rate
    if ("T_p2_Hdecomp_rate" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "FTEMha", soil_params[which(soil.names == "T_p2_Hdecomp_rate")], overwrite = TRUE)
    }
    
    # reference temperature for decomposition of humified organic matter
    if ("T_r_HOMdecomp" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "TREFh", soil_params[which(soil.names == "T_r_HOMdecomp")], overwrite = TRUE)
    }
    
    # parameter (1/2) of the temperature function on decomposition rate of organic residues
    if ("FTEMr" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "FTEMr", soil_params[which(soil.names == "FTEMr")], overwrite = TRUE)
    }
    
    # parameter (2/2) of the temperature function on decomposition rate of organic residues
    if ("FTEMra" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "FTEMra", soil_params[which(soil.names == "FTEMra")], overwrite = TRUE)
    }
    
    # reference temperature for decomposition of organic residues
    if ("T_r_ORdecomp" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "TREFr", soil_params[which(soil.names == "T_r_ORdecomp")], overwrite = TRUE)
    }
    
    # TODO: come back to these
    # # not used anymore, or at least not with this name!!!
    # # relative potential mineralization rate: K2 = fmin1 * exp(- fmin2*argi) / (1+fmin3*calc)
    # if ("FMIN1" %in% soil.names) {
    #   SticsRFiles::set_param_xml(gen_file, "FMIN1", soil_params[which(soil.names == "FMIN1")], overwrite = TRUE)
    # }
    # 
    # # not used anymore, or at least not with this name!!!
    # # parameter defining the effect of clay on the potential mineralization rate: K2 = fmin1 * exp(-fmin2*argi) / (1+fmin3*calc)
    # if ("FMIN2" %in% soil.names) {
    #   SticsRFiles::set_param_xml(gen_file, "FMIN2", soil_params[which(soil.names == "FMIN2")], overwrite = TRUE)
    # }
    # 
    # # not used anymore, or at least not with this name!!!
    # # parameter defining the effect of CaCO3 on the potential mineralization rate: K2 = fmin1 * exp(-fmin2*argi) / (1+fmin3*calc)
    # if ("FMIN3" %in% soil.names) {
    #   SticsRFiles::set_param_xml(gen_file, "FMIN3", soil_params[which(soil.names == "FMIN3")], overwrite = TRUE)
    # }
    
    # N/C ratio of soil humus
    if ("Wh" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "Wh", soil_params[which(soil.names == "Wh")], overwrite = TRUE)
    }
    
    # soil pH below which NH3 volatilisation derived from fertiliser is nil
    if ("pHminvol" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "pHminvol", soil_params[which(soil.names == "pHminvol")], overwrite = TRUE)
    }
    
    # soil pH above which NH3 volatilisation derived from fertiliser is maximum
    if ("pHmaxvol" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "pHmaxvol", soil_params[which(soil.names == "pHmaxvol")], overwrite = TRUE)
    }

    # N uptake rate at which fertilizer loss is divided by 2
    if ("Nupt_fertloss_halve" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "Vabs2", soil_params[which(soil.names == "Nupt_fertloss_halve")], overwrite = TRUE)
    }

    # maximal amount of N immobilised in soil derived from the mineral fertilizer
    if ("maxNimm_mineralfert" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "Xorgmax", soil_params[which(soil.names == "maxNimm_mineralfert")], overwrite = TRUE)
    }
    
    # relative water content (fraction of field capacity) below which mineralisation rate is nil
    if ("hminm" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "hminm", soil_params[which(soil.names == "hminm")], overwrite = TRUE)
    }

    # relative water content (fraction of field capacity) below which mineralisation rate is maximum
    if ("hoptm" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "hoptm", soil_params[which(soil.names == "hoptm")], overwrite = TRUE)
    }

    # skipping, alphaph:
    # maximal soil pH variation per unit of inorganic N added with slurry
      
    # skipping, dphvolmax:
    # maximal pH increase following the application of slurry
    
    # skipping, phvols:
    # parameter used to calculate the variation of soil pH after the addition of slurry
    
    # relative soil mineralisation rate at water saturation
    if ("fhminsat" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "fhminsat", soil_params[which(soil.names == "fhminsat")], overwrite = TRUE)
    }
      
    # reduction factor of decomposition rate of organic residues when mineral N is limiting
    if ("Nlim_reductionOMdecomp" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "fredkN", soil_params[which(soil.names == "Nlim_reductionOMdecomp")], overwrite = TRUE)
    }
    
    # reduction factor of decomposition rate of microbial biomass when mineral N is limiting
    if ("Nlim_reductionMBdecomp" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "fredlN", soil_params[which(soil.names == "Nlim_reductionMBdecomp")], overwrite = TRUE)
    }

    # minimal value for the ratio N/C of the microbial biomass when N limits decomposition
    if ("fNCbiomin" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "fNCbiomin", soil_params[which(soil.names == "fNCbiomin")], overwrite = TRUE)
    }
    
    # additional reduction factor of residues decomposition rate when mineral N is very limited in soil
    if ("fredNsup" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "fredNsup", soil_params[which(soil.names == "fredNsup")], overwrite = TRUE)
    }
    
    # maximum priming ratio (relative to SOM decomposition SD rate)
    if ("Primingmax" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "Primingmax", soil_params[which(soil.names == "Primingmax")], overwrite = TRUE)
    }
    
    ### Nitrification, denitrification and associated N2O emissions
    ### TODO: modify these params
    
    ### Soil hydrology and compaction
    
    # minimal amount of rain required to produce runoff (mm.d-1)
    if ("precmin4runoff" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "pminruis", soil_params[which(soil.names == "precmin4runoff")], overwrite = TRUE)
    }
    
    # soil thermal diffusivity (cm2.s-1)
    if ("soil_thermal_diffusivity" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "diftherm", soil_params[which(soil.names == "soil_thermal_diffusivity")], overwrite = TRUE)
    }
    
    # skipping, bformnappe:
    # coefficient for the water table shape (artificially drained soil)

    # drain radius (cm)
    if ("rdrain" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "rdrain", soil_params[which(soil.names == "rdrain")], overwrite = TRUE)
    }
    
    # soil water potential corresponding to wilting point (Mpa)
    if ("SWP_WP" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "psihumin", soil_params[which(soil.names == "SWP_WP")], overwrite = TRUE)
    }
    
    # soil water potential corresponding to field capacity (Mpa)
    if ("SWP_FC" %in% soil.names) {
      SticsRFiles::set_param_xml(gen_file, "psihucc", soil_params[which(soil.names == "SWP_FC")], overwrite = TRUE)
    }
    
    # soil moisture content (fraction of field capacity) above which compaction may occur and delay sowing
    if ("SMC_compaction_delay_sow" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "prophumtasssem", pft.traits[which(pft.names == "SMC_compaction_delay_sow")], overwrite = TRUE)
    }
    
    # soil moisture content (fraction of field capacity) above which compaction may occur and delay harvest
    if ("SMC_compaction_delay_harvest" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "prophumtassrec", pft.traits[which(pft.names == "SMC_compaction_delay_harvest")], overwrite = TRUE)
    }

    ### skipping
    ### Soil tillage if soil compaction activated
    
    ### Typology of pebbles fertilisers and residues
    ### should some of these parameters come from event files?
    
    ### codetypeng: Types of mineral fertilisers - 1 atm
    # 1: Ammonium.nitrate
    # 2: Urea.Ammonium.Nitrate.solution
    # 3: Urea
    # 4: Anhydrous.ammonia
    # 5: Ammonium.sulphate
    # 6: Ammonium.phosphate
    # 7: Calcium.nitrate
    # 8: Fixed.efficiency
    
    # each option has 4 params
    # engamm: fraction of ammonium in the N fertilizer
    # orgeng: maximal amount of fertilizer N that can be immobilized in the soil (fraction for type 8)
    # deneng: maximal fraction of the mineral fertilizer that can be denitrified (used if codedenit is not activated)
    # voleng: maximal fraction of mineral fertilizer that can be volatilized
      
    ### codetypres: Type of residues for decomposition parameters - 21 atm
    # 1:  Main crop on surface
    # 2:  Intermediate crop on surface
    # 3:  Manure on surface
    # 4:  Green compost on surface
    # 5:  Sewage sludge on surface
    # 6:  Vinasse on surface
    # 7:  Horn on surface
    # 8:  Grapevine shoots on surface
    # 9:  Others.1 on surface
    # 10: Others.2 on surface
    # 11: Main crop ploughed in
    # 12: Intermediate crop ploughed in
    # 13: Manure ploughed in
    # 14: Green compost ploughed in
    # 15: Sewage sludge ploughed in
    # 16: Vinasse ploughed in
    # 17: Cattle horn ploughed in
    # 18: Grapevine shoots ploughed in
    # 19: Others.1 ploughed in
    # 20: Others.2 ploughed in
    # 21: Dead roots in soil
    
    # each option has 17 params
    
    # fraction of organic residue which is decomposable
    if ("fOR_decomp" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "CroCo", pft.traits[which(pft.names == "fOR_decomp")], overwrite = TRUE)
    }

    # parameter of organic residues decomposition: kres=akres+bkres/CsurNres
    if ("ORdecomp_par" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "akres", pft.traits[which(pft.names == "ORdecomp_par")], overwrite = TRUE)
    }
      
    # potential rate of decomposition of organic residues: kres=akres+bkres/CsurNres
    if ("ORdecomp_rate" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "bkres", pft.traits[which(pft.names == "ORdecomp_rate")], overwrite = TRUE)
    }
    
    # parameter determining C/N ratio of biomass during organic residues decomposition: CsurNbio=awb+bwb/CsurNres
    if ("awb" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "awb", pft.traits[which(pft.names == "awb")], overwrite = TRUE)
    }
    
    # parameter determining C/N ratio of biomass during organic residues decomposition: CsurNbio=awb+bwb/CsurNres
    if ("bwb" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "bwb", pft.traits[which(pft.names == "bwb")], overwrite = TRUE)
    }

    # minimum ratio C/N of microbial biomass decomposing organic residues
    if ("minC2N_microbialbiomass" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "cwb", pft.traits[which(pft.names == "minC2N_microbialbiomass")], overwrite = TRUE)
    }
    
    # parameter of organic residues humification: hres = 1 - ahres*CsurNres/(bhres+CsurNres)
    if ("ahres" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "ahres", pft.traits[which(pft.names == "ahres")], overwrite = TRUE)
    }
    
    # parameter of organic residues humification: hres = 1 - ahres*CsurNres/(bhres+CsurNres)
    if ("bhres" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "bhres", pft.traits[which(pft.names == "bhres")], overwrite = TRUE)
    }
  
    
    # TODO: we need a soil PFT
    
    # potential decay rate of microbial biomass decomposing organic residues
    if ("microbialbiomass_decay" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "kbio", pft.traits[which(pft.names == "microbialbiomass_decay")], overwrite = TRUE)
    }
    
    # Carbon assimilation yield by the microbial biomass during crop residues decomposition
    if ("microbialbiomass_C_yield" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "yres", pft.traits[which(pft.names == "microbialbiomass_C_yield")], overwrite = TRUE)
    }
    
    # minimum value of C/N ratio of organic residue (g.g-1)
    if ("CNresmin" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "CNresmin", pft.traits[which(pft.names == "CNresmin")], overwrite = TRUE)
    }
    
    # maximum value of C/N ratio of organic residue (g.g-1)
    if ("CNresmax" %in% pft.names) {
      SticsRFiles::set_param_xml(gen_file, "CNresmax", pft.traits[which(pft.names == "CNresmax")], overwrite = TRUE)
    }

    # skipping, qmulchruis0:
    # amount of mulch above which runoff is suppressed
    
    # skipping, mouillabilmulch:
    # maximum wettability of crop mulch
 
    # skipping, kcouvmlch:
    # extinction coefficient connecting the soil cover to the amount of plant mulch
    
    # skipping, albedomulchresidus:
    # albedo of crop mulch
    
    # skipping, Qmulchdec:
    # maximal amount of decomposable mulch
      
    SticsRFiles::convert_xml2txt(file = gen_file)
    
    this_usm <- grep(names(trait.values)[pft], usmdirs)
    sapply(this_usm, function(x){
      file.copy(file.path(rundir, "tempopar.sti"), file.path(usmdirs[x], "tempopar.sti"), overwrite = TRUE)
    })
    
    ### new formulations 
    # DO NOTHING ELSE FOR NOW
    
    SticsRFiles::convert_xml2txt(file = newf_file)
    sapply(this_usm, function(x){
      file.copy(file.path(rundir, "tempoparv6.sti"), file.path(usmdirs[x], "tempoparv6.sti"), overwrite = TRUE)
    })
  }
  
  
  
  ############################ Prepare Initialization File ##################################
  
  ## this is where we overwrite model initial conditions
  
  # read in template ini file
  ini_xml  <- XML::xmlParse(system.file("pecan_ini.xml", package = "PEcAn.STICS"))
  for(i in seq_along(usmdirs)){
   
    # doesn't really matter what these are called, they will all be eventually 'ficini.txt'
    ini_file <- file.path(rundir, paste0(basename(usmdirs[i]), "_ini.xml"))
    
    # write the ini file 
    XML::saveXML(ini_xml, file = ini_file)
    
    # DO NOTHING FOR NOW
    # but when you do note that this also has multiple options, e.g.
    # SticsRFiles::set_param_xml(file = ini_file, param = "lai0", values = 1, select = "plante", select_value = "1", overwrite = TRUE) 
    if(i > 1){
      # these may or may not be modified depending on how crop cycles work in STICS
      # 'snu' is bare soil
      # fine for annual crops but need to change for perennials
      SticsRFiles::set_param_xml(file = ini_file, param = "stade0",     values = "snu", select = "plante", select_value = "1", overwrite = TRUE)  
      # when snu others are set to 0 by STICS
       
    }else if(!is.null(settings$run$inputs$poolinitcond)){
      ic_path <- settings$run$inputs$poolinitcond$path
      ic_nc   <- ncdf4::nc_open(ic_path)
      
      # initial leaf area index (m2 m-2)
      lai0    <- ncdf4::ncvar_get(ic_nc, "LAI")
      SticsRFiles::set_param_xml(file = ini_file, param = "lai0", values = lai0, select = "plante", select_value = "1", overwrite = TRUE)  
      
      # initial aerial biomass (kg m-2 --> t ha-1)
      masec0    <- ncdf4::ncvar_get(ic_nc, "AGB")
      SticsRFiles::set_param_xml(file = ini_file, param = "masec0", values = PEcAn.utils::ud_convert(masec0, "kg m-2", "t ha-1"), select = "plante", select_value = "1", overwrite = TRUE)
      
      # initial depth of root apex of the crop (m --> cm)
      zrac0    <- ncdf4::ncvar_get(ic_nc, "rooting_depth")
      if(zrac0 < 0.2) zrac0 <- 0.2
      SticsRFiles::set_param_xml(file = ini_file, param = "zrac0", values = PEcAn.utils::ud_convert(zrac0, "m", "cm"), select = "plante", select_value = "1", overwrite = TRUE) 
      
      # initial grain dry weight - haven't started any simulations from this stage yet
      # SticsRFiles::set_param_xml(file = ini_file, param = "magrain0",   values = 0, select = "plante", select_value = "1", overwrite = TRUE)    
      
      # initial N amount in the plant (kg m-2 --> kg ha-1)
      QNplante0    <- ncdf4::ncvar_get(ic_nc, "plant_nitrogen_content")
      SticsRFiles::set_param_xml(file = ini_file, param = "QNplante0",  values = PEcAn.utils::ud_convert(QNplante0, "kg m-2", "kg ha-1"), select = "plante", select_value = "1", overwrite = TRUE) 
      
      # Not anymore
      # initial reserve of biomass (kg m-2 --> t ha-1)
      #resperenne0    <- ncdf4::ncvar_get(ic_nc, "reserve_biomass")
      #SticsRFiles::set_param_xml(file = ini_file, param = "resperenne0", values = PEcAn.utils::ud_convert(resperenne0, "kg m-2", "t ha-1"), select = "plante", select_value = "1", overwrite = TRUE) 
      
      # initial root density in each of the five soil layers
      densinitial    <- ncdf4::ncvar_get(ic_nc, "root_density")
      if(all(densinitial==0)) densinitial[1] <- 0.5 # for lev
      if(zrac0 == 0.2){
        densinitial[2:5] <-0
      }else if(zrac0 < 0.4){
        densinitial[3:5] <-0
      }else if(zrac0 < 0.6){
        densinitial[4:5] <-0
      }else if(zrac0 < 0.8){
        densinitial[5] <-0 #densinitial layers should not be filled if zrac0 is not there
      }
      SticsRFiles::set_param_xml(file = ini_file, param = "densinitial", values = densinitial, select = "plante", select_value = "1", overwrite = TRUE) 
      
      # default 'lev'
      # SticsRFiles::set_param_xml(file = ini_file, param = "stade0", values = "plt", select = "plante", select_value = "1", overwrite = TRUE)  
      
      ncdf4::nc_close(ic_nc)
    }
    
    SticsRFiles::convert_xml2txt(file = ini_file)
    file.rename(file.path(rundir, "ficini.txt"), file.path(usmdirs[i], "ficini.txt"))
  }

  
  ############################ Prepare Soils ##################################
  
  ## this is where we modify soil characteristics
  
  #### THERE IS SOME BUG IN SticsRFiles::convert_xml2txt FOR SOLS.XML
  #### I NOW PUT TXT VERSION TO THE MODEL PACKAGE: param.sol
  #### TODO: revise others to have txt templates directly in the package
  
  # # changed from FINERT to finert and moved to the sols.xml
  # # initial fraction of soil organic N inactive for mineralisation (= stable SON/ total SON)
  # if ("FINERT" %in% soil.names) {
  #   SticsRFiles::set_param_xml(gen_file, "finert", soil_params[which(soil.names == "FINERT")], overwrite = TRUE)
  # }
  
  sols_file <- file.path(rundir, "param.sol")
  
  # cp template sols file (txt)
  file.copy(system.file("param.sol", package = "PEcAn.STICS"), sols_file)
  
  # check param names
  # sols_vals  <- SticsRFiles::get_soil_txt(sols_file)
  
  str_ns <- paste0(as.numeric(settings$run$site$id) %/% 1e+09, "-", as.numeric(settings$run$site$id) %% 1e+09)
  
  # I guess not important what this is called as long as it's consistent in usms
  SticsRFiles::set_soil_txt(file = sols_file, param="typsol", value=paste0("sol", str_ns))
  
  if(!is.null(settings$run$inputs$poolinitcond)){
    ic_path <- settings$run$inputs$poolinitcond$path
    ic_nc   <- ncdf4::nc_open(ic_path)
    
    # pH
    pH    <- ncdf4::ncvar_get(ic_nc, "pH")
    pH    <- round(pH[1], digits = 1) # STICS uses 1 pH value
    SticsRFiles::set_soil_txt(file = sols_file, param="pH", value=pH)
 
    sapply(1:5, function(x) SticsRFiles::set_soil_txt(file = sols_file, param="epc", value=20, layer = x)) 
    
    # volume_fraction_of_water_in_soil_at_field_capacity
    hccf    <- ncdf4::ncvar_get(ic_nc, "volume_fraction_of_water_in_soil_at_field_capacity")
    hccf    <- round(hccf*100, digits = 2)
    sapply(seq_along(hccf), function(x) SticsRFiles::set_soil_txt(file = sols_file, param="hccf", value=hccf[x], layer = x)) 
    
    # volume_fraction_of_condensed_water_in_soil_at_wilting_point
    hminf    <- ncdf4::ncvar_get(ic_nc, "volume_fraction_of_condensed_water_in_soil_at_wilting_point")
    hminf    <- round(hminf*100, digits = 2)
    sapply(seq_along(hminf), function(x) SticsRFiles::set_soil_txt(file = sols_file, param="hminf", value=hminf[x], layer = x)) 
    
    # soil_organic_nitrogen_content
    Norg    <- ncdf4::ncvar_get(ic_nc, "soil_organic_nitrogen_content")
    Norg    <- round(Norg[1]*100, digits = 2) # STICS uses 1 Norg value
    SticsRFiles::set_soil_txt(file = sols_file, param="Norg", value=Norg) 

    # mass_fraction_of_clay_in_soil
    argi    <- ncdf4::ncvar_get(ic_nc, "mass_fraction_of_clay_in_soil")
    argi    <- round(argi[1]*100, digits = 0) # STICS uses 1 argi value
    SticsRFiles::set_soil_txt(file = sols_file, param="argi", value=argi) 
    
    # soil_density (kg m-3 --> g cm-3)
    DAF    <- ncdf4::ncvar_get(ic_nc, "soil_density")
    DAF    <- round(PEcAn.utils::ud_convert(DAF, "kg m-3", "g cm-3"), digits = 1)
    sapply(seq_along(DAF), function(x) SticsRFiles::set_soil_txt(file = sols_file, param="DAF", value=DAF[x], layer = x)) 
    
    # c2n_humus
    #CsurNsol0    <- ncdf4::ncvar_get(ic_nc, "c2n_humus")
    #SticsRFiles::set_soil_txt(file = sols_file, param="CsurNsol", value=CsurNsol0) 
    
    # epd 
    epd <- rep(10, 5)
    sapply(seq_along(epd), function(x) SticsRFiles::set_soil_txt(file = sols_file, param="epd", value=epd[x], layer = x)) 
    
    ncdf4::nc_close(ic_nc)
  }
  
  file.copy(sols_file, file.path(usmdirs, "param.sol"))
  
  # DO NOTHING ELSE FOR NOW

  # this has some bug for sols.xml
  # SticsRFiles::convert_xml2txt(file = sols_file, javastics = javastics_path)
  
  ######################### Prepare Weather Station File ###############################
  
  ## this is where we modify more initial conditions and site characteristics
  
  # read in template sta file
  sta_xml  <- XML::xmlParse(system.file("pecan_sta.xml", package = "PEcAn.STICS"))
  
  # not important what it's called, will be 'station.txt' in the end
  sta_file <- file.path(rundir, paste0(str_ns, "_sta.xml"))
  
  XML::saveXML(sta_xml, file = sta_file)
  
  # change latitude
  SticsRFiles::set_param_xml(sta_file, "latitude", settings$run$site$lat, overwrite = TRUE)
  
  SticsRFiles::convert_xml2txt(file = sta_file)
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
  tec_df <- data.frame(Tec_name = "tmp_tec.xml")
  
  # these shouldn't be empty even if we don't use them (values from timothy example in STICS)
  tec_df$iplt0 <- 999 # date of sowing
  tec_df$profsem <- 2 # depth of sowing
  tec_df$densitesem <- 100 # plant sowing density
  tec_df$variete <- 1 # cultivar number corresponding to the cultivar name in the plant file (could be passed via a field activity file)
  tec_df$irecbutoir <- 999 #latest date of harvest (imposed if the crop cycle is not finished at this date)
  tec_df$profmes <- 120 # depth of measurement of the soil water reserve (cm)
  #tec_df$engrais <- 1 # fertilizer type
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
      
      usm_years <- c(sapply(strsplit(sub(".*_", "", basename(usmdirs[usmi])), "-"), function(x) (as.numeric(x))))
      # note that usm years can overlap, may need more sophisticated checks
      dseq_sub <- dseq[lubridate::year(dseq) %in% usm_years]
      
      events_sub <- events_file$events[lubridate::year(events_file$events$date) %in% usm_years, ]
      
      if("planting" %in% events_sub$mgmt_operations_event){
        
        pl_date <- events_sub$date[events_sub$mgmt_operations_event == "planting"]
        tec_df$iplt0 <- lubridate::yday(as.Date(pl_date))
        
        profsem <- events_sub$planting_depth[events_sub$mgmt_operations_event == "planting"]
        if(!is.null(profsem)){
          tec_df$profsem <- as.numeric(profsem) # depth of sowing
        }
        
        densitesem <- events_sub$planting_sowing_density[events_sub$mgmt_operations_event == "planting"]
        if(!is.null(densitesem)){
          tec_df$densitesem <- as.numeric(densitesem) # plant sowing density
        }
        
        # any other?
      }
      
      if("harvest" %in% events_sub$mgmt_operations_event){
        # param names
        h_param_names <- c("julfauche"  , # date of each cut for forage crops, julian.d
                           "hautcoupe"  , # cut height for forage crops, m
                           "lairesiduel", # residual LAI after each cut of forage crop, m2 m-2
                           "msresiduel" , # residual aerial biomass after a cut of a forage crop, t.ha-1
                           "anitcoupe",
                           "engraiscoupe",
                           "tauxexportfauche",
                           "restit",
                           "mscoupemini")   # amount of mineral N added by fertiliser application at each cut of a forage crop, kg.ha-1


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
            if("frg" %in% tolower(harvest_sub$harvest_crop) |
               "wcl" %in% tolower(harvest_sub$harvest_crop)){
              tec_df$irecbutoir <- 999
              if(!is.null(events_file$rotation)){
                tind <-  which(dseq_sub == as.Date(events_file$rotation$rotation_end[usmi]))  + lubridate::yday(dseq_sub[1]) - 1
                tec_df$irecbutoir <-  ifelse(length(tind) == 0, 999, tind)
              }
            }else{
              tec_df$irecbutoir <- harvest_df$julfauche
            }
            harvest_df$hautcoupe <- as.numeric(harvest_sub$harvest_cut_height[harvest_sub$date==harvest_sub$date[hrow]]) # # cut height for forage crops
            harvest_df$hautcoupe <- ifelse(harvest_df$hautcoupe == -99, 0.05, harvest_df$hautcoupe)
            harvest_df$lairesiduel <- ifelse(harvest_df$hautcoupe < 0.08, 0.2, 0.8) # hardcode for now
            harvest_df$msresiduel <- ifelse(harvest_df$hautcoupe < 0.08, 0.05, 0.3) # residual aerial biomass after a cut of a forage crop (t ha-1)
            harvest_df$anitcoupe <- 21 # amount of mineral N added by fertiliser application at each cut of a forage crop (kg ha-1)
            harvest_df$engraiscoupe <- 0
            harvest_df$tauxexportfauche <- 0
            harvest_df$restit <- 0
            harvest_df$mscoupemini <- 0
          }
          
          colnames(harvest_df) <- paste0(h_param_names, "_", hrow)
          harvest_list[[hrow]] <- harvest_df
        }
        harvest_tec <- do.call("cbind", harvest_list) 
        
        # need to get these from field data
        # cut crop - 1:yes, 2:no
        if("frg" %in% tolower(harvest_sub$harvest_crop) | "wcl" %in% tolower(harvest_sub$harvest_crop)){
          harvest_tec$codefauche <- 1
        }else{
          harvest_tec$codefauche <- 2 
        }
        #harvest_tec$mscoupemini <- 0 # min val of aerial biomass to make a cut
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
        
        usm_tec_df$ratiol <- 0
        
        SticsRFiles::gen_tec_xml(param_df = usm_tec_df,
                                 file=system.file("pecan_tec.xml", package = "PEcAn.STICS"),
                                 out_dir = usmdirs[usmi])
        
        # TODO: more than 1 USM, rbind
        
        SticsRFiles::convert_xml2txt(file = file.path(usmdirs[usmi], "tmp_tec.xml"))
        
      
     } # end-loop over usms
    } # TODO: if no events file is given modify other harvest parameters, e.g. harvest decision
  
  ################################ Prepare USM file ######################################

  # loop for each USM
  #ncodesuite <- ifelse(length(usmdirs) > 1, 1,0)
  
  for(usmi in seq_along(usmdirs)){
    
    #usm_years <- years_requested[(usmi*2-1):(usmi*2)]
    usm_years <- c(sapply(strsplit(sub(".*_", "", basename(usmdirs[usmi])), "-"), function(x) (as.numeric(x))))
    dseq_sub <- dseq[lubridate::year(dseq) %in% usm_years]
    
    # read in template USM (Unit of SiMulation) file, has the master settings, file names etc.
    usm_file <- file.path(usmdirs[usmi], "new_travail.usm")
    
    # cp template usm file 
    file.copy(system.file("template.usm", package = "PEcAn.STICS"), usm_file)
    
    # Type of LAI simulation 
    # 0 = culture (LAI calculated by the model), 1 = feuille (LAI forced)
    SticsRFiles::set_usm_txt(usm_file, "codesimul", "culture", append = FALSE) # hardcode for now
    
    # use optimization
    # 0 = no;  1 = yes main plant; 2 = yes associated plant
    SticsRFiles::set_usm_txt(usm_file, "codeoptim", 0, append = FALSE) 
    
    # option to simulate several
    # successive USM (0 = no, 1 = yes)
    if(usmi == 1){
      SticsRFiles::set_usm_txt(usm_file, "codesuite", 0, append = FALSE)
    }else{
      SticsRFiles::set_usm_txt(usm_file, "codesuite", 1, append = FALSE)
    }
     

    # number of simulated plants (sole crop=1; intercropping=2)
    SticsRFiles::set_usm_txt(usm_file, "nbplantes", 1, append = FALSE) # hardcode for now
    
    # pft name
    SticsRFiles::set_usm_txt(usm_file, "nom", basename(usmdirs[usmi]), append = FALSE)
    
    
    ## handle dates, also for partial year(s)
    ## needs developing with longer runs
    if(usmi == 1){
      # beginning day of the simulation (julian.d)
      # end day of the simulation (julian.d) (at the end of consecutive years, i.e. can be greater than 366)
      SticsRFiles::set_usm_txt(usm_file, "datedebut", lubridate::yday(settings$run$start.date), append = FALSE)
      SticsRFiles::set_usm_txt(usm_file, "datefin", (lubridate::yday(settings$run$start.date) + length(dseq_sub) - 1), append = FALSE)
    }else{
      SticsRFiles::set_usm_txt(usm_file, "datedebut", 1, append = FALSE) # for now!
      SticsRFiles::set_usm_txt(usm_file, "datefin", length(dseq_sub), append = FALSE)
    }
    
    # name of the initialization file
    SticsRFiles::set_usm_txt(usm_file, "finit", paste0(basename(usmdirs[usmi]), "_ini.xml"), append = FALSE)
    
    # soil number
    SticsRFiles::set_usm_txt(usm_file, "numsol", 1, append = FALSE)
    
    # name of the soil in the sols.xml file
    SticsRFiles::set_usm_txt(usm_file, "nomsol", paste0("sol", str_ns), append = FALSE)
    
    # name of the weather station file
    SticsRFiles::set_usm_txt(usm_file, "fstation", paste0(str_ns, "_sta.xml"), append = FALSE)
    
    # name of the first climate file
    SticsRFiles::set_usm_txt(usm_file, "fclim1", paste0(str_ns, ".", usm_years[1]), append = FALSE)
    
    # name of the last climate file
    if(length(usm_years) == 2){
      SticsRFiles::set_usm_txt(usm_file, "fclim2", paste0(str_ns, ".", usm_years[2]), append = FALSE)
    }else{
      # repeat same year
      SticsRFiles::set_usm_txt(usm_file, "fclim2", paste0(str_ns, ".", usm_years[1]), append = FALSE)
    }
    
    
    # number of simulation years
    SticsRFiles::set_usm_txt(usm_file, "nbans", length(unique(usm_years)), append = FALSE) # hardcode for now
    
    # number of calendar years involved in the crop cycle
    # 1 = 1 year e.g. for spring crops, 0 = two years, e.g. for winter crops
    culturean <- ifelse( length(unique(usm_years)) == 2, 0, 1)
    SticsRFiles::set_usm_txt(usm_file, "culturean", culturean, append = FALSE) #hardcoding this for now, if passed as a trait from priors it breaks sensitivity analysis
    # probably best to pass this via the json file
    
    # name of the plant file for main plant 
    if(length(plt_files) < usmi){
      # multiple usms, 1 plt file = same spp, consecutive rotations, but hacky
      SticsRFiles::set_usm_txt(usm_file, "fplt1", basename(plt_files[[1]]), append = FALSE) 
    }else{
      SticsRFiles::set_usm_txt(usm_file, "fplt1", basename(plt_files[[usmi]]), append = FALSE) 
    }
    
    
    # name of the technical file for main plant
    # does this even matter?
    SticsRFiles::set_usm_txt(usm_file, "ftec1", "tmp_tec.xml", append = FALSE) 
    
    # name of the LAI forcing file for main plant (null if none)
    SticsRFiles::set_usm_txt(usm_file, "flai1", "default.lai", append = FALSE) # hardcode for now, doesn't matter when codesimul==0
    
    # TODO: more than 1 PFTs 
    # STICS can run 2 PFTs max: main crop + intercrop
  }

  

  
  ################################ Prepare Run ######################################
  
  # symlink climate files
  met_path <- settings$run$inputs$met$path
  
  for(usmi in seq_along(usmdirs)){
    
    usm_years <- c(sapply(strsplit(sub(".*_", "", basename(usmdirs)[usmi]), "-"), function(x) (as.numeric(x))))
    dseq_sub <- dseq[lubridate::year(dseq) %in% usm_years]
    
    clim_list <- list() # temporary solution
    for(clim in seq_along(usm_years)){
      # currently assuming only first year file has been passed to the settings, modify met2model if changing the logic
      met_file  <- gsub(paste0(lubridate::year(settings$run$start.date), ".climate"), paste0(usm_years[clim], ".climate"), met_path)
      clim_list[[clim]] <- utils::read.table(met_file)
    }
    clim_run <- do.call("rbind", clim_list)
    utils::write.table(clim_run, file.path(usmdirs[usmi], "climat.txt"), col.names = FALSE, row.names = FALSE)
    
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
