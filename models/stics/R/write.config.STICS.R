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
  
  # Convert pecan parameters to stics names
  trait.values <- pecan2stics(trait.values)
  # read in template plt file, has all the formalisms
  plt_xml  <- XML::xmlParse(system.file("crop_plt.xml", package = "PEcAn.STICS"))
  
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
    
    # Apply changes to those parameters specified by trait.values for this pft.
    if (!is.null(pft.traits)) {
      SticsRFiles::set_param_xml(plant_file, param = names(pft.traits), values = as.list(unname(pft.traits)), overwrite = TRUE)
    }
    
    plt_files[[pft]] <- plant_file
    
    # to learn the parameters in a plant file
    # SticsRFiles::get_param_info()
    
    # to see parameters per formalism
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "phasic development")
    # unlist(values)
    
    # Creating a dataframe of parameter names and their values for feeding into SticsRFiles::set_param_xml.
    # Note that the parameters in this data frame are either hardcoded for now or otherwise require special treatment.
    plt_df <- data.frame(codebfroid = 2) # vernalization requirement, hardcoding for now, 2==yes. 
    
    # name code of plant in 3 letters
    # a handful of plants have to have specific codes, e.g. forages need to be 'fou' and vine needs to be 'vig'
    # but others can be anything? if not, either consider a LUT or passing via settings
    if(names(trait.values)[pft] %in% c("frg", "wcl", "alf")){ 
      plt_df$codeplante <- "fou"
      plt_df$codeperenne <- 2
    }else{
      plt_df$codeplante <- base::substr(names(trait.values)[pft],1,3)
      plt_df$codeperenne <- 1
    }
    
    # nbfeuilplant, leaf number per plant when planting, default 0, skipping for now
    
    # this is a switch, for now hardcoding to have delay at the beginning of the crop (1)
    # if starting the simulation from a later stage (e.g. lev) this has no effect
    # codegermin, option of simulation of a germination phase or a delay at the beginning of the crop (1) or direct starting (2)
    plt_df$codegermin <- 1
    
    # skipping the other parameters related to this switch for now
    # potgermi: soil water potential under which seed imbibition is impeded
    # nbjgerlim: maximum number of days after grain imbibition allowing full germination
    # propjgermin: minimal proportion of the duration nbjgerlim when the temperature is higher than the temperature threshold Tdmax
    
    # temperature beyond which foliar growth stops
    if ("tcxstop" %in% pft.names | "tdmax" %in% pft.names) {
      #  tcxstop must be > tdmax, priors should be set that way, and we can let the simulation fail afterwards, but putting a warning here
      #  Retrieve the new values if they exist, otherwise read them from the plant file
      if ("tcxstop" %in% pft.names) {
        tcxstop <- pft.traits[which(pft.names == "tcxstop")]
      } else {
        tcxstop   <- SticsRFiles::get_param_xml(plant_file, param="tcxstop", select = "formalisme", select_value = "leaves")[[1]][[1]]
      }
      if ("tdmax" %in% pft.names) {
        tdmax <- pft.traits[which(pft.names == "tdmax")]
      } else {
        tdmax   <- SticsRFiles::get_param_xml(plant_file, param="tdmax", select = "formalisme", select_value = "phasic development")[[1]][[1]]
      }
      if(tcxstop < tdmax){
        PEcAn.logger::logger.warn("tcmax_foliar_growth value (", tcxstop, ") should be greater than tdmax (", tdmax, ").")
      }
      # TODO: Do we force one of these to change or let the simulation fail?
    }
    
    # option to activate the N influence on root partitioning within the soil profile (1 = yes, 2 = no)
    plt_df$codazorac <- 1
    
    # cultivar parameters
    # values = SticsRFiles::get_param_xml(plant_file, select = "formalisme", select_value = "cultivar parameters")
    
    # there are multiple cultivars (varietes) in plt file
    # for now I assume we will always use only #1 in simulations 
    # hence, _tec file will always say variete==1, if you change the logic don't forget to update handling of the _tec file accordingly
    # by default set_param_xml modifies the given parameter in all cultivars.
    
    # Set the parameters that have been added to plt_df in the plant file.
    SticsRFiles::set_param_xml(plant_file, names(plt_df), plt_df[1, ], overwrite = TRUE)
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
  
  # these parameters won't change as crop changes in a continuous rotation
  
  # Convert pecan parameters to stics names for soil
  # prepare for pecan2stics call, expects a list
  soil_params_list <- list()
  soil_params_list[[1]] <- soil_params
  soil_params <- pecan2stics(soil_params_list)[[1]]
  soil.names <- names(soil_params)
  
  for (pft in seq_along(trait.values)) {
    
    
    if(names(trait.values)[pft] == "env"){
      next
    }
    
    gen_xml  <- XML::xmlParse(system.file("param_gen.xml", package = "PEcAn.STICS"))
    gen_file <- file.path(rundir, "param_gen.xml")
    XML::saveXML(gen_xml, file = gen_file)
    
    # This input file is created from the template and not modified.
    newf_xml  <- XML::xmlParse(system.file("param_newform.xml", package = "PEcAn.STICS"))
    newf_file <- file.path(rundir, "param_newform.xml")
    XML::saveXML(newf_xml, file = newf_file)  
    
    
    # Creating a dataframe of parameter names and their values for feeding into SticsRFiles::set_param_xml.
    # Note that the parameters in this data frame are either hardcoded for now or otherwise require special treatment.
    gen_df <- data.frame(codeinitprec = ifelse(length(usmdirs>1), 1, 2)) # reset initial conditions in chained simulations
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names  <- names(pft.traits)
    
    # Apply changes to those parameters specified by trait.values for this pft.
    # Currently no checking/differentiation between parameters that are in the plant xml vs these xmls, but, for now, SticsRFiles just throws a warning when the parameter is not in that file.
    if (!is.null(pft.traits)) {
      SticsRFiles::set_param_xml(gen_file, param = names(pft.traits), values = as.list(unname(pft.traits)), overwrite = TRUE)
    }
    
    # Set the parameters that have been added to gen_df in the param_gen file.
    SticsRFiles::set_param_xml(gen_file, names(gen_df), gen_df[1, ], overwrite = TRUE)
    
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
      SticsRFiles::set_param_xml(file = ini_file, param = "stade0",     values = "dor", select = "plante", select_value = "1", overwrite = TRUE)  
      # when snu others are set to 0 by STICS
      
    }else if(!is.null(settings$run$inputs$poolinitcond)){
      ic_path <- settings$run$inputs$poolinitcond$path
      ic_nc   <- ncdf4::nc_open(ic_path)
      
      # initial leaf area index (m2 m-2)
      lai0    <- ncdf4::ncvar_get(ic_nc, "LAI")
      
      # initial aerial biomass (kg m-2 --> t ha-1)
      masec0    <- ncdf4::ncvar_get(ic_nc, "AGB")
      
      # initial depth of root apex of the crop (m --> cm)
      zrac0    <- ncdf4::ncvar_get(ic_nc, "rooting_depth")
      if(zrac0 < 0.2) zrac0 <- 0.2
      
      # initial grain dry weight - haven't started any simulations from this stage yet
      # SticsRFiles::set_param_xml(file = ini_file, param = "magrain0",   values = 0, select = "plante", select_value = "1", overwrite = TRUE)    
      
      # initial N amount in the plant (kg m-2 --> kg ha-1)
      QNplante0    <- ncdf4::ncvar_get(ic_nc, "plant_nitrogen_content")
      QNplante0    <- PEcAn.utils::ud_convert(QNplante0, "kg m-2", "kg ha-1")
      
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
      
      # default 'lev'
      # SticsRFiles::set_param_xml(file = ini_file, param = "stade0", values = "plt", select = "plante", select_value = "1", overwrite = TRUE)  
      
      ic_list <- list(lai0 = lai0, masec0 = masec0, zrac0 = zrac0, QNplante0 = QNplante0, densinitial = densinitial)
      
      SticsRFiles::set_param_xml(file = ini_file, param = names(ic_list), values = ic_list, select = "plante", select_value = "1", overwrite = TRUE) 
      
      ncdf4::nc_close(ic_nc)
    }
    
    SticsRFiles::convert_xml2txt(file = ini_file)
    file.rename(file.path(rundir, "ficini.txt"), file.path(usmdirs[i], "ficini.txt"))
  }
  
  
  ############################ Prepare Soils ##################################
  
  ## this is where we modify soil characteristics
  
  #### THERE IS SOME BUG IN SticsRFiles::convert_xml2txt FOR SOLS.XML
  #### I NOW PUT TXT VERSION TO THE MODEL PACKAGE: param.sol
  #### sols_file <- file.path(rundir, "param.sol")
  #### Note this has changed now, if all is working might delete these comments
  sols_file <- file.path(rundir, "sols.xml")
  
  site_id <- tryCatch(
    as.numeric(settings$run$site$id),
    warning = function(w) as.character(settings$run$site$id)
  )
  if (is.numeric(site_id) && site_id > 1e9) {
    # assume this is a BETYdb id, condense for readability
    str_ns <- paste0(site_id %/% 1e9, "-", site_id %% 1e9)
  } else {
    #treat as string, leave as-is
    str_ns <- site_id
  }

  soils_df <- data.frame(soil_name = paste0("sol", str_ns))
  
  if(!is.null(settings$run$inputs$poolinitcond)){
    ic_path <- settings$run$inputs$poolinitcond$path
    ic_nc   <- ncdf4::nc_open(ic_path)
    
    # pH
    pH    <- ncdf4::ncvar_get(ic_nc, "pH")
    soils_df$pH <- round(pH[1], digits = 1) # STICS uses 1 pH value
    
    # Thickness of each soil layer. This sets all (five) at 20cm, to set individual ones use epc_1, epc_2, etc.
    soils_df$epc <- 20
    
    # volume_fraction_of_water_in_soil_at_field_capacity
    hccf    <- ncdf4::ncvar_get(ic_nc, "volume_fraction_of_water_in_soil_at_field_capacity")
    hccf    <- round(hccf*100, digits = 2)
    names(hccf) <- paste0("HCCF_", c(1:length(hccf)))
    soils_df <- cbind(soils_df, t(hccf))
    
    # volume_fraction_of_condensed_water_in_soil_at_wilting_point
    hminf    <- ncdf4::ncvar_get(ic_nc, "volume_fraction_of_condensed_water_in_soil_at_wilting_point")
    hminf    <- round(hminf*100, digits = 2)
    names(hminf) <- paste0("HMINF_", c(1:length(hminf)))
    soils_df <- cbind(soils_df, t(hminf))
    
    # soil_organic_nitrogen_content
    Norg    <- ncdf4::ncvar_get(ic_nc, "soil_organic_nitrogen_content")
    Norg    <- round(Norg[1]*100, digits = 2) # STICS uses 1 Norg value
    soils_df$norg <- Norg
    
    # mass_fraction_of_clay_in_soil
    argi    <- ncdf4::ncvar_get(ic_nc, "mass_fraction_of_clay_in_soil")
    argi    <- round(argi[1]*100, digits = 0) # STICS uses 1 argi value
    soils_df$argi <- argi
    
    # soil_density (kg m-3 --> g cm-3)
    DAF    <- ncdf4::ncvar_get(ic_nc, "soil_density")
    DAF    <- round(PEcAn.utils::ud_convert(DAF, "kg m-3", "g cm-3"), digits = 1)
    names(DAF) <- paste0("DAF_", c(1:length(DAF)))
    soils_df <- cbind(soils_df, t(DAF))
    
    # c2n_humus
    # CsurNsol0    <- ncdf4::ncvar_get(ic_nc, "c2n_humus")
    # soils_df$CsurNsol0 <- CsurNsol0
    
    # epd: thickness of mixing cells in each soil layer ( = 2 * dispersion length)
    epd <- rep(10, 5)
    names(epd) <- paste0("epd_", c(1:length(epd)))
    soils_df <- cbind(soils_df, t(epd))
    
    ncdf4::nc_close(ic_nc)
  }
  
  SticsRFiles::gen_sols_xml(sols_file, param_df = soils_df, template = system.file("sols.xml", package = "PEcAn.STICS"))
  SticsRFiles::convert_xml2txt(file = sols_file)
  file.copy(file.path(rundir, "param.sol"), file.path(usmdirs, "param.sol"))
  
  # check param values
  # sols_vals  <- SticsRFiles::get_soil_txt(file.path(rundir, "param.sol"), stics_version = SticsRFiles::get_stics_versions_compat()$latest_version)
  
  # DO NOTHING ELSE FOR NOW
  
  
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
  
  ################################ Prepare Climate file ######################################
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


# ==================================================================================================#
#' Function to translate pecan param names and units to stics names and units.
#' @export
#' @param trait.values trait.values, list
#' @return translated list
#' @author Quentin Bell
# Based on pecan2lpjguess function by Istem Fer https://github.com/PecanProject/pecan/blob/develop/models/lpjguess/R/write.config.LPJGUESS.R#L229
pecan2stics <- function(trait.values){
  
  # TODO :match all stics and pecan names
  vartable <- dplyr::tribble(
    ~sticsname, ~pecanname, ~sticsunits, ~pecanunits, ~sticsfile, 
    # Plant and soil related parameters
    "abscission", "fracLeafFall",	NA, NA, "plt.xml", 
    "adens", "adens",	NA, NA, "plt.xml", 
    "adil", "adil",	NA, NA, "plt.xml", 
    "ahres", "ahres",	NA, NA, "param_gen.xml", 
    "akres", "ORdecomp_par",	NA, NA, "param_gen.xml", 
    "ampfroid",	"vernalization_TAmp",	NA, NA, "plt.xml", 
    "awb", "awb",	NA, NA, "param_gen.xml", 
    "bdens", "dens_comp",	NA, NA, "plt.xml", 
    "bdil", "bdil",	NA, NA, "plt.xml", 
    "belong", "belong",	NA, NA, "plt.xml", 
    "beta",	"maxTPincrease_waterstress",	NA, NA, "param_gen.xml", 
    "bhres", "bhres",	NA, NA, "param_gen.xml", 
    "bkres", "ORdecomp_rate",	NA, NA, "param_gen.xml",  
    "bwb", "bwb",	NA, NA, "param_gen.xml", 
    "celong", "celong",	NA, NA, "plt.xml", 
    "CNresmax", "CNresmax",	NA, NA, "param_gen.xml", 
    "CNresmin", "CNresmin",	NA, NA, "param_gen.xml", 
    "coefamflax", "coefamflax",	NA, NA, "plt.xml", 
    "coefb", "rad_on_conversion_eff",	NA, NA, "param_gen.xml", 
    "coefdrpmat", "coefdrpmat",	NA, NA, "plt.xml", 
    "coefflodrp", "coefflodrp",	NA, NA, "plt.xml", 
    "coeflaxsen", "coeflaxsen",	NA, NA, "plt.xml", 
    "coeflevamf", "coeflevamf",	NA, NA, "plt.xml", 
    "coeflevdrp", "coeflevdrp",	NA, NA, "plt.xml", 
    "coefmshaut",	"biomass2usefulheight",	NA, NA, "plt.xml", 
    "coefsenlan", "coefsenlan",	NA, NA, "plt.xml", 
    "contrdamax",	"db_reduc_rgr_max",	NA, NA, "plt.xml", 
    "CroCo", "fOR_decomp",	NA, NA, "param_gen.xml", 
    "croirac", "croirac",	NA, NA, "plt.xml", 
    "cwb", "minC2N_microbialbiomass", NA, NA, "param_gen.xml", 
    "dacohes", "bd_rootgrowth_reduced", NA, NA, "param_gen.xml", 
    "daseuilbas",	"bd_rootgrowth_maximal", NA, NA, "param_gen.xml", 
    "daseuilhaut", "bd_rootgrowth_impossible", NA, NA, "param_gen.xml",
    "debsenrac", "root_sen_dday", "round", "0", "plt.xml", 
    "difN",	"difN_FC", NA, NA, "param_gen.xml", 
    "diftherm",	"soil_thermal_diffusivity", NA, NA, "param_gen.xml", 
    "dlaimaxbrut", "lai_max_rate", NA, NA, "plt.xml", 
    "dlaimin", "lai_growth_rate_accelerating", NA, NA, "plt.xml", 
    "draclong", "rootlength_prod_max", NA, NA, "plt.xml", 
    "durvieF", "leaf_lifespan_max", NA, NA, "plt.xml", 
    "durviesupmax",	"relative_addlifespan_DT_excessN", NA, NA, "plt.xml", 
    "efcroijuv", "RUE_juv", NA, NA, "plt.xml", 
    "efcroirepro", "RUE_rep", NA, NA, "plt.xml", 
    "efcroiveg", "RUE_veg", NA, NA, "plt.xml", 
    "elmax", "coleoptile_elong_dark_max", NA, NA, "plt.xml", 
    "extin", "extinction_coefficient_diffuse", NA, NA, "plt.xml", 
    "fhminsat", "fhminsat", NA, NA, "param_gen.xml", 
    "FINERT", "FINERT", NA, NA, "sols.xml", 
    "fNCbiomin", "fNCbiomin", NA, NA, "param_gen.xml", 
    "fredkN",	"Nlim_reductionOMdecomp", NA, NA, "param_gen.xml",
    "fredlN",	"Nlim_reductionMBdecomp", NA, NA, "param_gen.xml", 
    "fredNsup", "fredNsup", NA, NA, "param_gen.xml", 
    "FTEMh", "T_p1_Hdecomp_rate", NA, NA, "param_gen.xml", 
    "FTEMha",	"T_p2_Hdecomp_rate", NA, NA, "param_gen.xml", 
    "FTEMr", "FTEMr", NA, NA, "param_gen.xml", 
    "FTEMra", "FTEMra", NA, NA, "param_gen.xml", 
    "h2ofeuilverte", "water_content_TLP_leaf", NA, NA, "plt.xml", 
    "hautmax", "HTMAX", NA, NA, "plt.xml", 
    "hautbase", "height", NA, NA, "plt.xml", 
    "hminm", "hminm", NA, NA, "param_gen.xml", 
    "hoptm", "hoptm", NA, NA, "param_gen.xml", 
    "INNmin", "INNmin", NA, NA, "plt.xml", 
    "innsen", "innsen", NA, NA, "plt.xml", 
    "innturgmin", "innturgmin", NA, NA, "plt.xml", 
    "julvernal", "vernalization_init", "round", "0", "plt.xml", 
    "jvcmini", "vernalization_days_min", "round", "0", "plt.xml", 
    "kbio",	"microbialbiomass_decay", NA, NA, "param_gen.xml", 
    "khaut", "LAI2height", NA, NA, "plt.xml", 
    "Kmabs1", "Kmabs1", NA, NA, "plt.xml", 
    "kmax",	"crop_water_max", NA, NA, "plt.xml", 
    "laicomp", "lai_comp", NA, NA, "plt.xml", 
    "longsperac", "SRL", NA, NA, "plt.xml", 
    "lvfront", "rootdens_at_apex", NA, NA, "plt.xml", 
    "lvopt", "lvopt", NA, NA, "param_gen.xml", 
    "masecNmax", "masecNmax", NA, NA, "plt.xml", 
    "maxazorac", "maxazorac", NA, NA, "plt.xml", 
    "minazorac", "minazorac", NA, NA, "plt.xml", 
    "minefnra", "minefnra", NA, NA, "plt.xml", 
    "nlevlim1",	"days2reduced_emergence_postgerm", "round", "0", "plt.xml", 
    "nlevlim2",	"days2stopped_emergence_postgerm", "round", "0", "plt.xml", 
    "Nmeta", "Nmeta", NA, NA, "plt.xml", 
    "Nreserve", "Nreserve", NA, NA, "plt.xml", 
    "parazofmorte", "parazofmorte", NA, NA, "plt.xml", 
    "pentlaimax", "pentlaimax", NA, NA, "plt.xml", 
    "pHmaxvol", "pHmaxvol", NA, NA, "param_gen.xml", 
    "pHminvol", "pHminvol", NA, NA, "param_gen.xml", 
    "phobase", "phobase", NA, NA, "plt.xml", 
    "phosat", "phosat", NA, NA, "plt.xml", 
    "phyllotherme",	"phyllochron", NA, NA, "plt.xml", 
    "plNmin", "plNmin", NA, NA, "param_gen.xml", 
    "pminruis",	"precmin4runoff", NA, NA, "param_gen.xml", 
    "Primingmax", "Primingmax", NA, NA, "param_gen.xml", 
    "prophumtassrec",	"SMC_compaction_delay_harvest", NA, NA, "param_gen.xml", 
    "prophumtasssem",	"SMC_compaction_delay_sow", NA, NA, "param_gen.xml", 
    "proprac", "root2aerial_harvest", NA, NA, "param_gen.xml", 
    "psihucc", "SWP_FC", NA, NA, "param_gen.xml", 
    "psihumin",	"SWP_WP", NA, NA, "param_gen.xml", 
    "psisto", "psi_stomata_closure", NA, NA, "plt.xml", # psisto, potential of stomatal closing (absolute value) (bars). note: units in betyDB are m, but Istem's prior is for testing 
    "psiturg", "leaf_psi_tlp", NA, NA, "plt.xml", 
    "QNpltminINN", "QNpltminINN", NA, NA, "param_gen.xml", 
    "rapsenturg", "rapsenturg", NA, NA, "plt.xml", 
    "ratiodurvieI",	"early2last_leaflife", NA, NA, "plt.xml", 
    "ratiosen",	"senes2total_biomass", NA, NA, "plt.xml", 
    "rayon", "rayon", NA, NA, "plt.xml", 
    "rdrain", "rdrain", NA, NA, "param_gen.xml", 
    "remobres", "remobres", NA, NA, "plt.xml", 
    "sensrsec",	"rootsens2drought", NA, NA, "plt.xml", 
    "slamax", "SLAMAX",	"cm2 g-1", "m2 kg-1", "plt.xml", 
    "slamin", "SLAMIN",	"cm2 g-1", "m2 kg-1", "plt.xml", 
    "stamflax",	"cum_thermal_growth", NA, NA, "plt.xml", 
    "stlevamf",	"cum_thermal_juvenile", NA, NA, "plt.xml", 
    "stlevdrp",	"cum_thermal_filling", NA, NA, "plt.xml", 
    "stpltger",	"cum_thermal_germin", NA, NA, "plt.xml", 
    "stressdev", "phasic_delay_max", NA, NA, "plt.xml", 
    "swfacmin", "swfacmin", NA, NA, "plt.xml", 
    "tcmax", "tcmax_growth", NA, NA, "plt.xml", 
    "tcmin", "tcmin_growth", NA, NA, "plt.xml", 
    "tcxstop", "tcmax_foliar_growth", NA, NA, "plt.xml", 
    "tdmax", "tdmax", NA, NA, "plt.xml", 
    "tdmin", "tdmin", NA, NA, "plt.xml", 
    "temax", "temax", NA, NA, "plt.xml", 
    "temin", "temin", NA, NA, "plt.xml", 
    "teopt", "teopt", NA, NA, "plt.xml", 
    "teoptbis", "teoptbis", NA, NA, "plt.xml", 
    "tfroid",	"vernalization_TOpt", NA, NA, "plt.xml", 
    "tgmin", "emergence_Tmin", NA, NA, "plt.xml", 
    "tigefeuil", "stem2leaf", NA, NA, "plt.xml", 
    "tmin_mineralisation", "tmin_mineralisation", NA, NA, "param_gen.xml", 
    "TREFh", "T_r_HOMdecomp", NA, NA, "param_gen.xml", 
    "TREFr", "T_r_ORdecomp", NA, NA, "param_gen.xml", 
    "udlaimax", "udlaimax", NA, NA, "plt.xml", 
    "Vabs2", "Nupt_fertloss_halve", NA, NA, "param_gen.xml", 
    "vlaimax", "vlaimax", NA, NA, "plt.xml", 
    "Wh", "Wh", NA, NA, "param_gen.xml", 
    "GMIN1", "GMIN1", NA, NA, "param_gen.xml", 
    "GMIN2", "GMIN2", NA, NA, "param_gen.xml", 
    "GMIN3", "GMIN3", NA, NA, "param_gen.xml", 
    "GMIN4", "GMIN4", NA, NA, "param_gen.xml", 
    "GMIN5", "GMIN5", NA, NA, "param_gen.xml", 
    "GMIN6", "GMIN6", NA, NA, "param_gen.xml", 
    "GMIN7", "GMIN7", NA, NA, "param_gen.xml", 
    "Xorgmax", "maxNimm_mineralfert", NA, NA, "param_gen.xml", 
    "y0msrac", "rootmin_harvest", NA, NA, "param_gen.xml", 
    "yres", "microbialbiomass_C_yield", NA, NA, "param_gen.xml", 
    # Missing pecan parameters without corresponding STICS parameters
  )
  
  trait.values <- lapply(trait.values, function(x){
    names(x) <- vartable$sticsname[match(names(x), vartable$pecanname)]
    return(x)
  })
  
  # TODO : unit conversions?
  toconvert <- vartable$sticsname[!is.na(vartable$sticsunits)]
  trait.values <- lapply(trait.values, function(x){
    canconvert <- toconvert[toconvert %in% names(x)]      
    if(length(canconvert) != 0){
      for(noc in seq_along(canconvert)){
        if(vartable$sticsunits[vartable$sticsname == canconvert[noc]] == "round"){
          x[,names(x) == canconvert[noc]] <- round(x[,names(x) == canconvert[noc]])
        }else{
          x[,names(x) == canconvert[noc]] <- PEcAn.utils::ud_convert(x[,names(x) == canconvert[noc]], 
                                                                     vartable$pecanunits[vartable$sticsname == canconvert[noc]], 
                                                                     vartable$sticsunits[vartable$sticsname == canconvert[noc]])
        }
        
      }
    }
    return(x)
  })
  
  return(trait.values)
} 
