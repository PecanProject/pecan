##' Writes a LPJ-GUESS config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.LPJGUESS
##' @title Write LPJ-GUESS configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @param restart Logical, whether to restart the simulation.
##' @return configuration file for LPJ-GUESS for given run
##' @export
##' @author Istem Fer, Tony Gardella
write.config.LPJGUESS <- function(defaults, trait.values, settings, run.id, restart = NULL) {
  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  if (!file.exists(rundir)) {
    dir.create(rundir)
  }
  outdir <- file.path(settings$host$outdir, run.id)
  if (!file.exists(outdir)) {
    dir.create(outdir)
  }
  
  #-----------------------------------------------------------------------
  # write LPJ-GUESS specific instruction file
  # settings$model$restart        <- as.logical(!is.null(restart_year) && restart_year > 0)
  # settings$model$restart_year   <- restart_year
  settings <- write.insfile.LPJGUESS(settings, trait.values, rundir, outdir, run.id, restart)
  
  #-----------------------------------------------------------------------
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.LPJGUESS"), n = -1)
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
  
  jobsh <- gsub("@BINARY@", settings$model$binary, jobsh)
  jobsh <- gsub("@INSFILE@", settings$model$insfile, jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
} # write.config.LPJGUESS

# ==================================================================================================#
#' @name write.insfile.LPJGUESS
#' @title Write LPJ-GUESS instruction script
#' @export
#' @param settings PEcAn settings list
#' @param trait.values trait.values
#' @param rundir rundir
#' @param outdir outdir
#' @param run.id PEcAn run ID
#' @param restart A list object passed from the SDA workflow containing run-specific restart information, or NULL for initial runs.
#' @return settings Updated list
#' @author Istem Fer, Yinghao Sun
write.insfile.LPJGUESS <- function(settings, trait.values, rundir, outdir, run.id, restart = NULL) {
  
  guessins  <- readLines(con = system.file("template.ins", package = "PEcAn.LPJGUESS"), n = -1)
  paramsins <- readLines(con = system.file("pecan.ins", package = "PEcAn.LPJGUESS"), n = -1)
  pftindx   <- 204:274 # should grab automatically
  pftblock  <- paramsins[pftindx] # lines with pft params
  
  # # fill save state flags
  # if(is.null(restart)){
  #   ## Past code for resatrt/save
  #   # year_string <- substring(basename(settings$run$inputs$met[[1]]), 
  #   #                          nchar(basename(settings$run$inputs$met[[1]]))-15,
  #   #                          nchar(basename(settings$run$inputs$met[[1]]))-7)
  #   # #spinup plus simulation years, extract from defult, or pass it here if you'll be varying this in the future
  #   # spinup_years <- as.numeric(gsub("[^[:digit:].]", "", paramsins[grepl("nyear_spinup", paramsins, fixed = TRUE)]))
  #   # state_year   <- spinup_years + diff(as.numeric(strsplit(year_string, split = ".", fixed = TRUE)[[1]])) + 1
  #   
  #   # Get start and end years directly from PEcAn's settings
  #   start_year_run <- lubridate::year(settings$run$start.date)
  #   end_year_run <- lubridate::year(settings$run$end.date)
  #   #spinup plus simulation years, extract from defult, or pass it here if you'll be varying this in the future
  #   spinup_years <- as.numeric(gsub("[^[:digit:].]", "", paramsins[grepl("nyear_spinup", paramsins, fixed = TRUE)]))
  #   num_actual_simulation_years <- end_year_run - start_year_run + 1
  #   # state_year <- spinup_years + num_actual_simulation_years
  #   
  #   # For the initial run, the restart parameter should be set to 0
  #   paramsins  <- gsub("@RESTART_OPTION@", 0, paramsins) 
  #   
  #   
  #   
  # }else{
  #   # read previous year's params.ins and add 1 or?
  # }
  # 
  # if(!is.null(settings$model$save_state)){
  #   save_state <- as.logical(settings$model$save_state)
  #   if(save_state){
  #     paramsins  <- gsub("@SAVE_STATE_OPTION@", 1, paramsins)
  #     paramsins  <- gsub("@STATE_PATH@", paste0("state_path '", outdir, "'"), paramsins)
  #     paramsins  <- gsub("@STATE_YEAR@", paste0("state_year ", state_year), paramsins)
  #   }else{
  #     paramsins  <- gsub("@RESTART_OPTION@", 0, paramsins)
  #     paramsins  <- gsub("@STATE_PATH@", "!state_path", paramsins)
  #     paramsins  <- gsub("@STATE_PATH@", "!state_year", paramsins)
  #   }
  # }else{
  #   # wouldn't hurt to save state by default?
  #   paramsins  <- gsub("@SAVE_STATE_OPTION@", 1, paramsins)
  #   paramsins  <- gsub("@STATE_PATH@", paste0("state_path '", outdir, "'"), paramsins)
  #   paramsins  <- gsub("@STATE_YEAR@", paste0("state_year ", state_year), paramsins)
  # }
  
  if (!is.null(restart)) {
    # This is a restart run
    # The model should be told to restart from a previous state and save its new state.
    restart_flag      <- 1
    restart_year_val  <- lubridate::year(restart$start.time)-1
    save_state_flag   <- 1
    save_year_val   <- lubridate::year(restart$stop.time)
  } else {
    # This is an initial run (t=1).
    restart_flag      <- 0
    restart_year_val  <- 0 # Use LPJ-GUESS default for no restart year
    save_state_flag   <- 1
    save_year_val   <- lubridate::year(settings$run$start.date)
  }

  
  # Define the state path with the %Y placeholder for the model to use.
  state_path_val <- file.path(outdir, "state", "%Y")
  
  # Substitute placeholders in the `params.ins` template.
  paramsins  <- gsub("@RESTART@", paste("restart", restart_flag), paramsins)
  paramsins  <- gsub("@SAVE_STATE@", paste("save_state", save_state_flag), paramsins)
  paramsins  <- gsub("@RESTART_YEAR@", paste("restart_year", restart_year_val), paramsins)
  paramsins  <- gsub("@SAVE_YEAR@", paste("save_year", save_year_val), paramsins)
  paramsins  <- gsub("@STATE_PATH@", paste0("state_path '", state_path_val, "'"), paramsins)
  
  # cp the grid indices file
  gridcf.file <- settings$run$inputs$gridcf$path
  if (is.null(gridcf.file) || !file.exists(gridcf.file)) {
    PEcAn.logger::logger.severe(
      "Gridcf file not found! Please specify its path in pecan.xml under run -> inputs -> gridcf -> path."
    )
  }
  # gridind   <- readLines(con = system.file("gridlist.txt", package = "PEcAn.LPJGUESS"), n = -1)
  # writeLines(gridind, grid.file)
  guessins  <- gsub("@GRID_CF_FILE@", gridcf.file, guessins)
  
  gridlist.file <- settings$run$inputs$gridlist$path
  if (is.null(gridlist.file) || !file.exists(gridlist.file)) {
    PEcAn.logger::logger.severe(
      "Gridlist file not found! Please specify its path in pecan.xml under run -> inputs -> gridlist -> path."
    )
  }
  guessins  <- gsub("@GRID_LIST_FILE@", gridlist.file, guessins)
  
  
  pft_names <- sapply(settings$pfts, `[[`,"name")
  lpjguess_param_data <- PEcAn.utils::load_local(system.file("lpjguess_params.Rdata",package = "PEcAn.LPJGUESS"))
  lpjguess_param_list <- lpjguess_param_data$lpjguess_param_list
  # name and unit conversion
  trait.values <- pecan2lpjguess(trait.values)
  
  # these are strings, should they be passed via xml?
  # e.g. defaults lifeform=tree phenology=evergreen leafphysiognomy=broadleaf landcover=natural pathway=c3
  noprior_params <- c("lifeform", "landcover", "pathway", "phenology", "leafphysiognomy")
  
  write2pftblock <-  vector("list", length(settings$pfts))
  # write params with values from trait.values
  for (i in seq_along(settings$pfts)) {
    
      write2pftblock[[i]] <- pftblock
      write2pftblock[[i]] <- gsub(paste0("@pft@"), pft_names[i], write2pftblock[[i]])
      
      warning_list <- list()
      
      # pass param values
      # IMPORTANT : Ideally all params should have priors on them! Currently the defaults are only for a tropical broadleaved evergreen pft
      for(t in seq_along(lpjguess_param_list)){
        trait_name <- names(lpjguess_param_list)[t]
        if(trait_name != "pft" & !(trait_name %in% noprior_params)){
          if(trait_name %in% names(trait.values[[i]])){ # pass sample
            
            pecan_sample <- trait.values[[i]][[trait_name]]
            
            if(trait_name == "rootdist"){  # convert from ratio to fractions
              lower_layer_fraction = 1/(pecan_sample+1)
              upper_layer_fraction = 1 - lower_layer_fraction
              pecan_sample <- paste(upper_layer_fraction, lower_layer_fraction)
            }
            
            
            # if(trait_name == "wooddens"){  # convert from relative density to sapwood and heartwood density (kgC/m3)
            #   pecan_sample <- pecan_sample*997 # density of water
            # }
            
            write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), pecan_sample, write2pftblock[[i]])
          }else{ # use default
            write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), lpjguess_param_list[[trait_name]], write2pftblock[[i]])
            warning_list[[trait_name]] <- trait_name
          }
        }  
      }
      
      # handle the no prior params
      for(t in seq_along(noprior_params)){
        trait_name <- noprior_params[t]
        if(!is.null(settings$pfts[[i]][[trait_name]])){ # specified in xml
          write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), paste0("'", settings$pfts[[i]][[trait_name]], "'"), write2pftblock[[i]])
        }else{ #pass the default, add to warning
          write2pftblock[[i]] <- gsub(paste0("@", trait_name, "@"), paste0("'", lpjguess_param_list[[trait_name]], "'"), write2pftblock[[i]])
          warning_list[[trait_name]] <- trait_name
        }
      }
      
      PEcAn.logger::logger.warn("***You have not specified the following parameters for your PFT,", pft_names[i],"- Be aware that the defaults may not work well for you.***", unlist(warning_list))
  } #end of pft-loop
  
  # erase the placeholder, write back the pft blocks
  paramsins <- paramsins[-pftindx] 
  paramsins <- c(paramsins, unlist(write2pftblock))
  
  # write clim file names (cf input)
  tmp.file <- settings$run$inputs$met$path
  pre.file <- gsub(".tmp.nc", ".pre.nc", tmp.file)
  cld.file <- gsub(".tmp.nc", ".cld.nc", tmp.file)

  guessins <- gsub("@TEMP_FILE@", tmp.file, guessins)
  guessins <- gsub("@PREC_FILE@", pre.file, guessins)
  guessins <- gsub("@INSOL_FILE@", cld.file, guessins)
  
  # # when using cru input, lpjguess will not use these clim files
  # cru.file <- settings$run$inputs$met$path
  # misc.file <- sub("\\.bin$", "misc.bin", cru.file)
  # guessins <- gsub("@MET_AND_SOIL_FILE@", cru.file, guessins)
  # guessins <- gsub("@MISC_FILE@", misc.file, guessins)
  
  # create and write CO2 file
  start.year <- lubridate::year(settings$run$start.date)
  end.year <- lubridate::year(settings$run$end.date)
  n.year <- length(start.year:end.year)
  # co2.file <- file.path(settings$rundir,
  #                       paste0("co2.", sprintf("%04d", start.year), ".", end.year, ".txt"))
  co2.file <- file.path(settings$rundir,
                        paste0("co2.", sprintf("%04d", start.year), ".2020", ".txt"))
  # for pre-industrial values just use 280 ppm
  if (end.year < 1850) {
    CO2 <- data.frame(start.year:end.year, rep(280, n.year))
  } else if (end.year < 2021) {
    co2_data <- new.env()
    utils::data(co2.1850.2020, package = "PEcAn.LPJGUESS", envir = co2_data)
    co2.1850.2020 <- co2_data$co2.1850.2020
    if (start.year < 1850) {
      CO2_preind <- data.frame(year = start.year:1849, ppm = rep(280, length(start.year:1849)))
      # CO2_postind <- co2.1850.2020[1:which(co2.1850.2020[, 1] == end.year), ]
      CO2_postind <- co2.1850.2020
      CO2 <- rbind(CO2_preind, CO2_postind)
    } else {
      # CO2 <- co2.1850.2020[1:which(co2.1850.2020[, 1] == end.year), ]
      CO2 <- co2.1850.2020
    }
  } else {
    PEcAn.logger::logger.severe("End year should be < 2021 for CO2")
  }
  utils::write.table(CO2, file = co2.file, row.names = FALSE, col.names = FALSE, sep = "\t", eol = "\n")
  guessins <- gsub("@CO2_FILE@", co2.file, guessins)

  
  # # create and write CO2 file
  # co2.file <- settings$run$inputs$co2$path
  # if (is.null(co2.file) || !file.exists(co2.file)) {
  #   PEcAn.logger::logger.severe(
  #     "CO2 file not found! Please specify its path in pecan.xml under run -> inputs -> co2 -> path."
  #   )
  # }
  # guessins  <- gsub("@CO2_FILE@", co2.file, guessins)
  
  # write soil file path
  # when using cru input, it's also climate file
  soil.file <- settings$run$inputs$soil$path
  # misc.file <- sub("\\.bin$", "misc.bin", soil.file)
  guessins <- gsub("@SOIL_FILE@", soil.file, guessins)
  # guessins <- gsub("@MISC_FILE@", misc.file, guessins)
  
  landuse.file <- settings$run$inputs$landuse$path
  guessins <- gsub("@LU_FILE@", landuse.file, guessins)
  landusechange.file <- settings$run$inputs$landusechange$path
  guessins <- gsub("@LUC_FILE@", landusechange.file, guessins)
  
  settings$model$insfile <- file.path(settings$rundir, run.id, "guess.ins")
  
  # version check
  if(!is.null(settings$model$revision)){
    if(settings$model$revision == "PalEON"){
      rm_inds <- which(grepl("@@@@@ Remove in PalEON version @@@@@", paramsins))
      paramsins <- paramsins[-(rm_inds[1]:rm_inds[2])]
    }
  }
  
  writeLines(paramsins, con = file.path(settings$rundir, run.id, "params.ins"))
  writeLines(guessins, con = file.path(settings$rundir, run.id, "guess.ins"))
  
  return(settings)
} # write.insfile.LPJGUESS


# ==================================================================================================#
#' Function to translate pecan param names and units to lpjguess names and units
#' @export
#' @param trait.values trait.values, list
#' @return translated list
#' @author Istem Fer
pecan2lpjguess <- function(trait.values){
  
  # leafphysiognomy and phenology are special cases
  # these are binary flags
  ph_params <- c("evergreen", "cold_deciduous", "broad_leaved")
  if(any(ph_params %in% unlist(lapply(trait.values, names)))){
    for(i in seq_along(trait.values)){
      if("evergreen" %in% names(trait.values[[i]])){
        # "any" might be unexpected here, grasses can be "any" phenology
        trait.values[[i]][names(trait.values[[i]]) == "evergreen"] <- ifelse(trait.values[[i]][names(trait.values[[i]]) == "evergreen"], "evergreen", "any")
        names(trait.values[[i]])[names(trait.values[[i]]) == "evergreen"] <- "phenology"
      }
      if("cold_deciduous" %in% names(trait.values[[i]])){
        trait.values[[i]][names(trait.values[[i]]) == "cold_deciduous"] <- ifelse(trait.values[[i]][names(trait.values[[i]]) == "cold_deciduous"], "summergreen", "raingreen")
        names(trait.values[[i]])[names(trait.values[[i]]) == "cold_deciduous"] <- "phenology"
      }
      if("broad_leaved" %in% names(trait.values[[i]])){
        trait.values[[i]][names(trait.values[[i]]) == "broad_leaved"] <- ifelse(trait.values[[i]][names(trait.values[[i]]) == "broad_leaved"], "broadleaf", "needleleaf")
        names(trait.values[[i]])[names(trait.values[[i]]) == "broad_leaved"] <- "leafphysiognomy"
      }
    }
  }
  
  
  # TODO :match all lpjguess and pecan names
  vartable <- tibble::tribble(
    ~pecanname, ~lpjguessname, ~pecanunits, ~lpjguessunits, 
    "root_turnover_rate", "turnover_root", NA, NA,   
    "sapwood_turnover_rate", "turnover_sap", NA, NA,
    "leaf_turnover_rate", "turnover_leaf", NA, NA,
    "SLA", "sla", NA, NA,
    "ci2ca", "lambda_max", NA, NA,         
    "Emax", "emax", NA, NA,
    "reprfrac", "reprfrac", NA, NA,
    "water_stress_threshold", "wscal_min", NA, NA,
    "drought_tolerance", "drought_tolerance", NA, NA,
    "turnover_harv_prod", "turnover_harv_prod", NA, NA, 
    "crownarea_max", "crownarea_max", NA, NA,
    "ltor_max", "ltor_max", NA, NA,                
    "root_dist_ratio", "rootdist", NA, NA,
    "k_allom2", "k_allom2", NA, NA,           
    "k_allom3", "k_allom3", NA, NA,          
    "k_rp", "k_rp", NA, NA,               
    "wood_density", "wooddens", NA, NA,           
    "c2n_fineroot", "cton_root", NA, NA,
    "c2n_sapwood", "cton_sap", NA, NA,           
    "nup2root_max", "nuptoroot", NA, NA,
    "km_volume", "km_volume", NA, NA,            
    "growth_resp_factor", "respcoeff", NA, NA,
    "kest_repr", "kest_repr", NA, NA,
    "kest_bg", "kest_bg", NA, NA,           
    "kest_pres", "kest_pres", NA, NA,
    "k_chilla", "k_chilla", NA, NA,
    "k_chillb", "k_chillb", NA, NA,
    "k_chillk", "k_chillk", NA, NA,
    "litterme", "litterme", NA, NA,
    "harv_eff", "harv_eff", NA, NA,
    "res_outtake", "res_outtake", NA, NA,
    "harvest_slow_frac", "harvest_slow_frac", NA, NA,
    "fnstorage", "fnstorage", NA, NA,      
    "GDD", "phengdd5ramp", NA, NA,
    "est_max", "est_max", NA, NA,
    "parff_min", "parff_min", NA, NA,
    "alphar", "alphar", NA, NA,
    "greff_min", "greff_min", NA, NA, 
    "k_allom1", "k_allom1", NA, NA,
    "sapwood_ratio", "k_latosa", NA, NA,        
    "gcmin", "gmin", NA, NA,               
    "intc", "intc", NA, NA,
    "ga", "ga", NA, NA,
    "tcmin_surv", "tcmin_surv", NA, NA,
    "tcmin_est", "tcmin_est", NA, NA,
    "tcmax_est", "tcmax_est", NA, NA,
    "twmin_est", "twmin_est", NA, NA,
    "gdd5min_est", "gdd5min_est", NA, NA,
    "pstemp_min", "pstemp_min", NA, NA,
    "pstemp_low", "pstemp_low", NA, NA,
    "pstemp_high", "pstemp_high", NA, NA,        
    "pstemp_max", "pstemp_max", NA, NA,
    "leaf_longevity", "leaflong", NA, NA,
    "longevity", "longevity", NA, NA,
    "fireresist", "fireresist", NA, NA,
    "eps_iso", "eps_iso", NA, NA,
    "seas_iso", "seas_iso", NA, NA,
    "eps_mon", "eps_mon", NA, NA,
    "storfrac_mon", "storfrac_mon", NA, NA,
    "minmoist_est", "minmoist_est", NA, NA,
    "phenology", "phenology", NA, NA, # these two lines are hacks
    "leafphysiognomy", "leafphysiognomy", NA, NA
    )
  
  trait.values <- lapply(trait.values, function(x){
    names(x) <- vartable$lpjguessname[match(names(x), vartable$pecanname)]
    return(x)
  })
  
  # TODO : unit conversions?
  toconvert <- vartable$lpjguessname[!is.na(vartable$lpjguessunits)]
  trait.values <- lapply(trait.values, function(x){
    canconvert <- toconvert[toconvert %in% names(x)]      
    if(length(canconvert) != 0){
      for(c in seq_along(canconvert)){
        x[,names(x) == canconvert[c]] <- PEcAn.utils::ud_convert(x[,names(x) == canconvert[c]], 
                                                              vartable$pecanunits[vartable$lpjguessname == canconvert[c]], 
                                                              vartable$lpjguessunits[vartable$lpjguessname == canconvert[c]])
      }
    }
    return(x)
  })
  
  return(trait.values)
} 
