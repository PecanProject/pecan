#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Writes a BASGRA config file.
##'
##' Requires a pft xml object, a list of trait values for a single model run,
##' and the name of the file to create
##'
##' @name write.config.BASGRA
##' @title Write BASGRA configuration files
##' @param defaults list of defaults to process
##' @param trait.values vector of samples for a given trait
##' @param settings list of settings from pecan settings file
##' @param run.id id of run
##' @param IC initial conditions list
##' @return configuration file for BASGRA for given run
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
write.config.BASGRA <- function(defaults, trait.values, settings, run.id, IC = NULL) {

  # find out where to write run/ouput
  rundir <- file.path(settings$host$rundir, run.id)
  outdir <- file.path(settings$host$outdir, run.id)
  
  # load default(!) BASGRA params
  run_params <- PEcAn.utils::load_local(system.file("BASGRA_params.Rdata", package = "PEcAn.BASGRA"))$default_params
  
  run_params[which(names(run_params) == "LAT")] <- as.numeric(settings$run$site$lat)
  
  #### write run-specific PFT parameters here #### Get parameters being handled by PEcAn
  for (pft in seq_along(trait.values)) {
    
    pft.traits <- unlist(trait.values[[pft]])
    pft.names <- names(pft.traits)
    
    # Replace matches
    for (mi in seq_along(pft.traits)) {
      ind <- which(names(run_params) == pft.names[mi])
      run_params[ind] <- pft.traits[mi]
    }
    
    # N-C ratio of roots (g N g-1 C)
    if ("c2n_fineroot" %in% pft.names) {
      run_params[which(names(run_params) == "NCR")] <- 1/pft.traits[which(pft.names == "c2n_fineroot")]
    }
    
    # PAR extinction coefficient (m2 m-2)
    if ("extinction_coefficient" %in% pft.names) {
      run_params[which(names(run_params) == "K")] <- pft.traits[which(pft.names == "extinction_coefficient")]
    }
    
    # Transpiration coefficient (mm d-1)
    if ("transpiration_coefficient" %in% pft.names) {
      run_params[which(names(run_params) == "TRANCO")] <- pft.traits[which(pft.names == "transpiration_coefficient")]
    }
    
    if ("phyllochron" %in% pft.names) {
      run_params[which(names(run_params) == "PHY")] <- pft.traits[which(pft.names == "phyllochron")]
    }
    
    if ("leaf_width" %in% pft.names) {
      # Leaf width on non-elongating tillers (m)
      run_params[which(names(run_params) == "LFWIDV")] <- udunits2::ud.convert(pft.traits[which(pft.names == "leaf_width")], "mm", "m")
    }
    
    if ("generative_leaf_width" %in% pft.names) {
      # Leaf width on elongating tillers (m)
      run_params[which(names(run_params) == "LFWIDG")] <- udunits2::ud.convert(pft.traits[which(pft.names == "generative_leaf_width")], "mm", "m")
    }
    
    # Initial and maximum value rooting depth (m)
    if ("rooting_depth" %in% pft.names) {
      run_params[which(names(run_params) == "ROOTDM")] <- pft.traits[which(pft.names == "rooting_depth")]
    }
    
    # Maximum root depth growth rate (m day-1)
    if ("root_growth_rate" %in% pft.names) {
      run_params[which(names(run_params) == "RRDMAX")] <- pft.traits[which(pft.names == "root_growth_rate")]
    }
    
    # Rubisco content of upper leaves (g m-2 leaf)
    if ("rubisco_content" %in% pft.names) {
      run_params[which(names(run_params) == "RUBISC")] <- pft.traits[which(pft.names == "rubisco_content")]
    }
    
    # Area of a leaf relative to a rectangle of same length and width (-)
    if ("shape" %in% pft.names) {
      run_params[which(names(run_params) == "SHAPE")] <- pft.traits[which(pft.names == "shape")]
    }
    
    # Sink strength of small elongating tillers (g C tiller-1 d-1)
    if ("elongating_tiller_sink_strength" %in% pft.names) {
      run_params[which(names(run_params) == "SIMAX1T")] <- pft.traits[which(pft.names == "elongating_tiller_sink_strength")]
    }
    
    # Minimum value of effective temperature for leaf elongation (deg C)
    if ("gdd_tbase" %in% pft.names) {
      run_params[which(names(run_params) == "TBASE")] <- pft.traits[which(pft.names == "gdd_tbase")]
    }
    
    # Time constant of mobilisation of reserves (day)
    if ("tc_res" %in% pft.names) {
      run_params[which(names(run_params) == "TCRES")] <- pft.traits[which(pft.names == "tc_res")]
    }
    
    # Optimum temperature for vegetative tillers becoming generative (deg C)
    if ("TOpt_ge" %in% pft.names) {
      run_params[which(names(run_params) == "TOPTGE")] <- pft.traits[which(pft.names == "TOpt_ge")]
    }
    
    # Growth yield per unit expended carbohydrate (g C g-1 C)
    if ("growthYield" %in% pft.names) {
      run_params[which(names(run_params) == "YG")] <- pft.traits[which(pft.names == "growthYield")]
    }
    
    # Maximum surface temperature at which hardening is possible (deg C)
    if ("THard_max" %in% pft.names) {
      run_params[which(names(run_params) == "THARDMX")] <- pft.traits[which(pft.names == "THard_max")]
    }
    
    # Minimum relative death rate of foliage (day-1)
    if ("min_foliage_mort_rate" %in% pft.names) {
      run_params[which(names(run_params) == "RDRTMIN")] <- pft.traits[which(pft.names == "min_foliage_mort_rate")]
    }
    
    # Maximum N-C ratio of shoot (g N g-1 C)
    if ("n2c_shoot_max" %in% pft.names) {
      run_params[which(names(run_params) == "NCSHMAX")] <- pft.traits[which(pft.names == "n2c_shoot_max")]
    }
    
    # Maximum refreezing rate per degree below temperature which snow melts
    if ("max_refreezing_rate" %in% pft.names) {
      run_params[which(names(run_params) == "SWrf")] <- pft.traits[which(pft.names == "max_refreezing_rate")]
    }
    
    # Vernalisation threshold (deg C)
    if ("vernalization_threshold" %in% pft.names) {
      run_params[which(names(run_params) == "TVERN")] <- pft.traits[which(pft.names == "vernalization_threshold")]
    }
    
    
    ##### Soil parameters
    
    # Fraction of decomposed litter becoming fast SOM
    if ("f_litter_SOM_fast" %in% pft.names) {
      run_params[which(names(run_params) == "FLITTSOMF")] <- pft.traits[which(pft.names == "f_litter_SOM_fast")]
    }
    
    # Fraction of decomposed fast SOM
    if ("fastOM2slowOM" %in% pft.names) {
      run_params[which(names(run_params) == "FSOMFSOMS")] <- pft.traits[which(pft.names == "fastOM2slowOM")]
    }
    
    # Residence time of slowly decomposing OM
    if ("sOM_residence_time" %in% pft.names) {
      run_params[which(names(run_params) == "TCSOMS")] <- pft.traits[which(pft.names == "sOM_residence_time")]
    }
    
    # Residence time of fast decomposing OM
    if ("fOM_residence_time" %in% pft.names) {
      run_params[which(names(run_params) == "TCSOMF")] <- round(pft.traits[which(pft.names == "fOM_residence_time")])
    }
    
    # Residence time of litter
    if ("litter_residence_time" %in% pft.names) {
      run_params[which(names(run_params) == "TCLITT")] <- pft.traits[which(pft.names == "litter_residence_time")]
    }
    
    # Temperature at which decomposition is maximal (deg C)
    if ("Tdecomp_max" %in% pft.names) {
      run_params[which(names(run_params) == "TMAXF")] <- pft.traits[which(pft.names == "Tdecomp_max")]
    }
    
    # Resilience of decomposition to temperature change (deg C)
    if ("decomp_res2Tdelta" %in% pft.names) {
      run_params[which(names(run_params) == "TSIGMAF")] <- pft.traits[which(pft.names == "decomp_res2Tdelta")]
    }
    
  } #### End parameter update

  
  #### Update initial conditions
  if (!is.null(IC)) {
    
    ic.names <- names(IC)
    
    # Initial value of leaf area index m2 m-2 - logged)
    if ("ilai" %in% ic.names) {
      run_params[which(names(run_params) == "LOG10LAII")] <- log(IC$lai)
    }
    
    
  }else if(!is.null(settings$run$inputs$poolinitcond$path)){
    
    IC.path <- settings$run$inputs$poolinitcond$path
    IC.pools <- PEcAn.data.land::prepare_pools(IC.path, constants = list(sla = SLA))
    
    if(!is.null(IC.pools)){
      IC.nc <- ncdf4::nc_open(IC.path)
      
      ## laiInit m2/m2
      lai <- try(ncdf4::ncvar_get(IC.nc, "LAI"), silent = TRUE)
      if (!is.na(lai) && is.numeric(lai)) {
        run_params[which(names(run_params) == "LOG10LAII")] <- log(lai)
      }
      
      # This is IC
      # Initial value of litter C (g C m-2)
      clitt0 <- try(ncdf4::ncvar_get(IC.nc, "litter_carbon_content"), silent = TRUE)
      if (!is.na(clitt0) && is.numeric(clitt0)) {
        run_params[which(names(run_params) == "CLITT0")] <- udunits2::ud.convert(clitt0, "kg", "g")
      }
      
      # This is IC
      # Initial value of SOM (g C m-2)
      # csom0 <- try(ncdf4::ncvar_get(IC.nc, "SOC"), silent = TRUE)
      # if (!is.na(csom0) && is.numeric(csom0)) {
      #   run_params[which(names(run_params) == "CSOM0")] <- udunits2::ud.convert(csom0, "Mg ha-1", "kg C m-2"), "kg", "g")
      # }
      
      # Initial fraction of SOC that is fast (g C g-1 C)
      if ("r_fSOC" %in% pft.names) {
        run_params[which(names(run_params) == "FCSOMF0")] <- pft.traits[which(pft.names == "r_fSOC")]
      }
      
      # This is IC, change later
      # Initial C-N ratio of litter (g C g-1 N)
      if ("c2n_litter" %in% pft.names) {
        run_params[which(names(run_params) == "CNLITT0")] <- 100*pft.traits[which(pft.names == "c2n_litter")]
      }
      
      # Initial C-N ratio of fast SOM (g C g-1 N)
      if ("c2n_fSOM" %in% pft.names) {
        run_params[which(names(run_params) == "CNSOMF0")] <- pft.traits[which(pft.names == "c2n_fSOM")]
      }
      
      # Initial C-N ratio of slow SOM (g C g-1 N)
      if ("c2n_sSOM" %in% pft.names) {
        run_params[which(names(run_params) == "CNSOMS0")] <- pft.traits[which(pft.names == "c2n_sSOM")]
      }
      
      # Initial value of soil mineral N (g N m-2)
      if ("NMIN" %in% pft.names) {
        run_params[which(names(run_params) == "NMIN0")] <- pft.traits[which(pft.names == "NMIN")]
      }
      
      # This is IC, change later
      # Initial value of soil water concentration (m3 m-3)
      if ("initial_volume_fraction_of_condensed_water_in_soil" %in% pft.names) {
        run_params[which(names(run_params) == "WCI")] <- pft.traits[which(pft.names == "initial_volume_fraction_of_condensed_water_in_soil")]
      }
      
      
      # Water concentration at saturation (m3 m-3)
      if ("volume_fraction_of_water_in_soil_at_saturation" %in% pft.names) {
        run_params[which(names(run_params) == "WCST")] <- pft.traits[which(pft.names == "volume_fraction_of_water_in_soil_at_saturation")]
      }
      
      # # Temperature that kills half the plants in a day (degrees Celcius)
      # if ("plant_min_temp" %in% pft.names) {
      #   run_params[which(names(run_params) == "LT50I")] <- pft.traits[which(pft.names == "plant_min_temp")]
      # }
      
    }
  }

  #-----------------------------------------------------------------------
  # write job.sh
  # create launch script (which will create symlink)
  if (!is.null(settings$model$jobtemplate) && file.exists(settings$model$jobtemplate)) {
    jobsh <- readLines(con = settings$model$jobtemplate, n = -1)
  } else {
    jobsh <- readLines(con = system.file("template.job", package = "PEcAn.BASGRA"), n = -1)
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
  
  jobsh <- gsub("@SITE_MET@",     settings$run$inputs$met$path,     jobsh)
  jobsh <- gsub("@SITE_HARVEST@", settings$run$inputs$harvest$path, jobsh)
  
  jobsh <- gsub("@START_DATE@", settings$run$start.date, jobsh)
  jobsh <- gsub("@END_DATE@", settings$run$end.date, jobsh)
  
  jobsh <- gsub("@OUTDIR@", outdir, jobsh)
  jobsh <- gsub("@RUNDIR@", rundir, jobsh)
  
  jobsh <- gsub(
    "@RUN_PARAMS@",
    paste0("c(", PEcAn.utils::listToArgString(run_params), ")"),
    jobsh)
  
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  

} # write.config.MODEL
