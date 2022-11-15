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
      
    # Maximum SLA of new leaves
    if ("SLAMAX" %in% pft.names) {
      run_params[which(names(run_params) == "SLAMAX")] <- PEcAn.utils::ud_convert(pft.traits[which(pft.names == "SLAMAX")], "kg-1","g-1")
    }
    
    # Number of elongating leaves per non-elongating tiller
    if ("n_el2nel" %in% pft.names) {
      run_params[which(names(run_params) == "NELLVM")] <- pft.traits[which(pft.names == "n_el2nel")]
    }
    
    # N-C ratio of roots (g N g-1 C)
    if ("c2n_fineroot" %in% pft.names) {
      run_params[which(names(run_params) == "NCR")] <- 1/pft.traits[which(pft.names == "c2n_fineroot")]
    }
    
    # Phenological stage above which elongation and appearance of leaves on elongating tillers decreases
    if ("phen_etil_decrease" %in% pft.names) {
      run_params[which(names(run_params) == "PHENCR")] <- pft.traits[which(pft.names == "phen_etil_decrease")]
    }
    
    # Relative death rate of leaves and non-elongating tillers due to shading when LAI is twice the threshold (LAICR)
    if ("relative_shading_death" %in% pft.names) {
      run_params[which(names(run_params) == "RDRSCO")] <- pft.traits[which(pft.names == "relative_shading_death")]
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
      run_params[which(names(run_params) == "LFWIDV")] <- PEcAn.utils::ud_convert(pft.traits[which(pft.names == "leaf_width")], "mm", "m")
    }
    
    if ("generative_leaf_width" %in% pft.names) {
      # Leaf width on elongating tillers (m)
      run_params[which(names(run_params) == "LFWIDG")] <- PEcAn.utils::ud_convert(pft.traits[which(pft.names == "generative_leaf_width")], "mm", "m")
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
    
    # Ratio of total to aerobic respiration
    if ("total2RA" %in% pft.names) {
      run_params[which(names(run_params) == "KRTOTAER")] <- pft.traits[which(pft.names == "total2RA")]
    }
    
    # Day length below which phenological stage is reset to zero
    if ("min_daylength_reset" %in% pft.names) {
      run_params[which(names(run_params) == "DAYLB")] <- pft.traits[which(pft.names == "min_daylength_reset")]
    }
    
    # Day length below which phenological development slows down
    if ("min_daylength_slow" %in% pft.names) {
      run_params[which(names(run_params) == "DAYLP")] <- pft.traits[which(pft.names == "min_daylength_slow")]
    }
    
    # Day length below which DAYLGE becomes less than 1
    if ("daylength_effect" %in% pft.names) {
      run_params[which(names(run_params) == "DLMXGE")] <- pft.traits[which(pft.names == "daylength_effect")]
    }
    
    # LAI above which shading induces leaf senescence
    if ("lai_senescence" %in% pft.names) {
      run_params[which(names(run_params) == "LAICR")] <- pft.traits[which(pft.names == "lai_senescence")]
    }
    
    # Decrease in tillering with leaf area index
    if ("lai_til_decrease" %in% pft.names) {
      run_params[which(names(run_params) == "LAIEFT")] <- pft.traits[which(pft.names == "lai_til_decrease")]
    }
    
    # Maximum ratio of tiller and leaf apearance at low leaf area index
    if ("lai_til2leaf_max" %in% pft.names) {
      run_params[which(names(run_params) == "LAITIL")] <- pft.traits[which(pft.names == "lai_til2leaf_max")]
    }
    
    # Proportionality of leaf senescence with temperature
    if ("leaf_senescence_temp_rate" %in% pft.names) {
      run_params[which(names(run_params) == "RDRTEM")] <- pft.traits[which(pft.names == "leaf_senescence_temp_rate")]
    }
    
    # Maximum relative rate of tillers becoming elongating tillers
    if ("til2etil_max_rate" %in% pft.names) {
      run_params[which(names(run_params) == "RGENMX")] <- pft.traits[which(pft.names == "til2etil_max_rate")]
    }
    
    # Fraction of reserves in elongating tillers that is harvested
    if ("etil_resv_harv" %in% pft.names) {
      run_params[which(names(run_params) == "HAGERE")] <- pft.traits[which(pft.names == "etil_resv_harv")]
    }
    
  } #### End parameter update
  
  
  
  #### Update initial conditions
  if (!is.null(IC)) {
    
    ic.names <- names(IC)
    
    if ("LAI"  %in% ic.names) {
      run_params[names(run_params) == "LOG10LAII"] <- IC$LAI
    }
    
    if ("fast_soil_pool_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CSOMF0"] <- PEcAn.utils::ud_convert(IC$fast_soil_pool_carbon_content, "kg", "g")
    }
    
    if ("slow_soil_pool_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CSOMS0"] <- PEcAn.utils::ud_convert(IC$slow_soil_pool_carbon_content, "kg", "g")
    }
    
    if ("CropYield"  %in% ic.names) {
       run_params[names(run_params) == "YIELDI"] <-  PEcAn.utils::ud_convert(IC$CropYield, "kg", "g")
    }
    
    if ("litter_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CLITT0"] <-  PEcAn.utils::ud_convert(IC$litter_carbon_content, "kg", "g")
    }
    
    # not as important as others but you can throw this into the SDA too, then comment out last value overwriting below
    # if ("stubble_carbon_content"  %in% ic.names) {
    #   run_params[names(run_params) == "CSTUBI"] <-  PEcAn.utils::ud_convert(IC$stubble_carbon_content, "kg", "g")
    # }
   
    if ("stem_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CSTI"] <-  PEcAn.utils::ud_convert(IC$stem_carbon_content, "kg", "g")
    }
    
    if ("root_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "LOG10CRTI"] <-  PEcAn.utils::ud_convert(IC$root_carbon_content, "kg", "g")
    }

    if ("reserve_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "LOG10CRESI"] <-  PEcAn.utils::ud_convert(IC$reserve_carbon_content, "kg", "g")
    }
    
    if ("leaf_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "LOG10CLVI"] <-  PEcAn.utils::ud_convert(IC$leaf_carbon_content, "kg", "g")
    }
    
    if ("dead_leaf_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CLVDI"] <-  PEcAn.utils::ud_convert(IC$dead_leaf_carbon_content, "kg", "g")
    }

    if ("nonelongating_generative_tiller"  %in% ic.names) {
      run_params[names(run_params) == "TILG1I"] <-  IC$nonelongating_generative_tiller
    }

    if ("elongating_generative_tiller"  %in% ic.names) {
      run_params[names(run_params) == "TILG2I"] <-  IC$elongating_generative_tiller
    }

    if ("nonelongating_vegetative_tiller"  %in% ic.names) {
      run_params[names(run_params) == "TILVI"] <-  IC$nonelongating_vegetative_tiller
    }
    
    if ("tiller_density"  %in% ic.names) {
      run_params[names(run_params) == "TILTOTI"] <-  IC$tiller_density
    }

    if ("phenological_stage"  %in% ic.names) {
      run_params[names(run_params) == "PHENI"] <-  IC$phenological_stage
    }
    

  }else if(!is.null(settings$run$inputs$poolinitcond$path)){
    
    IC.path <- settings$run$inputs$poolinitcond$path
    #IC.pools <- PEcAn.data.land::prepare_pools(IC.path, constants = list(sla = SLA))
    
    #if(!is.null(IC.pools)){
    IC.nc <- ncdf4::nc_open(IC.path)
    
    ## laiInit m2/m2
    lai <- try(ncdf4::ncvar_get(IC.nc, "LAI"), silent = TRUE)
    if (!is.na(lai) && is.numeric(lai)) {
      run_params[which(names(run_params) == "LOG10LAII")] <- lai
    }
    
    # Initial value of litter C (g C m-2)
    clitt0 <- try(ncdf4::ncvar_get(IC.nc, "litter_carbon_content"), silent = TRUE)
    if (!is.na(clitt0) && is.numeric(clitt0)) {
      run_params[which(names(run_params) == "CLITT0")] <- PEcAn.utils::ud_convert(clitt0, "kg", "g")
    }
    
 
    # Initial value of slow SOM (g C m-2)
    csoms0 <- try(ncdf4::ncvar_get(IC.nc, "slow_soil_pool_carbon_content"), silent = TRUE)
    if (!is.na(csoms0) && is.numeric(csoms0)) {
      run_params[which(names(run_params) == "CSOMS0")] <- PEcAn.utils::ud_convert(csoms0, "kg", "g")
    }
    
    # Initial value of fast SOM (g C m-2)
    csomf0 <- try(ncdf4::ncvar_get(IC.nc, "fast_soil_pool_carbon_content"), silent = TRUE)
    if (!is.na(csomf0) && is.numeric(csomf0)) {
      run_params[which(names(run_params) == "CSOMF0")] <- PEcAn.utils::ud_convert(csomf0, "kg", "g")
    }
    
    # Initial value of root C (g C m-2)
    crti <- try(ncdf4::ncvar_get(IC.nc, "root_carbon_content"), silent = TRUE)
    if (!is.na(crti) && is.numeric(crti)) {
      # not log10 anymore, don't mind the name
      run_params[which(names(run_params) == "LOG10CRTI")] <- PEcAn.utils::ud_convert(crti, "kg", "g")
    }
    
    # Initial value of leaf C (g C m-2)
    clvi <- try(ncdf4::ncvar_get(IC.nc, "leaf_carbon_content"), silent = TRUE)
    if (!is.na(clvi) && is.numeric(clvi)) {
      # not log10 anymore, don't mind the name
      run_params[which(names(run_params) == "LOG10CLVI")] <- PEcAn.utils::ud_convert(clvi, "kg", "g")
    }
    
    # Initial mineral N
    nmin0 <- try(ncdf4::ncvar_get(IC.nc, "soil_nitrogen_content"), silent = TRUE)
    if (!is.na(nmin0) && is.numeric(nmin0)) {
      run_params[which(names(run_params) == "NMIN0")] <- PEcAn.utils::ud_convert(nmin0, "kg", "g")
    }
    
    # Rooting depth (m)
    rootd <- try(ncdf4::ncvar_get(IC.nc, "rooting_depth"), silent = TRUE)
    if (!is.na(rootd) && is.numeric(rootd)) {
      run_params[which(names(run_params) == "ROOTDM")] <- rootd
    }
    
    # WCI
    wci <- try(ncdf4::ncvar_get(IC.nc, "SoilMoistFrac"), silent = TRUE)
    if (!is.na(wci) && is.numeric(wci)) {
      run_params[which(names(run_params) == "WCI")] <- wci
    }
    
    # Tiller density (m-2)
    tiltoti <- try(ncdf4::ncvar_get(IC.nc, "tiller_density"), silent = TRUE)
    if (!is.na(tiltoti) && is.numeric(tiltoti)) {
      run_params[which(names(run_params) == "TILTOTI")] <- tiltoti
    }
    
    # Phenological stage
    pheni <- try(ncdf4::ncvar_get(IC.nc, "phenological_stage"), silent = TRUE)
    if (!is.na(pheni) && is.numeric(pheni)) {
      run_params[which(names(run_params) == "PHENI")] <- pheni
    }
    
    # Initial C in reserves (g C m-2)
    cresi <- try(ncdf4::ncvar_get(IC.nc, "reserve_carbon_content"), silent = TRUE)
    if (!is.na(cresi) && is.numeric(cresi)) {
      # not log10 anymore, don't mind the name
      run_params[which(names(run_params) == "LOG10CRESI")] <- PEcAn.utils::ud_convert(cresi, "kg", "g")
    }
    
    # N-C ratio of roots
    n2c <- try(ncdf4::ncvar_get(IC.nc, "n2c_roots"), silent = TRUE)
    if (!is.na(n2c) && is.numeric(n2c)) {
      run_params[which(names(run_params) == "NCR")] <- n2c
    }
    
    # Initial C-N ratio of fast SOM
    c2n <- try(ncdf4::ncvar_get(IC.nc, "c2n_fast_pool"), silent = TRUE)
    if (!is.na(c2n) && is.numeric(c2n)) {
      run_params[which(names(run_params) == "CNSOMF0")] <- c2n
    }
    
    # Water concentration at saturation (m3 m-3)
    wcst <- try(ncdf4::ncvar_get(IC.nc, "water_concentration_at_saturation"), silent = TRUE)
    if (!is.na(wcst) && is.numeric(wcst)) {
      run_params[which(names(run_params) == "WCST")] <- wcst
    }
    
    # Water concentration at field capacity (m3 m-3)
    wcfc <- try(ncdf4::ncvar_get(IC.nc, "water_concentration_at_field_capacity"), silent = TRUE)
    if (!is.na(wcfc) && is.numeric(wcfc)) {
      # WCFC  = FWCFC  * WCST
      run_params[which(names(run_params) == "FWCFC")] <- wcfc / wcst 
    }
    
    # Water concentration at wilting point (m3 m-3)
    wcwp <- try(ncdf4::ncvar_get(IC.nc, "water_concentration_at_wilting_point"), silent = TRUE)
    if (!is.na(wcwp) && is.numeric(wcwp)) {
      # WCWP  = FWCWP  * WCST
      run_params[which(names(run_params) == "FWCWP")] <- wcwp / wcst 
    }

  }
  
  # THESE "PARAMETERS" (IN FACT, INITIAL CONDITIONS) WERE NOT PART OF THE ORIGINAL VECTOR
  # THESE DERIVATIONS WERE PART OF THE BASGRA CODE, NOW TAKEN OUT HERE BECAUSE OF SDA
  # BUT WHEN NOT DOING SDA WE STILL NEED TO PASS THEM
  
  # NRT        = NCR * CRTI
  run_params[which(names(run_params) == "NRTI")] <- run_params[names(run_params) == "LOG10CRTI"]*
    run_params[names(run_params) == "NCR"]
  
  # NCSHI    = NCSHMAX * (1-EXP(-K*LAII)) / (K*LAII)
  # NSH      = NCSHI * (CLVI+CSTI)
  lai_tmp <- run_params[names(run_params) == "LOG10LAII"]
  ncshi <- run_params[names(run_params) == "NCSHMAX"] * 
    (1-exp(-run_params[names(run_params) == "K"]*lai_tmp)) / (run_params[names(run_params) == "K"]*lai_tmp)
  run_params[which(names(run_params) == "NSHI")] <- ncshi * 
    ((run_params[names(run_params) == "LOG10CLVI"]) + run_params[names(run_params) == "CSTI"])
  
  #  WAL        = 1000. * ROOTDM * WCI
  run_params[names(run_params) == "WALI"]  <- 1000. * run_params[names(run_params) == "ROOTDM"] * run_params[names(run_params) == "WCI"]
  
  # O2         = FGAS * ROOTDM * FO2MX * 1000./22.4
  run_params[names(run_params) == "O2I"]  <- run_params[names(run_params) == "FGAS"] * 
    run_params[names(run_params) == "ROOTDM"] * run_params[names(run_params) == "FO2MX"] * 1000./22.4
  
  #NLITT      = CLITT0 / CNLITT0
  run_params[names(run_params) == "NLITT0"]  <- run_params[names(run_params) == "CLITT0"] / run_params[names(run_params) == "CNLITT0"]
  
  #NSOMF      = (CSOM0 *    FCSOMF0)  / CNSOMF0
  run_params[names(run_params) == "NSOMF0"]  <- run_params[names(run_params) == "CSOMF0"] / run_params[names(run_params) == "CNSOMF0"]
  run_params[names(run_params) == "NSOMS0"]  <- run_params[names(run_params) == "CSOMS0"] / run_params[names(run_params) == "CNSOMS0"]
  
  
  
  ##################################################################
  ######################### PREVIOUS STATE #########################
  ##################################################################
  
  # overwrite initial values with previous time steps
  # as model2netcdf is developed, some or all of these can be dropped?
  last_vals <- c()
  last_states_file <- file.path(outdir, "last_vals_basgra.Rdata")
  
  if(file.exists(last_states_file)){
    
    # TODO: certain variables should be thrown into the state matrix in SDA together
    # but in case someone forgot to do so, make sure those missing values are passed from where we left off here
    
    load(last_states_file)
    
    # SDA handles this now
    # PHENI	   = pa(6) 
    run_params[names(run_params) == "PHENI"] <- last_vals[names(last_vals) == "PHEN"]
    
    # LT50I      = pa(9)
    run_params[names(run_params) == "LT50I"] <- last_vals[names(last_vals) == "LT50"]
    
    run_params[names(run_params) == "CSTUBI"] <- last_vals[names(last_vals) == "CSTUB"] 
    
    run_params[names(run_params) == "ROOTDM"] <- last_vals[names(last_vals) == "ROOTD"]
    
    run_params[names(run_params) == "DRYSTORI"] <- last_vals[names(last_vals) == "DRYSTOR"]
    run_params[names(run_params) == "FdepthI"]  <- last_vals[names(last_vals) == "Fdepth"]
    run_params[names(run_params) == "SDEPTHI"]  <- last_vals[names(last_vals) == "Sdepth"]
    run_params[names(run_params) == "TANAERI"]  <- last_vals[names(last_vals) == "TANAER"]
    run_params[names(run_params) == "WAPLI"]    <- last_vals[names(last_vals) == "WAPL"]
    run_params[names(run_params) == "WAPSI"]    <- last_vals[names(last_vals) == "WAPS"]
    run_params[names(run_params) == "WASI"]     <- last_vals[names(last_vals) == "WAS"]
    run_params[names(run_params) == "WETSTORI"] <- last_vals[names(last_vals) == "WETSTOR"]
  
    
    run_params[names(run_params) == "FRTILGI"] <- last_vals[names(last_vals) == "FRTILG"] 
    
    #TILV       = TILTOTI * (1. - FRTILGI)
    #TILG1      = TILTOTI *       FRTILGI *    FRTILGG1I
    #TILG2      = TILTOTI *       FRTILGI * (1-FRTILGG1I)
    
    run_params[names(run_params) == "TILVI"]  <- run_params[names(run_params) == "TILTOTI"] * (1-run_params[names(run_params) == "FRTILGI"])
    gtil <- run_params[names(run_params) == "TILTOTI"] - run_params[names(run_params) == "TILVI"]
    run_params[names(run_params) == "TILG1I"] <- gtil*last_vals[names(last_vals) == "TILG1"]  / 
      (last_vals[names(last_vals) == "TILTOT"] - last_vals[names(last_vals) == "TILV"])
    run_params[names(run_params) == "TILG2I"] <- gtil*last_vals[names(last_vals) == "TILG2"]  / 
      (last_vals[names(last_vals) == "TILTOT"] - last_vals[names(last_vals) == "TILV"])
    
    run_params[names(run_params) == "DAYLI"]  <- last_vals[names(last_vals) == "DAYL"]
    
    run_params[names(run_params) == "NMIN0"] <- last_vals[names(last_vals) == "NMIN"]
    
    run_params[names(run_params) == "WALI"]        <- last_vals[names(last_vals) == "WAL"] 
    run_params[names(run_params) == "WCI"]  <- last_vals[names(last_vals) == "WAL"] / (1000 * last_vals[names(last_vals) == "ROOTD"])
    run_params[names(run_params) == "O2I"]         <- last_vals[names(last_vals) == "O2"]
    
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
  jobsh <- gsub("@SITE_FERTILIZE@", settings$run$inputs$fertilize$path, jobsh)
  if(!is.null(settings$run$inputs$co2_file$path)){
    jobsh <- gsub("@SITE_CO2FILE@", settings$run$inputs$co2_file$path, jobsh)
  }else{
    jobsh <- gsub("@SITE_CO2FILE@", 'NULL', jobsh)
  }
  
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
  
  
} # write.config.BASGRA
