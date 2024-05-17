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
  if (!is.null(settings$run$inputs$defaults$path)) {
    df_run_params <- utils::read.csv(settings$run$inputs$defaults$path)
  } else {
    df_run_params <- utils::read.csv(system.file("BASGRA_params.csv", package = "PEcAn.BASGRA"))
  }
  run_params <- stats::setNames(df_run_params[,2], df_run_params[,1])
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
    
    if ("hardening_parameter" %in% pft.names) {
      run_params[which(names(run_params) == "Hparam")] <- pft.traits[which(pft.names == "hardening_parameter")]
    }
    
    if ("max_res_abg" %in% pft.names) {
      run_params[which(names(run_params) == "COCRESMX")] <- pft.traits[which(pft.names == "max_res_abg")]
    }
    
    if ("max_size_etil" %in% pft.names) {
      run_params[which(names(run_params) == "CSTAVM")] <- pft.traits[which(pft.names == "max_size_etil")]
    }
    
    if ("maxSLAmin" %in% pft.names) {
      run_params[which(names(run_params) == "FSLAMIN")] <- pft.traits[which(pft.names == "maxSLAmin")]
    }
    
    if ("rehardening_disappears" %in% pft.names) {
      run_params[which(names(run_params) == "reHardRedDay")] <- pft.traits[which(pft.names == "rehardening_disappears")]
    }
    
    if ("LUE_increase" %in% pft.names) {
      run_params[which(names(run_params) == "KLUETILG")] <- pft.traits[which(pft.names == "LUE_increase")]
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

    # Yasso decomposition parameters
    param_pairs <- list(
      c('som_a_decomp_rate', 'yasso_alpha_a'), c('som_w_decomp_rate', 'yasso_alpha_w'),
      c('som_e_decomp_rate', 'yasso_alpha_e'), c('som_n_decomp_rate', 'yasso_alpha_n'),
      c('yasso_rate_pc', 'yasso_rate_pc'), c('yasso_tresp_pc', 'yasso_tres_pc')
    )
    for (param_pair in param_pairs) {
      # Yasso-specific params
      if (param_pair[1] %in% pft.names) {
        run_params[which(names(run_params) == param_pair[2])] <- pft.traits[which(pft.names == param_pair[1])]
      }
    }
    
  } #### End parameter update

  #### Update initial conditions
  if (!is.null(IC)) {
    
    ic.names <- names(IC)
    
    # let's combine these here
    last_vals <- c()
    last_states_file <- file.path(outdir, "last_vals_basgra.Rdata")
    
    if(!file.exists(last_states_file) & is.null(IC$test_vals)){
      PEcAn.logger::logger.severe("Last step output values are missing for restart.")
    }else if(!is.null(IC$test_vals)){
      # for package testing
      last_vals <- IC$test_vals
    }else{
      load(last_states_file)
    }
    
    if ("LAI"  %in% ic.names) {
      run_params[names(run_params) == "LOG10LAII"] <- IC$LAI
    }else{
      run_params[names(run_params) == "LOG10LAII"] <- last_vals[names(last_vals) == "LAI"]
    }
    
    # For Yasso restart
    if(run_params[names(run_params) == "use_yasso"]){
      
      last_somf <- sum(last_vals[names(last_vals) == "CSOM_A"],
                       last_vals[names(last_vals) == "CSOM_W"],
                       last_vals[names(last_vals) == "CSOM_E"],
                       last_vals[names(last_vals) == "CSOM_N"])
      
      last_soms <- last_vals[names(last_vals) == "CSOM_H"]
      
      if ("fast_soil_pool_carbon_content"  %in% ic.names & "slow_soil_pool_carbon_content"  %in% ic.names) {
        
        new_somf <- PEcAn.utils::ud_convert(IC$fast_soil_pool_carbon_content, "kg", "g") 
        new_soms <- PEcAn.utils::ud_convert(IC$slow_soil_pool_carbon_content, "kg", "g") 
        
        ratio_somf <- new_somf / last_somf
        
        # update via ratio
        run_params[names(run_params) == "CSOM_A"] <- ratio_somf * last_vals[names(last_vals) == "CSOM_A"]
        run_params[names(run_params) == "CSOM_W"] <- ratio_somf * last_vals[names(last_vals) == "CSOM_W"]
        run_params[names(run_params) == "CSOM_E"] <- ratio_somf * last_vals[names(last_vals) == "CSOM_E"]
        run_params[names(run_params) == "CSOM_N"] <- ratio_somf * last_vals[names(last_vals) == "CSOM_N"]
        run_params[names(run_params) == "CSOM_H"] <- new_soms
        
        run_params[names(run_params) == "NSOM"] <- ((new_somf+new_soms)/(last_somf+last_soms)) * last_vals[names(last_vals) == "NSOM"]
        
      }else{
        
        run_params[names(run_params) == "CSOM_A"] <- last_vals[names(last_vals) == "CSOM_A"]
        run_params[names(run_params) == "CSOM_W"] <- last_vals[names(last_vals) == "CSOM_W"]
        run_params[names(run_params) == "CSOM_E"] <- last_vals[names(last_vals) == "CSOM_E"]
        run_params[names(run_params) == "CSOM_N"] <- last_vals[names(last_vals) == "CSOM_N"]
        run_params[names(run_params) == "CSOM_H"] <- last_soms
        
        run_params[names(run_params) == "NSOM"] <- last_vals[names(last_vals) == "NSOM"]
        
      }
      
      # #else-if ("TotSoilCarb"  %in% ic.names)?
      # new_totc <- PEcAn.utils::ud_convert(IC$TotSoilCarb, "kg", "g") 
      # 
      # ratio_soc <- new_totc / (last_somf + last_soms)
      # 
      # # update via ratio
      # run_params[names(run_params) == "CSOM_A"] <- ratio_soc * last_vals[names(last_vals) == "CSOM_A"]
      # run_params[names(run_params) == "CSOM_W"] <- ratio_soc * last_vals[names(last_vals) == "CSOM_W"]
      # run_params[names(run_params) == "CSOM_E"] <- ratio_soc * last_vals[names(last_vals) == "CSOM_E"]
      # run_params[names(run_params) == "CSOM_N"] <- ratio_soc * last_vals[names(last_vals) == "CSOM_N"]
      # run_params[names(run_params) == "CSOM_H"] <- ratio_soc * last_vals[names(last_vals) == "CSOM_H"]
      
      
    }else{ # no Yasso
      if ("fast_soil_pool_carbon_content"  %in% ic.names) {
        run_params[names(run_params) == "CSOMF0"] <- PEcAn.utils::ud_convert(IC$fast_soil_pool_carbon_content, "kg", "g")
      }else{
        run_params[names(run_params) == "CSOMF0"] <- last_vals[names(last_vals) == "CSOMF"]
      }
      run_params[names(run_params) == "NSOMF0"]  <- run_params[names(run_params) == "CSOMF0"] / run_params[names(run_params) == "CNSOMF0"]
      
      if ("slow_soil_pool_carbon_content"  %in% ic.names) {
        run_params[names(run_params) == "CSOMS0"] <- PEcAn.utils::ud_convert(IC$slow_soil_pool_carbon_content, "kg", "g")
      }else{
        run_params[names(run_params) == "CSOMS0"] <- last_vals[names(last_vals) == "CSOMS"]
      }
      run_params[names(run_params) == "NSOMS0"]  <- run_params[names(run_params) == "CSOMS0"] / run_params[names(run_params) == "CNSOMS0"]
      
    }
    
    if ("CropYield"  %in% ic.names) {
      run_params[names(run_params) == "YIELDI"] <- PEcAn.utils::ud_convert(IC$CropYield, "kg", "g")
    }else{
      run_params[names(run_params) == "YIELDI"]  <- last_vals[names(last_vals) == "YIELD_POT"]
    }
    
    if ("litter_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CLITT0"] <- PEcAn.utils::ud_convert(IC$litter_carbon_content, "kg", "g")
    }else{
      run_params[names(run_params) == "CLITT0"] <- last_vals[names(last_vals) == "CLITT"]
    }
    #run_params[names(run_params) == "NLITT0"] <- run_params[names(run_params) == "CLITT0"] / run_params[names(run_params) == "CNLITT0"]
    run_params[which(names(run_params) == "NLITT0")] <- last_vals[names(last_vals) == "NLITT"]
    
    if ("stubble_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CSTUBI"] <- PEcAn.utils::ud_convert(IC$stubble_carbon_content, "kg", "g")
    }else{
      run_params[names(run_params) == "CSTUBI"] <- last_vals[names(last_vals) == "CSTUB"]
    }
   
    if ("stem_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CSTI"] <- PEcAn.utils::ud_convert(IC$stem_carbon_content, "kg", "g")
    }else{
      run_params[names(run_params) == "CSTI"] <- last_vals[names(last_vals) == "CST"]
    }
    
    # NRT        = NCR * CRTI
    #run_params[names(run_params) == "NCR"] <- last_vals[names(last_vals) == "NRT"] / last_vals[names(last_vals) == "CRT"] 
    if ("root_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "LOG10CRTI"] <-  PEcAn.utils::ud_convert(IC$root_carbon_content, "kg", "g")
    }else{
      run_params[names(run_params) == "LOG10CRTI"] <- last_vals[names(last_vals) == "CRT"]
    }
    run_params[which(names(run_params) == "NRTI")] <- run_params[names(run_params) == "LOG10CRTI"]*run_params[names(run_params) == "NCR"]
   # if(run_params[which(names(run_params) == "NRTI")] <= 0) run_params[which(names(run_params) == "NRTI")] <- 0.0001

    # # NCSHI    = NCSHMAX * (1-EXP(-K*LAII)) / (K*LAII)
    # # NSH      = NCSHI * (CLVI+CSTI)
    lai_tmp <- run_params[names(run_params) == "LOG10LAII"]
    ncshi <- run_params[names(run_params) == "NCSHMAX"] *
      (1-exp(-run_params[names(run_params) == "K"]*lai_tmp)) / (run_params[names(run_params) == "K"]*lai_tmp)
    run_params[which(names(run_params) == "NSHI")] <- ncshi *
      ((run_params[names(run_params) == "LOG10CLVI"]) + run_params[names(run_params) == "CSTI"])
    

    if ("reserve_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "LOG10CRESI"] <- PEcAn.utils::ud_convert(IC$reserve_carbon_content, "kg", "g")
    }else{
      run_params[names(run_params) == "LOG10CRESI"] <- last_vals[names(last_vals) == "CRES"]
    }
    
    if ("leaf_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "LOG10CLVI"] <- PEcAn.utils::ud_convert(IC$leaf_carbon_content, "kg", "g")
    }else{
      run_params[names(run_params) == "LOG10CLVI"] <- last_vals[names(last_vals) == "CLV"]
    }
    
    if ("dead_leaf_carbon_content"  %in% ic.names) {
      run_params[names(run_params) == "CLVDI"] <- PEcAn.utils::ud_convert(IC$dead_leaf_carbon_content, "kg", "g")
    }else{
      run_params[names(run_params) == "CLVDI"] <- last_vals[names(last_vals) == "CLVD"]
    }
    
    if ("tiller_density"  %in% ic.names) {
      run_params[names(run_params) == "TILTOTI"] <- IC$tiller_density # all the tillers are updated from this with respect to model preserved ratios
    }else{
      run_params[names(run_params) == "TILTOTI"] <- last_vals[names(last_vals) == "TILTOT"]
    }

    run_params[names(run_params) == "FRTILGI"] <- last_vals[names(last_vals) == "FRTILG"] 
    
    if(run_params[names(run_params) == "FRTILGI"] == 0) run_params[names(run_params) == "FRTILGI"] <- 0.01
    
    #TILV       = TILTOTI * (1. - FRTILGI)
    if ("nonelongating_vegetative_tiller"  %in% ic.names) {
      run_params[names(run_params) == "TILVI"] <-  IC$nonelongating_vegetative_tiller
      # preserve ratio
      #run_params[names(run_params) == "FRTILGI"] <- 1 - (run_params[names(run_params) == "TILVI"]/run_params[names(run_params) == "TILTOTI"])
    }else{
      run_params[names(run_params) == "TILVI"]  <- run_params[names(run_params) == "TILTOTI"] * (1-run_params[names(run_params) == "FRTILGI"])
    }
    
    gtil <- run_params[names(run_params) == "TILTOTI"] - run_params[names(run_params) == "TILVI"]
    
    #TILG1      = TILTOTI *       FRTILGI *    FRTILGG1I
    if ("nonelongating_generative_tiller"  %in% ic.names) {
      run_params[names(run_params) == "TILG1I"] <-  IC$nonelongating_generative_tiller
      # can also update FRTILGG1I but I don't throw these into the state matrix anymore and TILG1I is initialized from its own variable, not derived from fractions
    }else{
      run_params[names(run_params) == "TILG1I"] <- gtil*(last_vals[names(last_vals) == "TILG1"]  / 
                                                           (last_vals[names(last_vals) == "TILG1"]+last_vals[names(last_vals) == "TILG2"]))
      if(is.nan(run_params[names(run_params) == "TILG1I"])) run_params[names(run_params) == "TILG1I"] <- 1
      #if(is.infinite(run_params[names(run_params) == "TILG1I"])) run_params[names(run_params) == "TILG1I"] <- 1
    }
    
    #TILG2      = TILTOTI *       FRTILGI * (1-FRTILGG1I)
    if ("elongating_generative_tiller"  %in% ic.names) {
      run_params[names(run_params) == "TILG2I"] <-  IC$elongating_generative_tiller
    }else{
      run_params[names(run_params) == "TILG2I"] <- gtil*(last_vals[names(last_vals) == "TILG2"]  / 
                                                           (last_vals[names(last_vals) == "TILG1"]+last_vals[names(last_vals) == "TILG2"]))
      if(is.nan(run_params[names(run_params) == "TILG2I"])) run_params[names(run_params) == "TILG2I"] <- 1
      #  if(is.infinite(run_params[names(run_params) == "TILG2I"])) run_params[names(run_params) == "TILG2I"] <- 1
    }
    
    if ("phenological_stage"  %in% ic.names) {
      run_params[names(run_params) == "PHENI"] <- IC$phenological_stage
    }else{
      run_params[names(run_params) == "PHENI"] <- last_vals[names(last_vals) == "PHEN"]
    }

    if ("lethal_temperature50"  %in% ic.names) {
      run_params[names(run_params) == "LT50I"] <- IC$lethal_temperature50
    }else{
      run_params[names(run_params) == "LT50I"] <- last_vals[names(last_vals) == "LT50"]
    }
    
    
    if ("rooting_depth"  %in% ic.names) {
      run_params[names(run_params) == "ROOTDM"] <- IC$rooting_depth
    }else{
      run_params[names(run_params) == "ROOTDM"] <- last_vals[names(last_vals) == "ROOTD"] # this doesn't change
    }
    
    # these change too
    run_params[names(run_params) == "TEMPR30"]  <- last_vals[names(last_vals) == "TEMPR30"]
    run_params[names(run_params) == "PRECIP30"]  <- last_vals[names(last_vals) == "PRECIP30"]
    
    run_params[names(run_params) == "DAYLI"]  <- last_vals[names(last_vals) == "DAYL"]
    
    run_params[names(run_params) == "NMIN0"]  <- last_vals[names(last_vals) == "NMIN"]
    
    run_params[names(run_params) == "O2I"]    <- last_vals[names(last_vals) == "O2"]
    
    
    # water stuff, to be in SDA 
    
    run_params[names(run_params) == "DRYSTORI"] <- last_vals[names(last_vals) == "DRYSTOR"]
    run_params[names(run_params) == "FdepthI"]  <- last_vals[names(last_vals) == "Fdepth"]
    run_params[names(run_params) == "SDEPTHI"]  <- last_vals[names(last_vals) == "Sdepth"]
    run_params[names(run_params) == "TANAERI"]  <- last_vals[names(last_vals) == "TANAER"]
    run_params[names(run_params) == "WAPLI"]    <- last_vals[names(last_vals) == "WAPL"]
    run_params[names(run_params) == "WAPSI"]    <- last_vals[names(last_vals) == "WAPS"]
    run_params[names(run_params) == "WASI"]     <- last_vals[names(last_vals) == "WAS"]
    run_params[names(run_params) == "WETSTORI"] <- last_vals[names(last_vals) == "WETSTOR"]
    
    #  WAL        = 1000. * ROOTDM * WCI
    if ("SoilMoistFrac"  %in% ic.names) {
      run_params[names(run_params) == "WCI"] <-  IC$SoilMoistFrac
      run_params[names(run_params) == "WALI"]  <- 1000. * (run_params[names(run_params) == "ROOTDM"] - run_params[names(run_params) == "FdepthI"]) * run_params[names(run_params) == "WCI"]
    }else{
      run_params[names(run_params) == "WALI"]  <- last_vals[names(last_vals) == "WAL"] 
      run_params[names(run_params) == "WCI"]   <- run_params[names(run_params) == "WALI"] / (1000 * (run_params[names(run_params) == "ROOTDM"]- run_params[names(run_params) == "FdepthI"]))
    }
    
    yasso_pools <- c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N', 'CSOM_H', 'NSOM', 'TEMPR30', 'PRECIP30')
    for (p in yasso_pools) {
      if (p %in% ic.names) {
        run_params[names(run_params) == p] <- IC[[p]]
      }
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
    nmin0 <- try(ncdf4::ncvar_get(IC.nc, "soil_inorganic_nitrogen_content"), silent = TRUE)
    if (!is.na(nmin0) && is.numeric(nmin0)) {
      run_params[which(names(run_params) == "NMIN0")] <- PEcAn.utils::ud_convert(nmin0, "kg", "g")
    }
    
        # Initial organic N
    nsom0 <- try(ncdf4::ncvar_get(IC.nc, "soil_organic_nitrogen_content"), silent = TRUE)
    if (!is.na(nsom0) && is.numeric(nsom0)) {
      run_params[which(names(run_params) == "NSOM")] <- PEcAn.utils::ud_convert(nsom0, "kg", "g")
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
    
    tilg1 <- try(ncdf4::ncvar_get(IC.nc, "nonelongating_generative_tiller"), silent = TRUE)
    if (!is.na(tilg1) && is.numeric(tilg1)) {
      run_params[names(run_params) == "TILG1I"] <-  tilg1
    }
    
    tilg2 <- try(ncdf4::ncvar_get(IC.nc, "elongating_generative_tiller"), silent = TRUE)
    if (!is.na(tilg2) && is.numeric(tilg2)) {
      run_params[names(run_params) == "TILG2I"] <-  tilg2
    }
    
    tilv <- try(ncdf4::ncvar_get(IC.nc, "nonelongating_vegetative_tiller"), silent = TRUE)
    if (!is.na(tilv) && is.numeric(tilv)) {
      run_params[names(run_params) == "TILVI"] <-  tilv
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

    yasso_pools <- c('CSOM_A', 'CSOM_W', 'CSOM_E', 'CSOM_N', 'CSOM_H', 'NSOM', 'TEMPR30', 'PRECIP30')
    for (p in yasso_pools) {
      value <- try(ncdf4::ncvar_get(IC.nc, p), silent=TRUE)
      if (!is.na(value) && is.numeric(value)) {
        run_params[names(run_params) == p] <- value
      }
    }
  }

  
  # if the default parameter file is set to force some parameter values, override the trait.values here:
  if ('force' %in% colnames(df_run_params)) {
    mask <- as.logical(df_run_params$force)
    run_params[mask] <- df_run_params$value[mask]
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
  if (!is.null(settings$run$write.raw.output)) {
    jobsh <- gsub("@WRITE_RAW_OUTPUT@", settings$run$write.raw.output, jobsh)
  } else {
    jobsh <- gsub("@WRITE_RAW_OUTPUT@", FALSE, jobsh)
  }
  
  jobsh <- gsub(
    "@RUN_PARAMS@",
    paste0("c(", PEcAn.utils::listToArgString(run_params), ")"),
    jobsh)
  writeLines(jobsh, con = file.path(settings$rundir, run.id, "job.sh"))
  Sys.chmod(file.path(settings$rundir, run.id, "job.sh"))
  
  
} # write.config.BASGRA
