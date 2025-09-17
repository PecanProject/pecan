#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' @title Read restart function for SDA with SIPNET
##' 
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @inheritParams PEcAn.ModelName::read_restart.ModelName
##' 
##' @description Read Restart for SIPNET
##' 
##' @return X.vec      vector of forecasts
##' @export
read_restart.SIPNET <- function(outdir, runid, stop.time, settings, var.names, params) {
  
  # local helpers
  .safe_num <- function(x, default = NA_real_) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (is.finite(x)) x else default
  }
  grab_scalar <- function(v, last_idx) {
    val <- tryCatch(v[last_idx], error = function(e) NA_real_)
    if (length(val) == 0) val <- NA_real_
    .safe_num(val, default = NA_real_)
  }
  nz_num <- function(x) { if (is.finite(x)) x else 0 }
  
  prior.sla <- params[[which(!names(params) %in% c("soil", "soil_SDA", "restart"))[1]]]$SLA
  
  forecast <- list()
  params$restart <-c() #state.vars not in var.names will be added here
  #SIPNET inital states refer to models/sipnet/inst/template.param
  state.vars <- c("SWE", "SoilMoistFrac", "AbvGrndWood", "TotSoilCarb", "LAI", 
                  "litter_carbon_content", "fine_root_carbon_content", 
                  "coarse_root_carbon_content", "litter_mass_content_of_water", "GWBI")
  #when adding new state variables make sure the naming is consistent across read_restart, write_restart and write.configs
  #pre-populate parsm$restart with NAs so state names can be added
  params$restart <- rep(NA, length(setdiff(state.vars, var.names)))
  #add states to params$restart NOT in var.names
  names(params$restart) <- setdiff(state.vars, var.names)
  # Read ensemble output
  ens <- PEcAn.utils::read.output(runid = runid,
                                  outdir = file.path(outdir, runid),
                                  start.year = lubridate::year(stop.time),
                                  end.year = lubridate::year(stop.time),
                                  variables = c(state.vars,"time_bounds"))
  #calculate last
  start.time <- as.Date(paste0(lubridate::year(stop.time),"-01-01"))
  time_var <- ens$time_bounds[1,]
  real_time <- as.POSIXct(time_var*3600*24, origin = start.time)
  # last <- which(as.Date(real_time)==as.Date(stop.time))[1]
  
  # restart index (exact match; else latest prior; else last)
  idxs <- which(as.Date(real_time) == as.Date(stop.time))
  if (length(idxs) > 0) {
    last <- tail(idxs, 1)
  } else {
    prior <- which(as.Date(real_time) <= as.Date(stop.time))
    if (length(prior) > 0) {
      last <- max(prior)
      PEcAn.logger::logger.warn("read_restart.SIPNET: no exact match for stop.time; using most recent prior step.")
    } else {
      last <- length(real_time)
      PEcAn.logger::logger.warn("read_restart.SIPNET: no prior step exists; using final timestep in file.")
    }
  }
  
  #### PEcAn Standard Outputs
  if ("AbvGrndWood" %in% var.names) {
    # AbvGrndWood -> forecast (Mg/ha) and skip if invalid
    val <- tryCatch(
      PEcAn.utils::ud_convert(.safe_num(ens$AbvGrndWood[last]), "kg/m^2", "Mg/ha"),
      error = function(e) NA_real_
    )
    forecast[[length(forecast) + 1]] <- if (length(val) == 1 && is.finite(val)) val else NA_real_
    names(forecast[[length(forecast)]]) <- "AbvGrndWood"
    if (!is.finite(forecast[[length(forecast)]][1])) {
      PEcAn.logger::logger.warn("AbvGrndWood missing/invalid at restart; using NA.")
    }
    
    # robust wood fractions
    abv <- .safe_num(ens$AbvGrndWood[last])
    fr  <- .safe_num(ens$fine_root_carbon_content[last])
    cr  <- .safe_num(ens$coarse_root_carbon_content[last])
    
    wood_total_C <- abv + fr + cr
    if (!is.finite(wood_total_C) && wood_total_C > 0) wood_total_C <- 1e-4
    
    params$restart["abvGrndWoodFrac"] <- abv / wood_total_C
    params$restart["coarseRootFrac"]  <- cr  / wood_total_C
    params$restart["fineRootFrac"]    <- fr  / wood_total_C
    
  } else {
    # store AbvGrndWood (g/m^2) into params
    val_gm2 <- tryCatch(
      PEcAn.utils::ud_convert(.safe_num(ens$AbvGrndWood[last]), "kg/m^2", "g/m^2"),
      error = function(e) NA_real_
    )
    if (length(val_gm2) == 1 && is.finite(val_gm2)) {
      params$restart["AbvGrndWood"] <- val_gm2
    } else {
      PEcAn.logger::logger.warn("AbvGrndWood missing/invalid; not setting params$restart['AbvGrndWood'].")
    }
    
    # robust wood fractions
    abv <- .safe_num(ens$AbvGrndWood[last])
    fr  <- .safe_num(ens$fine_root_carbon_content[last])
    cr  <- .safe_num(ens$coarse_root_carbon_content[last])
    
    wood_total_C <- abv + fr + cr
    if (!isTRUE(is.finite(wood_total_C) && wood_total_C > 0)) wood_total_C <- 1e-4
    
    params$restart["abvGrndWoodFrac"] <- abv / wood_total_C
    params$restart["coarseRootFrac"]  <- cr  / wood_total_C
    params$restart["fineRootFrac"]    <- fr  / wood_total_C
  }
  
  
  if ("GWBI" %in% var.names) {
    gwbi_vec <- suppressWarnings(as.numeric(unlist(ens$GWBI)))
    if (!length(gwbi_vec) || all(!is.finite(gwbi_vec))) {
      PEcAn.logger::logger.warn("GWBI present but non-numeric/NA; setting GWBI = NA")
      gwbi_ann <- NA_real_
    } else {
      # sum over the year
      gwbi_ann <- sum(gwbi_vec, na.rm = TRUE)   # -> kg C m-2 yr-1
    }
    forecast[[length(forecast) + 1]] <- gwbi_ann
    names(forecast[[length(forecast)]]) <- "GWBI"
  }
  
  # Reading in NET Ecosystem Exchange for SDA - unit is kg C m-2 s-1 and the average is estimated
  if ("NEE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- mean(ens$NEE)  ## 
    names(forecast[[length(forecast)]]) <- c("NEE")
  }
  
  
  # Reading in Latent heat flux for SDA  - unit is MW m-2
  if ("Qle" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$Qle[last]*1e-6  ##  
    names(forecast[[length(forecast)]]) <- c("Qle")
  }
  
  if ("leaf_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$leaf_carbon_content[last]  ## kgC/m2*m2/kg*2kg/kgC
    names(forecast[[length(forecast)]]) <- c("LeafC")
  }
  
  if ("LAI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$LAI[last]  ## m2/m2 
    names(forecast[[length(forecast)]]) <- c("LAI")
  }else{
    params$restart["LAI"] <- ens$LAI[last]
  }
  
  if ("litter_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_carbon_content[last]  ##kgC/m2
    names(forecast[[length(forecast)]]) <- c("litter_carbon_content")
  }else{
    params$restart["litter_carbon_content"] <- PEcAn.utils::ud_convert(ens$litter_carbon_content[last], 'kg m-2', 'g m-2')
  }
  
  if ("litter_mass_content_of_water" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_mass_content_of_water[last]  ##kgC/m2
    names(forecast[[length(forecast)]]) <- c("litter_mass_content_of_water")
  }else{
    params$restart["litter_mass_content_of_water"] <- ens$litter_mass_content_of_water[last]
  }
  
  if ("SoilMoistFrac" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoistFrac[last]*100  ## here we multiply it by 100 to convert from proportion to percentage.
    names(forecast[[length(forecast)]]) <- c("SoilMoistFrac")
  }else{
    params$restart["SoilMoistFrac"] <- ens$SoilMoistFrac[last]
  }
  
  # This is snow
  if ("SWE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SWE[last]  ## kgC/m2
    names(forecast[[length(forecast)]]) <- c("SWE")
  }else{
    params$restart["SWE"] <- ens$SWE[last]/10
  }
  
  if ("TotLivBiom" %in% var.names) {
    forecast[[length(forecast) + 1]] <- PEcAn.utils::ud_convert(ens$TotLivBiom[last],  "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("TotLivBiom")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last]
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }else{
    params$restart["TotSoilCarb"] <- PEcAn.utils::ud_convert(ens$TotSoilCarb[last], 'kg m-2', 'g m-2')
  }
  
  #remove any remaining NAs from params$restart
  params$restart <- stats::na.omit(params$restart)
  
  print(runid)
  
  # normalize forecast to exactly var.names
  fv <- setNames(rep(NA_real_, length(var.names)), var.names)
  if (length(forecast)) {
    raw <- unlist(forecast, use.names = TRUE)
    if (length(raw)) {
      keep <- intersect(names(raw), var.names)
      if (length(keep)) fv[keep] <- suppressWarnings(as.numeric(raw[keep]))
    }
  }
  
  X_tmp <- list(X = fv, params = params)
  return(X_tmp)
} # read_restart.SIPNET
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
} # read_restart.SIPNET
