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
  
  prior.sla <- params[[which(!names(params) %in% c("soil", "soil_SDA", "restart"))[1]]]$SLA
  
  forecast <- list()
  
  # additional varnames, because we need these deterministic relationships
  var.names <- c(var.names, "fine_root_carbon_content", "coarse_root_carbon_content")
  
  # Read ensemble output
  ens <- read.output(runid = runid, 
                     outdir = file.path(outdir, runid), 
                     start.year = lubridate::year(stop.time), 
                     end.year = lubridate::year(stop.time),
                     variables = var.names)
  
  last <- length(ens[[1]])
  
  forecast <- list()

  
  #### PEcAn Standard Outputs
  if ("GWBI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- udunits2::ud.convert(mean(ens$GWBI),  "kg/m^2/s", "Mg/ha/yr")
    names(forecast[[length(forecast)]]) <- c("GWBI")
  }
  
  if ("AbvGrndWood" %in% var.names) {
    forecast[[length(forecast) + 1]] <- udunits2::ud.convert(ens$AbvGrndWood[last],  "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("AbvGrndWood")
    
    # calculate fractions, store in params, will use in write_restart
    wood_total_C    <- ens$AbvGrndWood[last] + ens$fine_root_carbon_content[last] + ens$coarse_root_carbon_content[last]
    abvGrndWoodFrac <- ens$AbvGrndWood[last]  / wood_total_C
    coarseRootFrac  <- ens$coarse_root_carbon_content[last] / wood_total_C
    fineRootFrac    <- ens$fine_root_carbon_content[last]   / wood_total_C
    params$restart <- c(abvGrndWoodFrac, coarseRootFrac, fineRootFrac)

    if (length(params$restart)>0)
    names(params$restart) <- c("abvGrndWoodFrac", "coarseRootFrac", "fineRootFrac")
  }
  
  if ("leaf_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$leaf_carbon_content[last]  ## kgC/m2*m2/kg*2kg/kgC
    names(forecast[[length(forecast)]]) <- c("LeafC")
  }
  
  if ("litter_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_carbon_content[last]  ##kgC/m2
    names(forecast[[length(forecast)]]) <- c("Litter")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last]  ## kgC/m2
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }
  
  if ("SoilMoistFrac" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoistFrac[last]  ## unitless
    names(forecast[[length(forecast)]]) <- c("SoilMoistFrac")
  }
  
  if ("SWE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SWE[last]  ## kgC/m2
    names(forecast[[length(forecast)]]) <- c("SWE")
  }
  
  print(runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
} # read_restart.SIPNET
