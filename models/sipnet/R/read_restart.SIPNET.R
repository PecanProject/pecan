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
  

  params$restart <-c() ## This will be filled with some restart coefficient if above ground wood is in the state variables.

  #### PEcAn Standard Outputs
  if ("AbvGrndWood" %in% var.names) {
    forecast[[length(forecast) + 1]] <- udunits2::ud.convert(ens$AbvGrndWood[last],  "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("AbvGrndWood")
   
    # calculate fractions, store in params, will use in write_restart
    wood_total_C    <- ens$AbvGrndWood[last] + ens$fine_root_carbon_content[last] + ens$coarse_root_carbon_content[last]
    if (wood_total_C<=0) wood_total_C <- 0.0001 # Making sure we are not making Nans in case there is no plant living there.
    
    abvGrndWoodFrac <- ens$AbvGrndWood[last]  / wood_total_C
    coarseRootFrac  <- ens$coarse_root_carbon_content[last] / wood_total_C
    fineRootFrac    <- ens$fine_root_carbon_content[last]   / wood_total_C
    params$restart <- c(abvGrndWoodFrac, coarseRootFrac, fineRootFrac)

    if (length(params$restart)>0)
    names(params$restart) <- c("abvGrndWoodFrac", "coarseRootFrac", "fineRootFrac")
  }

  if ("GWBI" %in% var.names) {
       forecast[[length(forecast) + 1]] <- udunits2::ud.convert(mean(ens$GWBI),  "kg/m^2/s", "Mg/ha/yr")
           names(forecast[[length(forecast)]]) <- c("GWBI")
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
  }
  
  if ("litter_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$litter_carbon_content[last]  ##kgC/m2
    names(forecast[[length(forecast)]]) <- c("litter_carbon_content")
  }

    
  if ("SoilMoistFrac" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SoilMoistFrac[last]  ## unitless
    names(forecast[[length(forecast)]]) <- c("SoilMoistFrac")
  }
  
  # This is snow
  if ("SWE" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$SWE[last]  ## kgC/m2
    names(forecast[[length(forecast)]]) <- c("SWE")
  }
  
  if ("TotLivBiom" %in% var.names) {
    forecast[[length(forecast) + 1]] <- udunits2::ud.convert(ens$TotLivBiom[last],  "kg/m^2", "Mg/ha")
    names(forecast[[length(forecast)]]) <- c("TotLivBiom")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last]  ## kgC/m2
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }
 
  print(runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
} # read_restart.SIPNET