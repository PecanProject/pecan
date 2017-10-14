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
  
  prior.sla <- params[[which(names(params) != "soil")[1]]]$SLA
  
  forecast <- list()
  
  # Read ensemble output
  ens <- read.output(runid = runid, 
                     outdir = file.path(outdir, runid), 
                     start.year = lubridate::year(stop.time), 
                     end.year = lubridate::year(stop.time),
                     variables = var.names)
  
  last <- length(ens$NPP)
  
  forecast <- list()
  
  # unit.conv <- (10000/1)*(1/1000)*(365.25*24*60*60) ## kgC m-2 s-1 -> MgC/ha/yr
  
  #### PEcAn Standard Outputs
  if ("NPP" %in% var.names) {
    forecast[[1]] <- mean(ens$NPP)  #* unit.conv 
    names(forecast[[1]]) <- c("NPP")
  }
  
  if ("AbvGrndWood" %in% var.names) {
    forecast[[2]] <- ens$AbvGrndWood[last] / (1 - 0.2 - 0.2)  ## kgC/m2
    names(forecast[[2]]) <- c("AbvGrndWood")
  }
  
  if ("LeafC" %in% var.names) {
    forecast[[3]] <- ens$LeafC[last]  ## kgC/m2*m2/kg*2kg/kgC
    names(forecast[[3]]) <- c("LeafC")
  }
  
  if ("Litter" %in% var.names) {
    forecast[[4]] <- ens$Litter[last]  ##kgC/m2
    names(forecast[[4]]) <- c("Litter")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[5]] <- ens$TotSoilCarb[last]  ## kgC/m2
    names(forecast[[5]]) <- c("TotSoilCarb")
  }
  
  if ("SoilMoistFrac" %in% var.names) {
    forecast[[6]] <- ens$SoilMoistFrac[last]  ## kgC/m2
    names(forecast[[6]]) <- c("SoilMoistFrac")
  }
  
  if ("SWE" %in% var.names) {
    forecast[[7]] <- ens$SWE[last]  ## kgC/m2
    names(forecast[[7]]) <- c("SWE")
  }
  
  print(runid)
  return(unlist(forecast))
} # read_restart.SIPNET
