#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

## samples intial conditions for SIPNET
##' @title sample.IC.SIPNET
##' @name  sample.IC.SIPNET
##' @author Mike Dietze and Ann Raiho
##' 
##' @param ne number of ensembles
##' @param state state variables you want to pull
##' @description samples intial conditions for SIPNET
##' 
##' @return IC matrix of initial conditions
##' @export
##' 
sample.IC.SIPNET <- function(ne, state, year = 1) {
  
  ## Mg C / ha / yr NPP
  NPP <- ifelse(rep("NPP" %in% names(state), ne), 
                udunits2::ud.convert(state$NPP[sample.int(length(state$NPP), ne)],'kg/m^2/s','Mg/ha/yr'), # *.48, ## unit MgC/ha/yr
                runif(ne, 0, 10))  ## prior
  
  # g C * m-2 ground area in wood (above-ground + roots)
  plantWood <- ifelse(rep("AGB" %in% names(state), ne), 
                      udunits2::ud.convert(state$AGB[sample.int(length(state$AGB), ne)],'kg/m^2','g/m^2'), ## unit KgC/ha -> g C /m^2 
                      runif(ne, 0, 14000))  ## prior
  
  # initial leaf area, m2 leaves * m-2 ground area (multiply by leafCSpWt to
  ## get initial plant leaf C)
  lai <- ifelse(rep("LAI" %in% names(state), ne), 
                state$LAI[1, sample.int(ncol(state$LAI), ne), year], 
                runif(ne, 0, 7))  ## prior
  
  ## g C * m-2 ground area
  litter <- ifelse(rep("litter" %in% names(state), ne), 
                   state$litter[1, sample.int(ncol(state$litter), ne), year], 
                   runif(ne, 0, 1200))  ## prior
  
  ## g C * m-2 ground area
  soil <- ifelse(rep("soil" %in% names(state), ne), 
                 state$soil[1, sample.int(ncol(state$soil), ne), year], 
                 runif(ne, 0, 19000))  ## prior
  
  ## unitless: fraction of litterWHC
  litterWFrac <- ifelse(rep("litterW" %in% names(state), ne), 
                        state$litterW[1, sample.int(ncol(state$litterW), ne), year], 
                        runif(ne))  ## prior
  
  ## unitless: fraction of soilWHC
  soilWFrac <- ifelse(rep("soilW" %in% names(state), ne), 
                      state$soilW[1, sample.int(ncol(state$soilW), ne), year],
                      runif(ne))  ## prior
  
  ## cm water equiv
  snow <- ifelse(rep("snow" %in% names(state), ne), 
                 state$snow[1, sample.int(ncol(state$snow), ne), year], 
                 runif(ne, 0, 1))  ## prior
  
  microbe <- ifelse(rep("microbe" %in% names(state), ne), 
                    state$microbe[1, sample.int(ncol(state$microbe), ne), year], 
                    runif(ne, 0, 1))  ## prior 
  
  return(data.frame(NPP, plantWood, lai, litter,
                    soil, litterWFrac, soilWFrac, snow, microbe))
} # sample.IC.SIPNET
