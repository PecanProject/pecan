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
  
  ## Mg C / ha / yr GWBI
  ## no conversion needed because SIPNET doesn't take GWBI as IC anyway
  GWBI <- ifelse(rep("GWBI" %in% names(state), ne), 
                state$GWBI[sample.int(length(state$GWBI), ne)], ## unit MgC ha-1 yr-1
                runif(ne, 0, 10))  ## prior
  
  # g C * m-2 ground area in wood (above-ground + roots)
  Mgha2gm <- (1000000) / (10000) # these unit conversions are for testing
  # reminder : when working with kgC m-2 s-1 as NPP units singularity issues pop up in sda.enkf
  # using MgC ha-1 yr-1 for NPP in SDA and also brought back AbvGrndWood to MgC ha-1 for sanity reasons
  AbvGrndWood <- ifelse(rep("AbvGrndWood" %in% names(state), ne), 
                        PEcAn.utils::ud_convert(state$AbvGrndWood[sample.int(length(state$AbvGrndWood), ne)],  "Mg/ha", "g/m^2"), 
                        runif(ne, 700, 15000))  ## prior
  
  # sipnet accepts a plantWoodC pool that is above-ground + roots
  # instead of roots having their own state, we'll pass around fractions to update them deterministically
  fine_root_carbon_content <- runif(ne, 100, 1000)
  coarse_root_carbon_content <- runif(ne, 200, 2000)

  wood_total_C <- AbvGrndWood + fine_root_carbon_content + coarse_root_carbon_content
  
  abvGrndWoodFrac <- AbvGrndWood / wood_total_C
  coarseRootFrac  <- coarse_root_carbon_content / wood_total_C
  fineRootFrac    <- fine_root_carbon_content /  wood_total_C
  
  # initial leaf area, m2 leaves * m-2 ground area (multiply by leafCSpWt to
  ## get initial plant leaf C)
  lai <- ifelse(rep("LAI" %in% names(state), ne), 
                state$LAI[1, sample.int(ncol(state$LAI), ne), year], 
                runif(ne, 0, 7))  ## prior
  
  ## g C * m-2 ground area
  litter <- ifelse(rep("litter" %in% names(state), ne), 
                   state$litter[1, sample.int(ncol(state$litter), ne), year], 
                   runif(ne, 130, 1200))  ## prior
  
  ## g C * m-2 ground area
  soil <- ifelse(rep("soil" %in% names(state), ne), 
                 state$soil[1, sample.int(ncol(state$soil), ne), year], 
                 runif(ne, 1200, 2000))  ## prior
  
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
                 runif(ne, 0, 2000))  ## prior
  
  microbe <- ifelse(rep("microbe" %in% names(state), ne), 
                    state$microbe[1, sample.int(ncol(state$microbe), ne), year], 
                    runif(ne, 0.02, 1))  ## prior 
  
  return(data.frame(GWBI, AbvGrndWood, abvGrndWoodFrac, coarseRootFrac, fineRootFrac,  lai, litter,
                    soil, litterWFrac, soilWFrac, snow, microbe))
} # sample.IC.SIPNET
