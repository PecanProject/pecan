#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

sample.IC.LINKAGES <- function(ne, state, year = NULL) {
  ## g C * m-2 ground area in wood (above-ground + roots)
  biomass_tsca = ifelse(rep("biomass_tsca" %in% names(state), ne),
                        state$biomass_tsca[1, sample.int(ncol(state$biomass_tsca),ne), 1] * 0.1, ## unit Mg/ha ->kg/m2
                        runif(ne, 0, 14000)) ## prior    
  biomass_acsa3 = ifelse(rep("biomass_acsa3" %in% names(state), ne),
                         state$biomass_acsa3[1, sample.int(ncol(state$biomass_acsa3), ne), 1] * 0.1, ## unit Mg/ha ->kg/m2
                         runif(ne, 0, 14000)) ## prior  
  biomass_beal2 = ifelse(rep("biomass_beal2" %in% names(state),ne),
                         state$biomass_beal2[1, sample.int(ncol(state$biomass_beal2),ne), 1] * 0.1, ## unit Mg/ha ->kg/m2
                         runif(ne, 0, 14000)) ## prior  
  biomass_thoc2 = ifelse(rep("biomass_thoc2" %in% names(state),ne),
                         state$biomass_thoc2[1, sample.int(ncol(state$biomass_thoc2), ne), 1] * 0.1, ## unit Mg/ha ->kg/m2
                         runif(ne, 0, 14000)) ## prior  
  return(data.frame(biomass_tsca, biomass_acsa3, biomass_beal2, biomass_thoc2))
} # sample.IC.LINKAGES
