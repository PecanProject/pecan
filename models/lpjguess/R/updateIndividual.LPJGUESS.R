#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#' Adjust LPJ-GUESS state
#'
#' @title updateState.LPJGUESS
#'
#' @description 
#'
#'
#' @param model.state A large multiply-nested list containing the entire LPJ-GUESS state as read by 
#' function \code{readStateBinary.LPJGUESS} 
#' @param dens.initial A numeric vector of the initial stand-level stem densities (indiv/m^2) as named numeric vector 
#' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
#' using state data assimilation from function XXXXXX.  
#' @param dens.target A numeric vector of the target stand-level stem densities (indiv/m^2) as named numeric vector 
#' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
#' using state data assimilation from function XXXXXX 
#' @param biomass.target A numeric vector of the target stand-level biomasses (kgC/m^2) as named numeric vector 
#' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
#' using state data assimilation from function XXXXXX 
#' @param biomass.target A numeric vector of the target stand-level biomasses (kgC/m^2) as named numeric vector 
#' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
#' using state data assimilation from function XXXXXX 
#' @return  And updated model state (as a big old list o' lists)
#' @export
#' @author Matthew Forrest



updateState.LPJGUESS <- function(model.state, dens.initial, dens.target, biomass.initial, biomass.target) {
  
  
  # calculate relative increases to be applied later on (per PFT)
  dens.rel.change <- dens.target/dens.initial
  biomass.rel.change <- biomass.target/biomass.initial
  print(dens.rel.change)
  print(biomass.rel.change)
  
  
  # nstands - should always be 1 but lets make sure
  nstands <- unlist(model.state$nstands)
  if(nstands != 1) warning("More than one Stand found in LPJ-GUESS state.  This possibly implies that land use has been enabled
                           which the PEcAn code might not be robust against.")
  
  #
  for(stand.counter in 1:nstands) {
    
    # get the number of patches
    npatches <- model.state$Stand[[stand.counter]]$npatches
    # MF hack for now
    #npatches <- 1
    
    # get list of all the PFTs included in this stand
    active.PFTs <- c()
    for(stand.pft.id in 1:length(model.state$Stand[[stand.counter]]$Standpft$active)) {
      if(model.state$Stand[[stand.counter]]$Standpft$active[[stand.pft.id]]) active.PFTs <- append(active.PFTs, stand.pft.id -1)
    }
    
    
    # loop through each patch
    for(patch.counter in 1:npatches) {
      
      this.patch <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]
      
      # pull out the number of individuals and a list of them   
      nindividuals <- this.patch$Vegetation$number_of_individuals
      all.individuals <- this.patch$Vegetation$Individuals
      
      
      # for each individual
      for(individual.counter in 1:length(all.individuals)) {
        
        this.individual <- all.individuals[[individual.counter]]
        
        if(this.individual$alive) {
          
          this.pft.id <- this.individual$indiv.pft.id
          print(paste("PFT id = ", this.pft.id))
          
          if(!this.pft.id %in% active.PFTs) stop(paste0("Found individual of PFT id = ",this.pft.id, 
                                                        " but this doesn't seem to be active in the LPJ-GUESS run"))
          
          # STEP 0 - store the initial C-N ratios which we will use to magic up some new N to maintain the C-N ratios of the initial state
          cton_leaf <- this.individual$cmass_leaf/this.individual$nmass_leaf
          print("leaf")
          print(cton_leaf)
          print(this.individual$cmass_leaf)
          print(this.individual$nmass_leaf)
          cton_root <- this.individual$cmass_root/this.individual$nmass_root
          print("root")
          print(cton_root)
          print(this.individual$cmass_root)
          print(this.individual$nmass_root)
          cton_sap <- this.individual$cmass_sap/this.individual$nmass_sap
          print("sap")
          print(cton_sap)
          print(this.individual$cmass_sap)
          print(this.individual$nmass_sap)
          cton_heart <- this.individual$cmass_heart/this.individual$nmass_heart
          print("heart")
          print(cton_heart)
          print(this.individual$cmass_heart)
          print(this.individual$nmass_heart)
          
          
          
          # STEP 1 - nudge density of stems by adjusting the "indiv.densindiv" and also scaling the biomass pools appropriately
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$densindiv <- this.individual$densindiv * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_leaf <- this.individual$cmass_leaf * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_leaf <- this.individual$nmass_leaf * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_root <- this.individual$cmass_root * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_root <- this.individual$nmass_root * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_sap <- this.individual$cmass_sap * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_sap <- this.individual$nmass_sap * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_heart <- this.individual$cmass_heart * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_hear <- this.individual$nmass_heart * dens.rel.change[this.pft.id+1]
          
          # STEP 2 - nudge biomass by performing the LPJ-GUESS allocation routine
          
          # calculate the total biomass
          biomass.total <- this.individual$cmass_leaf+this.individual$cmass_root+this.individual$cmass_heart+this.individual$cmass_sap
          biomass.inc <- (biomass.total * biomass.rel.change[this.pft.id+1]) - biomass.total
          print(biomass.inc)
          
          cmass_root_inc <- 0
          cmass_sap_inc <- 0 
          cmass_debt_inc <- 0
          cmass_heart_inc <- 0
          litter_leaf_inc <- 0
          litter_root_inc <- 0
          exceeds_cmass <- 0
          
          # updated.pools <- allocation(bminc = as.numeric(biomass.inc),
          #                             cmass_leaf = as.numeric(this.individual$cmass_leaf),                                     ,
          #                             cmass_root = as.numeric(this.individual$cmass_sap),
          #                             cmass_sap = as.numeric(this.individual$cmass_sap),
          #                             cmass_debt  = as.numeric(this.individual$cmass_heart),
          #                             cmass_heart = as.numeric(this.individual$cmass_heart),
          #                             ltor = as.numeric(this.individual$ltor),
          #                             height = as.numeric(this.individual$height),
          #                             sla = as.numeric(this.individual$sla),
          #                             wooddens = as.numeric(this.individual$wooddens),
          #                             lifeform = as.integer(1), # BLARP
          #                             cmass_root_inc = as.numeric(cmass_root_inc),
          #                             cmass_sap_inc = as.numeric(cmass_sap_inc),
          #                             cmass_debt_inc = as.numeric(cmass_debt_inc),
          #                             cmass_heart_inc = as.numeric(cmass_heart_inc),
          #                             litter_leaf_inc = as.numeric(litter_leaf_inc),
          #                             litter_root_inc = as.numeric(litter_root_inc),
          #                             exceeds_cmass = as.numeric(exceeds_cmass))
          # print(updated.pools)
          
          
          # STEP 3 - adjust the various associated C pools based on the results of the previous step
          
          # STEP 4 - update N compartments using the initial C-N ratios 
          
          # STEP 5 - adjust the allometry of the individual based on the updated pools
          # QUESTION: what to do if allometry returns FALSE?
          
          
          
        }
        
      }
      
    }
    
  } # for each stand
  
  
  
  # STEP 6 - introduce new individuals to represent PFTs present in data but not in the model output
  
  return(model.state)
  
}

