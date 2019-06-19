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
  #print(dens.rel.change)
  #print(biomass.rel.change)
  
  
  # hard coded veg parameters for testing
  wooddens <- rep(200, 11)
  lifeform <- c(1,1,1,1,1,1,1,1,1,2,2)
  k_latosa <- c(5000, 5000,  5000, 6000, 6000,  6000, 6000,6000, 6000, 6000, 6000)
  k_allom2 <- rep(60, 11)
  k_allom3 <- rep(0.67, 11)
  leaflong <- c()
  leafphysiognomy <- c()
  
  # get proper SLA
  leaflong <- c(3, 3, 0.5, 0.5, 0.5, 3, 2, 2, 0.5, 0.5, 0.5)
  leafphysiognomy <- c("NEEDLELEAF", "NEEDLELEAF",  "NEEDLELEAF", "BROADLEAF", "BROADLEAF",  "BROADLEAF", "BROADLEAF","BROADLEAF", "BROADLEAF", "BROADLEAF", "BROADLEAF")
  sla <- c()
  getSLA <- function(leaflong, leafphysiognomy) {
    if (leafphysiognomy == "BROADLEAF") {
      sla = 0.2 * 10.0^(2.41 - 0.38 * log10(12.0 * leaflong))
    }
    else if (leafphysiognomy == "NEEDLELEAF") {
      sla = 0.2 * 10.0^(2.29 - 0.4 * log10(12.0 * leaflong))
    }
    return(sla)
  }
  
  for(temp_counter in 1:length(leaflong)){
    sla <- append(sla, getSLA(leaflong[temp_counter], leafphysiognomy[temp_counter]))
  }
  
  
  
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
      
      
      # for each individual
      for(individual.counter in 1:this.patch$Vegetation$number_of_individuals) {
        
        # IMPORTANT: note that this is for convenience to *read* variables from an individual but not write to
        # it also needs to be updated after the main state (model.state) is updated
        this.individual <- this.patch$Vegetation$Individuals[[individual.counter]]
        
        if(this.individual$alive) {
          
          this.pft.id <- this.individual$indiv.pft.id
          #print(paste("PFT id = ", this.pft.id))
          
          if(!this.pft.id %in% active.PFTs) stop(paste0("Found individual of PFT id = ",this.pft.id, 
                                                        " but this doesn't seem to be active in the LPJ-GUESS run"))
          
          
          # STEP 1 - nudge density of stems by adjusting the "indiv.densindiv" and also scaling the biomass pools appropriately
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$densindiv <- this.individual$densindiv * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_leaf <- this.individual$cmass_leaf * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_leaf <- this.individual$nmass_leaf * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_root <- this.individual$cmass_root * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_root <- this.individual$nmass_root * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_sap <- this.individual$cmass_sap * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_sap <- this.individual$nmass_sap * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_heart <- this.individual$cmass_heart * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_heart <- this.individual$nmass_heart * dens.rel.change[this.pft.id+1]
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_debt <- this.individual$cmass_debt * dens.rel.change[this.pft.id+1]
          
          # IMPORTANT: update the this.individual to include the density adjustments above
          this.individual <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]
          
          
          # STEP 2 - nudge biomass by performing the LPJ-GUESS allocation routine
          
          # calculate the total biomass and the absolute change based on this
          biomass.total <- this.individual$cmass_leaf+this.individual$cmass_root+this.individual$cmass_heart+this.individual$cmass_sap-this.individual$cmass_debt
          biomass.inc <- (biomass.total * biomass.rel.change[this.pft.id+1]) - biomass.total
          
          cmass_leaf_inc <- 0
          cmass_root_inc <- 0
          cmass_sap_inc <- 0 
          cmass_debt_inc <- 0
          cmass_heart_inc <- 0
          litter_leaf_inc <- 0
          litter_root_inc <- 0
          exceeds_cmass <- 0
          
          
          #print(this.individual$cmass_leaf/)
          
          updated.pools <- allocation(
            # vegetation state
            bminc = as.numeric(biomass.inc/this.individual$densindiv),
            cmass_leaf = as.numeric(this.individual$cmass_leaf/this.individual$densindiv),              
            cmass_root = as.numeric(this.individual$cmass_root/this.individual$densindiv),
            cmass_sap = as.numeric(this.individual$cmass_sap/this.individual$densindiv),
            cmass_debt  = as.numeric(this.individual$cmass_debt/this.individual$densindiv),
            cmass_heart = as.numeric(this.individual$cmass_heart/this.individual$densindiv),
            ltor = as.numeric(this.individual$ltor),
            height = as.numeric(this.individual$height),
            # PFT parameters
            sla = as.numeric(sla[this.pft.id+1]),
            wooddens = as.numeric(wooddens[this.pft.id+1]),
            lifeform = as.integer(lifeform[this.pft.id+1]), # BLARP
            k_latosa = as.numeric(k_latosa[this.pft.id+1]),
            k_allom2 = as.numeric(k_allom2[this.pft.id+1]),
            k_allom3 = as.numeric(k_allom3[this.pft.id+1]),
            # calculated increments (not actually used, see values returned in the updated.pools list)
            cmass_leaf_inc = as.numeric(cmass_leaf_inc),
            cmass_root_inc = as.numeric(cmass_root_inc),
            cmass_sap_inc = as.numeric(cmass_sap_inc),
            cmass_debt_inc = as.numeric(cmass_debt_inc),
            cmass_heart_inc = as.numeric(cmass_heart_inc),
            litter_leaf_inc = as.numeric(litter_leaf_inc),
            litter_root_inc = as.numeric(litter_root_inc),
            exceeds_cmass = as.numeric(exceeds_cmass))
          
          
          # STEP 3 - adjust the various associated C (and N) pools based on the results of the previous step
          
          # leaf
          original.cmass_leaf <- this.individual$cmass_leaf
          new.cmass_leaf <- this.individual$cmass_leaf + (updated.pools[["cmass_leaf_inc"]] * this.individual$densindiv)
          leaf.scaling <- new.cmass_leaf / original.cmass_leaf
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_leaf <- new.cmass_leaf
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_leaf <- this.individual$nmass_leaf * leaf.scaling
          
          # root
          original.cmass_root <- this.individual$cmass_root
          new.cmass_root <- this.individual$cmass_root + (updated.pools[["cmass_root_inc"]] * this.individual$densindiv)
          root.scaling <- new.cmass_root / original.cmass_root
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_root <- new.cmass_root
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_root <- this.individual$nmass_root * root.scaling
          
          # sap
          original.cmass_sap <- this.individual$cmass_sap
          new.cmass_sap <- this.individual$cmass_sap + (updated.pools[["cmass_sap_inc"]] * this.individual$densindiv)
          sap.scaling <- new.cmass_sap / original.cmass_sap
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_sap <- new.cmass_sap
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_sap <- this.individual$nmass_sap * sap.scaling
          
          
          # heart
          original.cmass_heart <- this.individual$cmass_heart
          new.cmass_heart <- this.individual$cmass_heart + (updated.pools[["cmass_heart_inc"]] * this.individual$densindiv)
          heart.scaling <- new.cmass_heart / original.cmass_heart
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_heart <- new.cmass_heart
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$nmass_heart <- this.individual$nmass_heart * heart.scaling
          
          # debt - no equivalant n debt
          original.cmass_debt <- this.individual$cmass_debt
          new.cmass_debt <- this.individual$cmass_debt + (updated.pools[["cmass_debt_inc"]] * this.individual$densindiv)
          model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]$cmass_debt <- new.cmass_debt
          
          # IMPORTANT: update the this.individual to after biomass changes above before the checks
          this.individual <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]]
          
          # checks
          if(FALSE) {
            
            biomass.final <- this.individual$cmass_leaf+this.individual$cmass_root+this.individual$cmass_heart+this.individual$cmass_sap-this.individual$cmass_debt
            if(abs((biomass.final/biomass.total) - 1.1) < 0.001) {
              print("--- okay ---")
            }
            else {
              print("--- not okay ---")
            }
            print(this.individual$indiv.pft.id)
            lifeform[this.individual$indiv.pft.id+1]
            print(biomass.final/biomass.total)
            print(unlist(updated.pools))
            print("--- end ---")
          }
          
          
          # STEP 5 - adjust the allometry of the individual based on the updated pools
          # QUESTION: what to do if allometry returns FALSE?
          
          
          
        }
        
      }
      
    }
    
  } # for each stand
  
  
  
  # STEP 6 - introduce new individuals to represent PFTs present in data but not in the model output
  
  return(model.state)
  
}

