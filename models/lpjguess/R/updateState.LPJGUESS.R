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
  crownarea_max <- rep(50, 11)
  lifeform <- c(1,1,1,1,1,1,1,1,1,2,2)
  k_latosa <- c(5000, 5000, 5000, 6000, 6000,  6000, 6000,6000, 6000, 6000, 6000)
  k_rp <- rep(1.6, 11)
  k_allom1 <-  c(150, 150, 150, 250, 250, 250, 250, 250, 250, 250, 250)
  k_allom2 <- rep(60, 11)
  k_allom3 <- rep(0.67, 11)
  
  # calculate SLA using leaf longevity and leaf physiognomy
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
        
        # IMPORTANT: note that this is for convenience to *read* variables from the original individual 
        # but it should not be written to.  Instead the 'updated.individual' (defined in the loop below)
        # should be updated and then used to update the main state (model.state)
        original.individual <- this.patch$Vegetation$Individuals[[individual.counter]]
        
        # don't adjust non-alive individuals as they will soon be removed
        if(original.individual$alive) {
          
          this.pft.id <- original.individual$indiv.pft.id
          #print(paste("PFT id = ", this.pft.id))
          
          if(!this.pft.id %in% active.PFTs) stop(paste0("Found individual of PFT id = ",this.pft.id, 
                                                        " but this doesn't seem to be active in the LPJ-GUESS run"))
          
          
          # STEP 1 - nudge density of stems by adjusting the "densindiv" and also scaling the biomass pools appropriately
          updated.individual <- adjustDensity.LPJGUESS(original.individual, dens.rel.change[this.pft.id+1])
          
          
          # STEP 2 - nudge biomass by performing the LPJ-GUESS allocation routine
          
          # calculate the total biomass (after the densindiv nudging above) and the absolute change based on this
          biomass.total <- updated.individual$cmass_leaf+updated.individual$cmass_root+updated.individual$cmass_heart+updated.individual$cmass_sap-updated.individual$cmass_debt
          biomass.inc <- (biomass.total * biomass.rel.change[this.pft.id+1]) - biomass.total
          
          # this function call runs the LPJ-GUESS allocation routine and ajusts the pools accordingly
          updated.individual <- adjustBiomass(individual = updated.individual, 
                                              biomass.increment = biomass.inc,  
                                              sla = sla[this.pft.id+1], 
                                              wooddens = wooddens[this.pft.id+1], 
                                              lifeform = lifeform[this.pft.id+1], 
                                              k_latosa = k_latosa[this.pft.id+1], 
                                              k_allom2 = k_allom2[this.pft.id+1], 
                                              k_allom3 = k_allom3[this.pft.id+1])
          
          
          
          # STEP 3 - adjust the allometry of the individual based on the updated pools
          # QUESTION: what to do if allometry returns FALSE?
          
          allometry.results <- allometry(
            # initial allometry/pools
            cmass_leaf = updated.individual$cmass_leaf, 
            cmass_sap = updated.individual$cmass_sap, 
            cmass_heart = updated.individual$cmass_heart, 
            densindiv = updated.individual$densindiv, 
            age = updated.individual$age, 
            fpc = updated.individual$fpc,
            deltafpc = updated.individual$deltafpc,
            # parameter values
            lifeform = lifeform[this.pft.id+1], 
            sla = sla[this.pft.id+1], 
            k_latosa = k_latosa[this.pft.id+1], 
            k_rp = k_rp[this.pft.id+1],
            k_allom1 = k_allom1[this.pft.id+1],
            k_allom2 = k_allom2[this.pft.id+1], 
            k_allom3 = k_allom3[this.pft.id+1], 
            wooddens = wooddens[this.pft.id+1],
            crownarea_max = crownarea_max[this.pft.id+1]) 
          
          # if not okay print a warning, and should actually start another iteration with new multipliers
          if(allometry.results$error.string != "OK") {
            print(allometry.results$error.string)
          }
          # else update the individual, the litter pools and break
          else {
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
          }
          
          
        }
        
      }
      
    }
    
  } # for each stand
  
  
  
  # STEP 6 - introduce new individuals to represent PFTs present in data but not in the model output
  
  return(model.state)
  
}

