##' @title AbvGrndWood
##'
##' @description Calculates the above-ground woody biomass of an LPJ-GUESS individual.
##'
##' @param individual A nested list representing an LPJ-GUESS individual from a binary state file.
##' @param include.debt Logical; if TRUE, includes carbon mass debt in the calculation.
##'
##' @return A numeric value representing the above-ground woody biomass (kgC/m²).
##' @keywords internal
AbvGrndWood <- function(individual, include.debt = TRUE){
  
  # get total wood
  if(include.debt) total.wood <- individual$cmass_sap + individual$cmass_heart - individual$cmass_debt
  else total.wood <- individual$cmass_sap + individual$cmass_heart
  
  # subtract below ground biomass
  # TODO add better allometry here
  above.ground.wood <- total.wood * (1-0.23)
  
  return(above.ground.wood)
  
}

##' @title TotalCarbon
##'
##' @description Calculates the total carbon content of an LPJ-GUESS individual.
##'
##' @param individual A nested list representing an LPJ-GUESS individual from a binary state file.
##' @param include.debt Logical; if TRUE, includes carbon mass debt in the calculation.
##'
##' @return A numeric value representing the total carbon content (kgC/m²).
##' @keywords internal
TotalCarbon <- function(individual, include.debt = TRUE){
  
  # get total wood
  if(include.debt) total.carbon <- individual$cmass_sap + individual$cmass_heart + individual$cmass_leaf + individual$cmass_root - individual$cmass_debt
  else total.carbon <- individual$cmass_sap + individual$cmass_heart + individual$cmass_leaf + individual$cmass_root
  
  return(total.carbon)
  
}



##' Adjust LPJ-GUESS state
##'
##' @title updateState.LPJGUESS
##'
##' @description Adjust LPJ-GUESS state variables based on input parameters.
##'
##'
##' @param model.state A large multiply-nested list containing the entire LPJ-GUESS state as read by 
##' function \code{readStateBinary.LPJGUESS} 
##' @param pft.params A data.frame containing the parameters for each PFT.  Each row represents one PFT (ordering must be consistent with the vectors below. 
##' The names of the columns describe the per-PFT parameter and must include: 
##' wooddens, crownarea_max, lifeform (1 = tree, 2 = grass), k_latosa, k_rp, k_allom1,  k_allom2, k_allom3, crownarea_max and sla. 
##' wooddens, crownarea_max, lifeform (1 = tree, 2 = grass), k_latosa, k_rp, k_allom1,  k_allom2, k_allom3, crownarea_max and sla. 
##' @param dens.initial A numeric vector of the initial stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX.  
##' @param dens.target A numeric vector of the target stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param AbvGrndWood.initial A numeric vector of the target stand-level above ground wood (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param AbvGrndWood.target A numeric vector of the target stand-level above ground wood (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param AbvGrndWood.epsilon A single numeric specifying how close the final above ground wood needs to be to the target
##' above ground stem biomass for each individual.  eg. 0.05 requires that the final above ground wood is within 5%
##' of the target above ground wood
##' @param trace Logical; if TRUE, prints detailed adjustment process information.
##' @param min.diam Minimum tree diameter (in cm) for inclusion in adjustments.
##' @param HEIGHT_MAX Maximum allowed height of an individual.  This is the maximum height that a tree
##' can have.  This is hard-coded in LPJ-GUESS to 150 m, but for SDA that might be unrealistically big, 
##' so this argument allows adjustment. 
##' @return  And updated model state (as a big old list o' lists)
##' @export update_state_LPJGUESS 
##' @author Matthew Forrest
update_state_LPJGUESS <- function(model.state, pft.params, dens.initial, dens.target, AbvGrndWood.initial, AbvGrndWood.target, AbvGrndWood.epsilon, trace, min.diam, HEIGHT_MAX = 150) {

  # calculate relative increases to be applied later on (per PFT)
  dens.rel.change <- dens.target/dens.initial
  AbvGrndWood.rel.change <- AbvGrndWood.target/AbvGrndWood.initial
  
  #print(dens.rel.change)
  #print(AbvGrndWood.rel.change)
  
  print(pft.params)
  
  
  # nstands - should always be 1 but lets make sure
  nstands <- unlist(model.state$nstands)
  if(nstands != 1) warning("More than one Stand found in LPJ-GUESS state.  This possibly implies that land use has been enabled
                           which the PEcAn code might not be robust against.")
  
  #
  for(stand.counter in 1:nstands) {
    
    # get the number of patches
    npatches <- model.state$Stand[[stand.counter]]$npatches
    
    # get list of all the PFTs included in this stand
    active.PFTs <- c()
    for(stand.pft.id in 1:length(model.state$Stand[[stand.counter]]$Standpft$active)) {
      if(model.state$Stand[[stand.counter]]$Standpft$active[[stand.pft.id]]) active.PFTs <- append(active.PFTs, stand.pft.id -1)
    }
    
    
    # loop through each patch
    for(patch.counter in 1:npatches) {
      
      this.patch <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]
      
      if(trace) {
        print("--------------------------------------------------------------------------------------------------")
        print(paste("-------------------------- STARTING PATCH", patch.counter, "------------------------------------------------------"))
        print(paste("-------------------------- NUMBER OF INDIVIDUALS =", length(this.patch$Vegetation$Individuals), "-------------------------------------------"))
        print("--------------------------------------------------------------------------------------------------")
      }
      
      
      # for each individual
      for(individual.counter in 1:length(this.patch$Vegetation$Individuals)) {
        
        
        # IMPORTANT: note that this is for convenience to *read* variables from the original individual 
        # but it should not be written to.  Instead the 'updated.individual' (defined in the loop below)
        # should be updated and then used to update the main state (model.state)
        original.individual <- this.patch$Vegetation$Individuals[[individual.counter]]
        
        # get the PFT id and check that it is active
        this.pft.id <- original.individual$indiv.pft.id
        pft.index <- this.pft.id + 1
        if(!this.pft.id %in% active.PFTs) stop(paste0("Found individual of PFT id = ",this.pft.id, 
                                                      " but this doesn't seem to be active in the LPJ-GUESS run"))
        
        # calculate its diameter to exclude small trees (converted to cm)
        diam = ((original.individual$height / pft.params[pft.index, "k_allom2"]) ^ (1.0 / pft.params[pft.index, "k_allom3"])) * 100
        
        # don't adjust non-alive individuals as they will soon be removed, 
        # also exclude small trees to help keep the adjustments sensible
        if(original.individual$alive & diam > min.diam) {
          
          
          # initialise the result code to "FIRST" for the first iteration
          result.code <- "FIRST"
          
          # get the initial, target and changes in Dens and AbvGrndWood
          initial.AbvGrndWood <- AbvGrndWood(original.individual)
          initial.Dens <- original.individual$densindiv          
          target.densindiv.rel.change <- dens.rel.change[pft.index]
          target.AbvGrndWood.rel.change <- AbvGrndWood.rel.change[pft.index]
          target.AbvGrndWood <- initial.AbvGrndWood * target.AbvGrndWood.rel.change
          
          if(trace) {
            print(paste(" * Adjusting individual", individual.counter))
            print(paste(" * PFT ID (zero-indexed) =", this.pft.id))
            print(paste(" * Initial AbvGrndWood value =", initial.AbvGrndWood))
            print(paste(" * Target AbvGrndWood value = ", target.AbvGrndWood))
            print(paste(" * Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
          }
          
          
          ####### STEP 0 - 'adjust the adjustment' - if the initial biomass adjustment is too crazy then tone it down here
          
          # if the biomass nudge is less that 0.75 the allocation will probably fail so increase the biomass
          # to 0.75 and increase the stem density accordingly
          if(target.AbvGrndWood.rel.change < 0.75) {
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            current.target.AbvGrndWood.rel.change <- 0.75
            current.target.densindiv.rel.change <- target.overall.rel.change / current.target.AbvGrndWood.rel.change 
            derived.overall.rel.change <- current.target.AbvGrndWood.rel.change * current.target.densindiv.rel.change
            
            if(trace) {
              print(paste(" ***** CHECK INITIAL ADJUSTMENTS"))
              print(paste(" ***** Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
              print(paste(" ***** Since Target AbvGrndWood relative change < 0.75, also adjust density"))
              print(paste(" ***** Modified target AbvGrndWood relative change =", current.target.AbvGrndWood.rel.change))
              print(paste(" ***** Modified target density relative change =", current.target.densindiv.rel.change))
              print(paste(" ***** Combined AbvGrndWood relative change =", derived.overall.rel.change))
            } # if trace
            
          } # if initial AbvGrndWood adjustment very low
          
          else if(target.AbvGrndWood.rel.change > 100) {
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            current.target.AbvGrndWood.rel.change <- 100
            current.target.densindiv.rel.change <- target.overall.rel.change / current.target.AbvGrndWood.rel.change 
            derived.overall.rel.change <- current.target.AbvGrndWood.rel.change * current.target.densindiv.rel.change
            
            if(trace) {
              print(paste(" ***** CHECK INITIAL ADJUSTMENTS"))
              print(paste(" ***** Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
              print(paste(" ***** Since Target AbvGrndWood relative change > 100, also adjust density"))
              print(paste(" ***** Modified target AbvGrndWood relative change =", current.target.AbvGrndWood.rel.change))
              print(paste(" ***** Modified target density relative change =", current.target.densindiv.rel.change))
              print(paste(" ***** Combined AbvGrndWood relative change =", derived.overall.rel.change))
            } # if trace
            
          } # if initial AbvGrndWood adjustment very high
          
          # AbvGrndWood nudge is safe to try without adjusting density
          else {
            current.target.AbvGrndWood.rel.change <- target.AbvGrndWood.rel.change
            current.target.densindiv.rel.change <- target.densindiv.rel.change
            
            if(trace) {
              print(paste(" ***** CHECK INITIAL ADJUSTMENTS"))
              print(paste(" ***** Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
              print(paste0(" ***** Initial nudge okay (rel change = ", target.AbvGrndWood.rel.change, "), no need to adjust density"))
            } # if trace
            
          } # if initial AbvGrndWood adjustment is not too crazy
          
          
          
          # STEP 1 - if necessary do an initial nudge density of stems by adjusting the "densindiv" 
          # and also scaling the biomass pools appropriately
          
          if(current.target.densindiv.rel.change != 1) {
            
            if(trace) {
              print(paste(" ------- BEFORE INITIAL DENSITY ADJUSTMENT -------"))
              print(paste(" ***** Density =", original.individual$densindiv))
              print(paste(" ***** AbvGrndWood =", AbvGrndWood(original.individual)))
            }
            
            updated.individual <- adjust.density.LPJGUESS(original.individual, current.target.densindiv.rel.change)
            final.densindiv <- updated.individual$densindiv
            
            if(trace) {
              print(paste(" ------- AFTER INITIAL DENSITY ADJUSTMENT -------"))
              print(paste(" ***** Density =", final.densindiv))
              print(paste(" ***** AbvGrndWood =", AbvGrndWood(updated.individual)))
            }
            if(updated.individual$densindiv != original.individual$densindiv * current.target.densindiv.rel.change) {
              stop(" ***** Density adjustment failed, this is suprising and confusing...")
            }
            
          }
          else {
            if(trace) {
              print(paste(" ------- NO INITIAL DENSITY ADJUSTMENT REQUIRED -------"))
            } # if trace
            
          } # if no density adjustment required
          
          
          # STEP 2 - nudge on biomass to get the right AbvGrndWood adjustment
          
          pre.nudge.TotalCarbon <- TotalCarbon(updated.individual)
          pre.nudge.AbvGrndWood <- AbvGrndWood(updated.individual)
          
          if(trace) {
            print(paste(" ------- BEFORE BIOMASS ADJUSTMENT -------"))
            print(paste(" ***** Pre-nudge Total Carbon =", pre.nudge.TotalCarbon))
            print(paste(" ***** Pre-nudge AbvGrndWood =", pre.nudge.AbvGrndWood))
            print(paste(" ***** Nudge target (relative) =", current.target.AbvGrndWood.rel.change))
          }
          
          # STEP 2A - nudge biomass by performing the iterative adjustments using LPJ-GUESS allocation routine
          current.AbvGrndWood <- pre.nudge.AbvGrndWood
          
          # always do one pass
          result.code <- "FIRST"
          counter <- 1
          while(result.code != "OK" & counter < 100) {
            
            print(paste(" -------------------------------------"))
            print(paste(" ------------ ITERATION", counter, "------------"))
            print(paste(" -------------------------------------"))
            
            is.first <- FALSE
            if(counter == 99) stop()
            
            # if nudge is already within the epsilon, adjust by that much biomass
            if(current.target.AbvGrndWood.rel.change > 1 - AbvGrndWood.epsilon & current.target.AbvGrndWood.rel.change < 1 + AbvGrndWood.epsilon) {
              this.biomass.inc <- pre.nudge.AbvGrndWood * (current.target.AbvGrndWood.rel.change - 1)
            }
            # else do a nudge by 5% of actual biomass
            else {
              if(current.AbvGrndWood < target.AbvGrndWood) {
                this.biomass.inc <- 0.1 * current.AbvGrndWood
              }
              else {
                this.biomass.inc <- -0.1 * current.AbvGrndWood
              }
            }
            
            
            if(trace) {
              print(paste(" ------- BEFORE BIOMASS ITERATION ", counter, "-------"))
              print(paste(" ***** Pre-nudge Total Carbon =", pre.nudge.TotalCarbon))
              print(paste(" ***** Pre-nudge AbvGrndWood =", pre.nudge.AbvGrndWood))
              print(paste(" ***** Overall AbvGrndWood Nudge target (relative) =", current.target.AbvGrndWood.rel.change))
              print(paste(" ***** cmass_sap =", updated.individual$cmass_sap))
              print(paste(" ***** cmass_heart =", updated.individual$cmass_heart))
              print(paste(" ***** cmass_leaf =", updated.individual$cmass_leaf))
              print(paste(" ***** cmass_root =", updated.individual$cmass_root))
              print(paste(" ***** cmass_debt =", updated.individual$cmass_debt))
            }
            
            
            # this function call runs the LPJ-GUESS allocation routine and adjusts the pools vegetation pools accordingly
            # however, it doesn't adjust the litter pools or do anything with 'exceeds_cmass', these are returned
            # as elements of the list, because they should only be applied to the state *if* this was a valid allocation
            updated.list <- adjust.biomass.LPJGUESS(individual = updated.individual, 
                                                    biomass.inc = this.biomass.inc,  
                                                    sla = pft.params[pft.index, "sla"], 
                                                    wooddens = pft.params[pft.index, "wooddens"], 
                                                    lifeform = pft.params[pft.index, "lifeform"], 
                                                    k_latosa = pft.params[pft.index, "k_latosa"], 
                                                    k_allom2 = pft.params[pft.index, "k_allom2"], 
                                                    k_allom3 = pft.params[pft.index, "k_allom3"])
            # extract the elements from the return list
            updated.individual <- updated.list[["individual"]]
            litter_root_inc <- updated.list[["litter_root_inc"]]
            litter_leaf_inc <- updated.list[["litter_leaf_inc"]]
            exceeds_cmass <- updated.list[["exceeds_cmass"]]
            rm(updated.list)
            
            # calculate the final AbvGrndWood and TotalCarbon
            post.nudge.AbvGrndWood <- AbvGrndWood(updated.individual)
            post.nudge.TotalCarbon <- TotalCarbon(updated.individual)
            
            
            if(trace) {
              print(paste(" ------- AFTER BIOMASS INTERATION ", counter, "-------"))
              print(paste(" ***** Post-nudge Total Carbon =", post.nudge.TotalCarbon))
              print(paste(" ***** Post-nudge AbvGrndWood =", post.nudge.AbvGrndWood))
              print(paste(" ***** Total Carbon Nudge achieved  (relative) =", post.nudge.TotalCarbon/pre.nudge.TotalCarbon))
              print(paste(" ***** AbvGrndWood Nudge achieved  (relative) =", post.nudge.AbvGrndWood/pre.nudge.AbvGrndWood))
              print(paste(" ***** Overall AbvGrndWood Nudge target (relative) =", current.target.AbvGrndWood.rel.change))
              print(paste(" ***** cmass_sap =", updated.individual$cmass_sap))
              print(paste(" ***** cmass_heart =", updated.individual$cmass_heart))
              print(paste(" ***** cmass_leaf =", updated.individual$cmass_leaf))
              print(paste(" ***** cmass_root =", updated.individual$cmass_root))
              print(paste(" ***** cmass_debt =", updated.individual$cmass_debt))
              print(paste(" ***** exceeds_cmass =", exceeds_cmass))
            }
            
            current.AbvGrndWood <- AbvGrndWood(updated.individual)
           
          # STEP 2B - calculate the new allometry of the individual based on the updated pools
          
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
            lifeform = pft.params[pft.index, "lifeform"], 
            sla = pft.params[pft.index, "sla"], 
            k_latosa = pft.params[pft.index, "k_latosa"], 
            k_rp = pft.params[pft.index, "k_rp"],
            k_allom1 = pft.params[pft.index, "k_allom1"],
            k_allom2 = pft.params[pft.index, "k_allom2"], 
            k_allom3 = pft.params[pft.index, "k_allom3"], 
            wooddens = pft.params[pft.index, "wooddens"],
            crownarea_max = pft.params[pft.index, "crownarea_max"], 
            HEIGHT_MAX = HEIGHT_MAX) 
          
          
          # STEP 3 - check if new allometry is valid. If yes, update state and move on,
          # if not adjust the nudging and start again
          result.code <- allometry.results$error.string
          
          
          # if allometry is valid check if we have converged on the desired AbvGrndWood
          if(result.code == "OK") {
            if(abs(current.AbvGrndWood-target.AbvGrndWood)/target.AbvGrndWood > AbvGrndWood.epsilon) result.code <- "NOTCONVERGED"
          }
          
          
          # right now just accept the neglibable leaf mass error
          if(result.code == "NegligibleLeafMass"){
            result.code <- "OK"
          }
          
          # this error normally arises because of a too large negative biomass increment, do here set a softer
          # biomass nudge
          if(result.code == "LowWoodDensity"){
            
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            print(target.overall.rel.change)
            current.target.AbvGrndWood.rel.change <- 1.1 * current.target.AbvGrndWood.rel.change
            current.target.densindiv.rel.change <- target.overall.rel.change / current.target.AbvGrndWood.rel.change 
            derived.overall.rel.change <- current.target.AbvGrndWood.rel.change * current.target.densindiv.rel.change
            print(derived.overall.rel.change)
            
          }
          # in the case individuals (as in each actual tree) get too big, so increase the individual density, 
          # but this needs to be balanced by the a smaller biomass nudge (do 10% increments)
          else if(result.code == "MaxHeightExceeded"){
            
            current.target.densindiv.rel.change <-  current.target.densindiv.rel.change * 1.1
            current.target.AbvGrndWood.rel.change <- current.target.AbvGrndWood.rel.change / 1.1
            
            if(trace) {
              print(paste(" ***** ERROR CODE =", result.code))
              print(paste(" ***** Since maximum height exceeded, also adjust density"))
              print(paste(" ***** Modified target AbvGrndWood relative change =", current.target.AbvGrndWood.rel.change))
              print(paste(" ***** Modified target density relative change =", current.target.densindiv.rel.change))
              print(paste(" ***** Combined AbvGrndWood relative change =", current.target.AbvGrndWood.rel.change * current.target.densindiv.rel.change))
            }
          }
          else if(result.code == "NegligibleLeafMass"){
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            print(target.overall.rel.change)
            current.target.AbvGrndWood.rel.change <- 0.75
            current.target.densindiv.rel.change <- target.overall.rel.change / current.target.AbvGrndWood.rel.change 
            derived.overall.rel.change <- current.target.AbvGrndWood.rel.change * current.target.densindiv.rel.change
            print(derived.overall.rel.change)
          }
          
          # finally if "OK"
          else if(result.code == "OK") {
            
            if(trace) {
              
              final.AbvGrndWood <- current.AbvGrndWood
              final.Dens <- updated.individual$densindiv
              
              print(paste(" -------------------------------------------------"))
              print(paste(" ------------ NUDGE SUCCESSFUL!!! ", counter, "------------"))
              print(paste(" -------------------------------------------------"))

              print(paste(" ***** Initial AbvGrndWood", initial.AbvGrndWood))
              print(paste(" ***** Target AbvGrndWood", target.AbvGrndWood))
              print(paste(" ***** Final AbvGrndWood", final.AbvGrndWood))
              print(paste(" ***** Relative AbvGrndWood nudge", final.AbvGrndWood/initial.AbvGrndWood))
              print(paste(" ***** Initial Dens", initial.Dens))
              print(paste(" ***** Final Dens", final.Dens))
              print(paste(" ***** Relative Dens nudge", final.Dens/initial.Dens))
              print(paste(" ***** Overall Applied AbvGrndWood nudge", final.Dens/initial.Dens * final.AbvGrndWood/initial.AbvGrndWood))
              
            }
            
            
            
            print("--------------------------------------------")
            print(paste("nudge = ", current.target.AbvGrndWood.rel.change ))
            print(initial.AbvGrndWood)
            print(target.AbvGrndWood)
            
            # check if the above ground stem biomass is acceptably close
            # if((ag.wood.after - target.AbvGrndWood)/ target.AbvGrndWood > AbvGrndWood.epsilon) {
            #   #result.code <- "AGSBTooBig"
            #   result.code <- "OK"
            # }
            # else if((ag.wood.after - target.AbvGrndWood)/ target.AbvGrndWood < -AbvGrndWood.epsilon) {
            #   #result.code <- "AGSBTooSmall"
            #   result.code <- "OK"
            # }
            
            # okay, allocation produces AGSB acceptables close to the target so update the allometry, 
            # save the individual back to the state, update the litter pools, 
            # deal with exceeds_cmass, and the code will break out of the while loop
            # if not, there will be a new iteration of allocation with new multipliers
            #else {
            # first update the allometry
            updated.individual$height <- allometry.results$height
            updated.individual$crownarea <- allometry.results$crownarea
            updated.individual$lai_indiv <- allometry.results$lai_indiv
            updated.individual$lai <- allometry.results$lai
            updated.individual$deltafpc <- allometry.results$deltafpc
            updated.individual$fpc <- allometry.results$fpc
            updated.individual$boleht <- allometry.results$boleht
            
            # save the individual back to the state
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
            
            # now the litter pools (determine N based on intial C:N ratio)
            # C:N ratios
            leaf_litter_cton <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]] / model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_leaf[[pft.index]]
            root_litter_cton <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]] / model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_root[[pft.index]]
            # update the C pools based on the calculated increments from the allocation call (these will only be non-zero in 'abnormal cases)
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]] + (litter_leaf_inc * updated.individual$densindiv) 
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]] + (litter_root_inc * updated.individual$densindiv) 
            # update the N pools simple by dividing the new C pool by the C:N ratio
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_leaf[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]] / leaf_litter_cton
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_root[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]] / root_litter_cton
            
            # and finally exceeds_cmass - not currently dealing with this because it is only used to maintin mass balance which
            # we *probably* don't need to do here, but print a warning if it is non-zero
            if(!exceeds_cmass == 0) warning(paste("Non-zero exceeds_cmass following allocation, exceeds_cmass =", exceeds_cmass))
            
           } # if allometry valid (check result.code)
          
          counter <- counter + 1
          
        } # while code is not "OK"
        
      } # if individual is alive
      
    } # for each individual
    
  } # for each patch
  
} # for each stand



# TODO MISSING - introduce new individuals to represent PFTs present in data but not in the model output

return(model.state)

}

