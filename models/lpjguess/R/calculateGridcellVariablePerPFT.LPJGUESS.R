#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##' @title calculateGridcellVariablePerPFT
##'
##' @description Calculates a per-PFT, gridcell-summed quantity from the LPJ-GUESS state, correctly averaging over patches.
##' This should be put into the SDA procedure. 
##'
##'
##' @param model.state A large multiply-nested list containing the entire LPJ-GUESS state as read by 
##' function \code{readStateBinary.LPJGUESS} 
##' @param variable A character string specifying what variable to extract.  This can be chosen based on the LPJ-GUESS variable name
##' as recorded in the big list of list (that represents describes the model state in R).  Once special case is "biomass" which
##' returns the sum of "cmass_leaf", "cmass_root", "cmass_sap" and "cmass_heart"  
##' @return  A numeric vector, with one entry per PFT
##' @export
##' @author Matthew Forrest
calculateGridcellVariablePerPFT <- function(model.state, variable) {
  
  # nstands - should always be 1 but lets make sure
  nstands <- unlist(model.state$nstands)
  if(nstands != 1) warning("More than one Stand found in LPJ-GUESS state.  This possibly implies that land use has been enabled
                         which the PEcAn code might not be robust against.")
  
  #
  for(stand.counter in 1:nstands) {
    
    # get the number of patches for weighting across patches
    npatches <- model.state$Stand[[stand.counter]]$npatches
    
    # get list of all the PFTs included in this stand
    active.PFTs <- c()
    for(stand.pft.id in 1:length(model.state$Stand[[stand.counter]]$Standpft$active)) {
      if(model.state$Stand[[stand.counter]]$Standpft$active[[stand.pft.id]]) active.PFTs <- append(active.PFTs, stand.pft.id -1)
    }
    
    
    # arrays to store the aggregated gridcell level properties
    gc.sum <- numeric(length(model.state$Stand[[stand.counter]]$Standpft$active))
   
    # loop through each patch
    print(length(model.state$Stand[[stand.counter]]$Patch))
    for(patch.counter in 1:npatches) {

      
      print("-------------------------------------------------------------------------------------")
      print(paste("--------------------------------  PATCH ", patch.counter, " -------------------------------------"))
      print("-------------------------------------------------------------------------------------")
      
      
      this.patch <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]

      # pull out the number of individuals and a list of them   
      nindividuals <- length(this.patch$Vegetation)
      all.individuals <- this.patch$Vegetation$Individuals
      
      # for each individual
      for(individual.counter in 1:length(all.individuals)) {
        this.individual <- all.individuals[[individual.counter]]
        
        # print(paste("id = ", this.individual$indiv.pft.id))
        # print(paste("leaf =" , this.individual$cmass_leaf))
        # print(paste("root =" , this.individual$cmass_root))
        # print(paste("sap =" , this.individual$cmass_sap))
        # print(paste("heart =" , this.individual$cmass_heart)) 
        # print(paste("debt =" , this.individual$cmass_debt)) 
        print(paste("alive =" , this.individual$alive)) 
        print(individual.counter)
      
        if(this.individual$alive) {
          
          # get the PFT ID
          this.pft.id <- this.individual$indiv.pft.id
          if(!this.pft.id %in% active.PFTs) stop(paste0("Found individual of PFT id = ",this.pft.id, 
                                                        " but this doesn't seem to be active in the LPJ-GUESS run"))
          
          # covert the PFT from '0-indexed' C++ style to '1-indexed' R style
          pft.index <- this.pft.id+1
          
          # calculate the total cmass and density of individuals per PFT
          if(variable == "cmass") {
            gc.sum[pft.index] <- gc.sum[pft.index] + (this.individual$cmass_leaf+this.individual$cmass_root+this.individual$cmass_heart+this.individual$cmass_sap-this.individual$cmass_debt)/npatches
            
            
            print(paste("id = ", this.individual$indiv.pft.id))
            print(paste("leaf =" , this.individual$cmass_leaf))
            print(paste("root =" , this.individual$cmass_root))
            print(paste("sap =" , this.individual$cmass_sap))
            print(paste("heart =" , this.individual$cmass_heart)) 
            print(paste("debt =" , this.individual$cmass_debt)) 
            
            print(gc.sum) 
           
            
         }
          else if(variable == "nmass") {
            gc.sum[pft.index] <- gc.sum[pft.index] + ((this.individual$nmass_leaf+this.individual$nmass_root+this.individual$nmass_heart+
                                                                 this.individual$nmass_sap+this.individual$nstore_labile+this.individual$nstore_longterm)/npatches)
            #gc.sum[pft.index] <- gc.sum[pft.index] + ((this.individual$nmass_leaf+this.individual$nmass_root+this.individual$nmass_heart+
            #                                                     this.individual$nmass_sap)/npatches)
            
            
            #print(paste("leaf =" , this.individual$nmass_leaf))
            #print(paste("root =" , this.individual$nmass_root))
            #print(paste("sap =" , this.individual$nmass_sap))
            #print(paste("heart =" , this.individual$nmass_heart))
            
          }
          else  gc.sum[pft.index] <- gc.sum[pft.index] + (this.individual[[variable]]/npatches)
          
        }
        
      }
      
      
      
    }
    
    
   
    
  } 
  
  return(gc.sum)
  
  
}


