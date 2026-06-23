##' @title calculateGridcellSOC
##'
##' @description Calculates soil carbon
##' This should be put into the SDA procedure. 
##'
##'
##' @param model.state A large multiply-nested list containing the entire LPJ-GUESS state as read by 
##' function \code{readStateBinary.LPJGUESS} 
##' @param pools "SOILSTRUCT","SOILMETA","SOILMICRO","SLOWSOM","PASSIVESOM" (see somdynam.cpp)
##' @return The total SOC in Gridcell. The unit is consistent with cmass in state, usually kg C m-2
##' @export
##' @author Yinghao Sun
calculateGridcellSOC <- function(model.state,
                                 pools = c("SOILSTRUCT","SOILMETA","SOILMICRO","SLOWSOM","PASSIVESOM")) {
  
  # Find NATURAL stands and normalize it to frac weighting
  nstands <- unlist(model.state$nstands); if(length(nstands)==0) nstands <- length(model.state$Stand)
  nat_idx <- integer(0); nat_frac <- numeric(0)
  for (s in 1:nstands) {
    st <- model.state$Stand[[s]]
    lc <- st$landcovertype
    if (!is.null(lc) && as.integer(lc) == 4) {   # 4 == NATURAL
      fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
      if (fr > 0) { nat_idx <- c(nat_idx, s); nat_frac <- c(nat_frac, fr) }
    }
  }
  if (length(nat_idx) == 0) return(0)
  weight <- nat_frac / sum(nat_frac)
  
  # 2=SOILSTRUCT, 3=SOILMICRO, 9=SOILMETA, 10=SLOWSOM, 11=PASSIVESOM (see somdynam.cpp)
  soc_pool_names <- c(
    "SURFSTRUCT","SOILSTRUCT","SOILMICRO","SURFHUMUS","SURFMICRO",
    "SURFMETA","SURFFWD","SURFCWD","SOILMETA","SLOWSOM","PASSIVESOM","LEACHED"
  )
  first_cm <- model.state$Stand[[nat_idx[1]]]$Patch[[1]]$Soil$Sompool$cmass
  if (!is.null(names(first_cm)) && all(pools %in% names(first_cm))) {
    sel <- match(pools, names(first_cm))
  } else {
    sel <- c(2L,3L,9L,10L,11L)
  }
  
  soc_gc <- 0
  for (k in seq_along(nat_idx)) {
    st <- model.state$Stand[[nat_idx[k]]]
    np <- st$npatches
    if (np <= 0) next
    
    # Average within the patch, then multiply by the stand area weight
    soc_patch <- numeric(np)
    for (p in seq_len(np)) {
      cm <- st$Patch[[p]]$Soil$Sompool$cmass
      soc_patch[p] <- sum(vapply(sel, function(j) as.numeric(cm[[j]]), 0.0))
    }
    soc_gc <- soc_gc + weight[k] * mean(soc_patch, na.rm = TRUE)
  }
  soc_gc
}


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
##' @param pft.params A data frame containing PFT parameters such as allometric coefficients.
##' @param min.diam Minimum tree diameter (in cm) required for inclusion in calculations.
##' @return  A numeric vector, with one entry per PFT
##' @export
##' @author Matthew Forrest
calculateGridcellVariablePerPFT <- function(model.state, variable, pft.params, min.diam = 5) {
  # number of stands (can be >1 when land use is enabled)
  nstands <- unlist(model.state$nstands)
  if(length(nstands)==0) nstands <- length(model.state$Stand)
  if(nstands < 1) stop("No Stand found in LPJ-GUESS state.")
  
  # We'll initialize total gridcell sum after we know PFT length from the first stand
  # Assume the active PFT layout is consistent across stands in a gridcell
  first_active_len <- length(model.state$Stand[[1]]$Standpft$active)
  gc.sum.total <- numeric(first_active_len)
  
  nat_idx <- integer(0)
  nat_frac <- numeric(0)
  for (s in 1:nstands) {
    st <- model.state$Stand[[s]]
    lc <- st$landcovertype
    if (!is.null(lc) && as.integer(lc) == 4) {             # 4 == NATURAL
      fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
      if (fr > 0) {
        nat_idx  <- c(nat_idx, s)
        nat_frac <- c(nat_frac, fr)
      }
    }
  }

  # no natural stands
  if (length(nat_idx) == 0) return(gc.sum.total)           
  # weights: normalized within NATURAL
  weight <- nat_frac / sum(nat_frac)
  
  # loop over stands
  for(k in seq_along(nat_idx)) {
    stand.counter <- nat_idx[k]
    stand <- model.state$Stand[[stand.counter]]
   
    # get the number of patches for weighting across patches
    npatches <- stand$npatches
    
    # get list of all the PFTs included in this stand
    active.PFTs <- c()
    for(stand.pft.id in 1:length(stand$Standpft$active)) {
      if(stand$Standpft$active[[stand.pft.id]]) active.PFTs <- append(active.PFTs, stand.pft.id - 1)
    }
    
    # per-stand accumulator (patch-mean inside, then * frac)
    gc.sum.stand <- numeric(length(stand$Standpft$active))
    
    # loop through each patch
    for(patch.counter in 1:npatches) {
      
      this.patch <- stand$Patch[[patch.counter]]
      
      # pull out the individuals in this patch
      all.individuals <- this.patch$Vegetation$Individuals
      if(length(all.individuals) == 0) next
      
      # for each individual
      for(individual.counter in 1:length(all.individuals)) {
        this.individual <- all.individuals[[individual.counter]]
        
        # get PFT index and convert from 0-indexed (C++) to 1-indexed (R)
        this.pft.id <- this.individual$indiv.pft.id
        pft.index <- this.pft.id + 1
        
        # calculate diameter (cm) to exclude small trees
        diam <- ((this.individual$height / pft.params[pft.index, "k_allom2"]) ^ (1.0 / pft.params[pft.index, "k_allom3"])) * 100
        
        # basic sanity (keep your original debug guard)
        if (!is.finite(diam)) {
          message("NaN diam: pft=", this.pft.id,
                  " stand=", stand.counter,
                  " patch=", patch.counter,
                  " indiv=", individual.counter,
                  " h=", this.individual$height,
                  " sap=", this.individual$cmass_sap,
                  " heart=", this.individual$cmass_heart,
                  " leaf=", this.individual$cmass_leaf,
                  " root=", this.individual$cmass_root,
                  " SLA=", pft.params[pft.index, "sla"],
                  " k2=", pft.params[pft.index, "k_allom2"],
                  " k3=", pft.params[pft.index, "k_allom3"],
                  " alive=", this.individual$alive)
        }
        
        if(this.individual$alive && diam >= min.diam) {
          
          # ensure this PFT is active in the run
          if(!this.pft.id %in% active.PFTs)
            stop(paste0("Found individual of PFT id = ", this.pft.id, 
                        " but this doesn't seem to be active in the LPJ-GUESS run"))
          
          # accumulate per-PFT quantity (patch-mean: divide by npatches as in your original)
          if(variable == "cmass") {
            # gc.sum.stand[pft.index] <- gc.sum.stand[pft.index] +
            #   (this.individual$cmass_leaf + this.individual$cmass_root +
            #      this.individual$cmass_heart + this.individual$cmass_sap -
            #      this.individual$cmass_debt) / npatches
            gc.sum.stand[pft.index] <- gc.sum.stand[pft.index] +
              (this.individual$cmass_veg) / npatches
          } else if(variable == "nmass") {
            gc.sum.stand[pft.index] <- gc.sum.stand[pft.index] +
              (this.individual$nmass_leaf + this.individual$nmass_root +
                 this.individual$nmass_heart + this.individual$nmass_sap +
                 this.individual$nstore_labile + this.individual$nstore_longterm) / npatches
          } else if(variable == "AbvGrndWood") {
            gc.sum.stand[pft.index] <- gc.sum.stand[pft.index] +
              AbvGrndWood(this.individual) / npatches
          } else {
            gc.sum.stand[pft.index] <- gc.sum.stand[pft.index] +
              (this.individual[[variable]] / npatches)
          }
        }
      } # end individuals
    }   # end patches
    
    # gridcell aggregation: add this stand weighted by its area fraction
    gc.sum.total <- gc.sum.total + weight[k] * gc.sum.stand
    
  } # end stands
  
  return(gc.sum.total)
}
