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
  above.ground.wood <- total.wood
  
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

##' @title Pick the patch with the largest canopy gap
##'
##' @description Selects a patch index within a stand that has the largest gap
##' (defined as 1 - sum(FPC)). Empty patches are treated as gap = 1 and preferred.
##'
##' @param stand A list representing an LPJ-GUESS stand node from a binary state.
##'
##' @return An integer (1-based) patch index, or NA_integer_ if no patch is available.
##'
##' @details The function reads each patch's individuals' \code{fpc}. If a patch
##' has no individuals, it is assigned a gap of 1. Among finite gaps, the maximum
##' is returned via \code{which.max}.
##'
##' @keywords internal
.pick_patch_for_seeding <- function(stand) {
  np <- stand$npatches
  if (is.null(np) || !is.finite(np) || np <= 0) return(NA_integer_)
  
  gaps <- rep(NA_real_, np)
  for (p in seq_len(np)) {
    patch <- stand$Patch[[p]]
    nind  <- length(patch$Vegetation$Individuals)
    if (nind == 0) { gaps[p] <- 1; next }
    # fpcs <- vapply(
    #   patch$Vegetation$Individuals,
    #   function(ind) { f <- ind$fpc; if (is.null(f) || !is.finite(f)) 0 else f },
    #   FUN.VALUE = 0.0
    # )
    fpcs <- vapply(
      patch$Vegetation$Individuals,
      function(ind) {
        if (isTRUE(ind$alive)) {
          f <- ind$fpc
          if (is.null(f) || !is.finite(f)) 0 else f
        } else 0
      },
      FUN.VALUE = 0.0
    )
    gaps[p] <- max(0, 1 - sum(fpcs, na.rm = TRUE))
  }
  which.max(gaps)
}

##' @title Build a minimal, self-consistent cohort at a given diameter
##'
##' @description Creates a new individual (cohort) for a given PFT using LPJ-GUESS
##' geometric relations (Eq.5/6) at the specified diameter (typically \code{min.diam})
##' and an extremely small density, so that area-based pools are negligible but the
##' cohort is immediately eligible for size-nudge.
##'
##' @param template A list: an existing individual used as a template (fields/slots).
##' @param pft_row A one-row data.frame or a named numeric vector with keys:
##' \code{k_allom1}, \code{k_allom2}, \code{k_allom3}, \code{k_rp},
##' \code{crownarea_max}, \code{wooddens}, \code{sla}, \code{k_latosa}.
##' @param pft_id Integer PFT id to assign to the new individual.
##' @param dens0 Numeric, tiny density (area-scale), default \code{1e-6}.
##' @param ltor_init Numeric, initial leaf:root ratio to back out root mass.
##' @param diam_cm Numeric, stem diameter in cm (use your \code{min.diam}).
##' @param lai_indiv0 Numeric, initial per-individual LAI used to back out leaf mass.
##'
##' @return A list representing the newly seeded individual with key state set
##' (geometry/pools) and derived quantities zeroed to be recomputed by daily/allometry.
##' @keywords internal
.seed_cohort_quick <- function(template, pft_row, pft_id,
                               dens0 = 1e-6, ltor_init = 1.0,
                               diam_cm, lai_indiv0 = 0.25) {
  
  stopifnot(is.list(template), is.finite(diam_cm), diam_cm > 0)
  
  # Make sure we can index pft_row with [[ ]] scalars
  getp <- function(nm) {
    if (is.data.frame(pft_row)) return(as.numeric(pft_row[[nm]]))
    as.numeric(pft_row[[nm]])
  }
  
  k_allom1       <- getp("k_allom1")
  k_allom2       <- getp("k_allom2")
  k_allom3       <- getp("k_allom3")
  k_rp           <- getp("k_rp")
  crownarea_max  <- getp("crownarea_max")
  wooddens       <- getp("wooddens")
  sla            <- getp("sla")
  k_latosa       <- getp("k_latosa")
  
  diam_m  <- diam_cm / 100
  height0 <- (diam_m ^ k_allom3) * k_allom2
  crown0  <- min(k_allom1 * (diam_m ^ k_rp), crownarea_max)
  crown0  <- max(crown0, 1e-6)
  
  vol     <- height0 * pi * diam_m * diam_m * 0.25
  hfrac   <- 0.02                         # For heartwood 
  f_sap   <- wooddens * height0 * sla / k_latosa
  lai_min <- (0.9 * wooddens * vol / ((1 + hfrac) * f_sap)) * (sla / crown0)
  
  lai_indiv0 <- max(lai_indiv0, lai_min)  # External input is allowed, at least no less than lai_min
  # Per-individual pools
  cmass_leaf_ind <- (lai_indiv0 * crown0) / sla
  cmass_sap_ind  <- (wooddens * height0 * sla / k_latosa) * cmass_leaf_ind
  cmass_heart_ind <- hfrac * cmass_sap_ind
  
  ## To be on the safe side: Do another LowWoodDensity check according to the formula in allometry
  wd_min   <- 0.9 * wooddens
  wood_now <- (cmass_heart_ind + cmass_sap_ind) / vol   # individual scale and does not include dens
  
  if (!is.finite(wood_now) || wood_now < wd_min) {
    required_wood <- wd_min * vol
    if (cmass_heart_ind + cmass_sap_ind > 0) {
      scale <- required_wood / (cmass_heart_ind + cmass_sap_ind)
      cmass_heart_ind <- cmass_heart_ind * scale
      cmass_sap_ind   <- cmass_sap_ind   * scale
    } else {
      cmass_heart_ind <- 0
      cmass_sap_ind   <- required_wood
    }
  }
  
  # Area-scale pools (aggregate by tiny density)
  cmass_leaf <- cmass_leaf_ind * dens0
  cmass_sap  <- cmass_sap_ind  * dens0
  cmass_root <- cmass_leaf / ltor_init
  cmass_heart <- cmass_heart_ind * dens0
  
  ind <- template
  
  # Identity & life-cycle
  ind$indiv.pft.id      <- pft_id
  ind$age               <- 1L
  ind$alive             <- TRUE
  ind$last_turnover_day <- -1L
  
  # Geometry & demographic
  ind$densindiv   <- dens0
  ind$height      <- height0
  ind$crownarea   <- crown0
  ind$ltor        <- ltor_init
  
  # Carbon pools (area-scale)
  ind$cmass_leaf  <- cmass_leaf
  ind$cmass_root  <- cmass_root
  ind$cmass_sap   <- cmass_sap
  ind$cmass_heart <- cmass_heart
  ind$cmass_debt  <- 0.0
  # ind$cmass_repro <- 0.0
  
  # Derived to be recomputed by daily/phenology/allometry
  ind$lai        <- 0.0
  ind$lai_indiv  <- lai_indiv0
  ind$fpc        <- 0.0
  ind$deltafpc   <- 0.0
  
  ind$nmass_leaf  <- 0
  ind$nmass_root  <- 0
  ind$nmass_sap   <- 0
  ind$nmass_heart <- 0
  # ind$nmass_debt  <- 0
  ind$nmass_veg   <- 0
  
  if (!is.null(ind$mlai))    ind$mlai[]    <- 0.0
  if (!is.null(ind$greff_5)) ind$greff_5[] <- 0.0
  ind$wstress    <- FALSE
  ind$phen       <- 1.0
  
  ind
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
##' @author Matthew Forrest, Yinghao Sun
update_state_LPJGUESS <- function(model.state, pft.params, dens.initial, dens.target, AbvGrndWood.initial, AbvGrndWood.target, AbvGrndWood.epsilon, trace, min.diam, HEIGHT_MAX = 150) {
  
  # Find stands that are NATURAL and frac>0
  nstands <- length(model.state$Stand)
  nat_idx_pre <- integer(0)
  for (s in seq_len(nstands)) {
    st <- model.state$Stand[[s]]
    lc <- st$landcovertype
    fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
    if (!is.null(lc) && as.integer(lc) == 4L && fr > 0) nat_idx_pre <- c(nat_idx_pre, s)
  }
  
  # ---- GRIDCELL-LEVEL SEEDING (once per PFT if globally absent) ----
  eps <- .Machine$double.eps
  
  # calculate the initial AGB value of "cell ×PFT" based on the current state (without changing the state)
  AbvGrndWood.initial.gc <- calculateGridcellVariablePerPFT(
    model.state, "AbvGrndWood", min.diam = min.diam, pft.params = pft.params
  )
  
  # Which PFTs are fully absent and observations >0?
  need_seed_idx <- which(AbvGrndWood.initial.gc <= eps & AbvGrndWood.target > 0)
  
  if (length(need_seed_idx)) {
    # Collect all the patches containing dead individuals from the stands that are NATURAL and have a frac>0 as candidates
    # and calculate the gap, giving priority to reviving those with a larger gap.
    cand <- list()
    for (s in seq_along(model.state$Stand)) {
      st <- model.state$Stand[[s]]
      lc <- st$landcovertype
      fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
      if (is.null(lc) || as.integer(lc) != 4L || fr <= 0) next  # 只看 NATURAL
      np <- st$npatches; if (!is.finite(np) || np <= 0) next
      
      for (p in seq_len(np)) {
        pa <- st$Patch[[p]]
        inds <- pa$Vegetation$Individuals
        if (length(inds) == 0) next
        
        dead_idx <- which(!vapply(inds, function(x) isTRUE(x$alive), logical(1)))
        if (length(dead_idx) == 0) next
        fpc_alive <- vapply(inds, function(ind) if (isTRUE(ind$alive)) as.numeric(ind$fpc) else 0.0, 0.0)
        gap <- max(0, 1 - sum(fpc_alive, na.rm = TRUE))
        
        cand[[length(cand) + 1L]] <- list(stand = s, patch = p, dead_idx = dead_idx, gap = gap)
      }
    }
    
    # Execute each PFT that "needs to be sown" one by one: Sow only once
    # A dead slot must be revived. If not, report an error and exit
    for (pft.index in need_seed_idx) {
      pft_id_zero_based <- pft.index - 1L
      
      # Find a template individual (any one will do, as long as the structure is complete)
      template_any <- NULL
      for (s in seq_along(model.state[["Stand"]])) {
        for (p in seq_along(model.state[["Stand"]][[s]][["Patch"]])) {
          inds <- model.state[["Stand"]][[s]][["Patch"]][[p]][["Vegetation"]][["Individuals"]]
          if (length(inds) > 0) { template_any <- inds[[1]]; break }
        }
        if (!is.null(template_any)) break
      }
      if (length(cand) == 0) {
        inds <- model.state[["Stand"]][[1]][["Patch"]][[1]][["Vegetation"]][["Individuals"]]
        if (is.null(template_any)) stop("No template individual available anywhere; cannot append a new cohort safely.")
        
        ltor0 <- suppressWarnings(as.numeric(template_any$ltor))
        if (!is.finite(ltor0) || ltor0 <= 0) ltor0 <- 1.0
        ltor0 <- min(max(ltor0, 0.2), 5.0)
        # Create a new cohort
        new_ind <- .seed_cohort_quick(template = template_any,
                                      pft_row  = pft.params[pft.index,],
                                      pft_id   = pft_id_zero_based,
                                      dens0    = 1e-2,
                                      ltor_init= ltor0,
                                      diam_cm  = min.diam+1,
                                      lai_indiv0 = 0.25)
        
        inds[[length(inds) + 1L]] <- new_ind
        model.state[["Stand"]][[s]][["Patch"]][[p]][["Vegetation"]][["Individuals"]] <- inds
        model.state[["Stand"]][[s]][["Patch"]][[p]][["Vegetation"]][["number_of_individuals"]] <- as.integer(length(inds))
        
      }else{
        # Select the candidate with the largest gap (including at least one dead slot)
        k <- which.max(vapply(cand, `[[`, 0.0, "gap"))
        s <- cand[[k]]$stand
        p <- cand[[k]]$patch
        i_dead <- cand[[k]]$dead_idx[1]  # Revive the dead slot without altering the length of Individuals

        st <- model.state$Stand[[s]]
        pa <- st$Patch[[p]]
        template <- pa$Vegetation$Individuals[[i_dead]]

        ltor0 <- suppressWarnings(as.numeric(template$ltor))
        if (!is.finite(ltor0) || ltor0 <= 0) ltor0 <- 1.0
        ltor0 <- min(max(ltor0, 0.2), 5.0)

        # Generate new individuals:minimum density; minimum diameter; reverse solution of other pools according to PFT parameters
        new.ind <- .seed_cohort_quick(
          template   = template,
          pft_row    = pft.params[pft.index, ],
          pft_id     = pft_id_zero_based,
          dens0      = 1e-2,         # Small but non-zero, facilitating this round of density nudge; The pos/siz will not be moved
          ltor_init  = ltor0,
          diam_cm    = min.diam+1,
          lai_indiv0 = 0.25
        )

        ## Overwrite dead slots instead of append (without changing the length and offset of Individuals)
        model.state$Stand[[s]]$Patch[[p]]$Vegetation$Individuals[[i_dead]] <- new.ind

        # Does this candidate have any other dead ends? 
        # Remove the one just used to avoid reusing the same position for the next PFT
        rest <- cand[[k]]$dead_idx[-1]
        if (length(rest)) cand[[k]]$dead_idx <- rest else cand <- cand[-k]

      }
      
      
    }
    
    # After sowing, calculate the initial value of the gridcell level again (for subsequent factor calculation)
    AbvGrndWood.initial.gc <- calculateGridcellVariablePerPFT(
      model.state, "AbvGrndWood", min.diam = min.diam, pft.params = pft.params
    )
  }
  # ---- end GRIDCELL-LEVEL SEEDING ----
  
  
  AbvGrndWood.initial <- calculateGridcellVariablePerPFT(model.state, "AbvGrndWood", min.diam=min.diam, pft.params=pft.params)
  # calculate relative increases to be applied later on (per PFT)
  dens.initial <- calculateGridcellVariablePerPFT(model.state, "densindiv", min.diam=min.diam, pft.params=pft.params)
  dens.target <- dens.initial
  dens.rel.change <- dens.target/dens.initial
  AbvGrndWood.rel.change <- AbvGrndWood.target/AbvGrndWood.initial
  

  # --- Perform density-individual splitting according to the AGB objective ---
  USE_ALPHA_SPLIT <- TRUE
  if (USE_ALPHA_SPLIT) {
    clip <- function(x, lo, hi) pmax(lo, pmin(hi, x))
    # PFT level: R = AGB_tar / AGB_now
    # R <- AbvGrndWood.target / AbvGrndWood.initial
    R_raw <- AbvGrndWood.target / pmax(AbvGrndWood.initial, .Machine$double.eps)
    R_eff <- R_raw
    R_eff[!is.finite(R_eff)] <- 1
    # R_eff[need_seed] <- 10

    # Adaptive α: Ensure that the "relative change of each plant" falls within [0.75, 1.30]
    # When R≈1, let α=0 to avoid 0/0
    alpha <- rep(0, length(R_eff))
    idx <- is.finite(R_eff) & (abs(R_eff - 1) > 1e-6)
    alpha[idx] <- 1 - log(clip(R_eff[idx], 0.75, 1.30)) / log(R_eff[idx])
    alpha <- clip(alpha, 0, 1)
    
    # ---- Density soft upper limit based on FPC ----
    # existing PFT-level FPC (grid-weighted summation)
    fpc_now <- calculateGridcellVariablePerPFT(model.state, "fpc", pft.params, min.diam = 0.5)
    FPC_CAP <- 0.98  # Adjustable
    
    n_now        <- dens.initial
    n_floor_frac <- 0.2   # proportion of density flooring, adjustable
    n_floor      <- pmax(1e-4, n_floor_frac * pmax(n_now, 1e-6))
    
    # For PFT where n_now≈0, use n_floor for multiplication to avoid 0 * Inf -> NaN
    n_now_eff <- pmax(n_now, n_floor)
    
    # obtain the "unconstrained" density target first
    n_tar_raw <- n_now_eff * (R_eff ^ alpha)          
    lambda_pft <- pmax(n_tar_raw / pmax(n_now, 1e-12), 1e-6)
    
    # Use the FPC soft upper limit shrinkage density multiple (linear approximation: FPC_new ≈ lambda * FPC_now)
    shrink <- rep(1, length(lambda_pft))
    idx <- is.finite(fpc_now) & (fpc_now > 0)
    shrink[idx] <- pmin(1, FPC_CAP / (lambda_pft[idx] * fpc_now[idx]))
    lambda_pft <- lambda_pft * shrink
    
    # Final density target and relative variation
    n_tar <- pmax(lambda_pft * n_now, n_floor)
    dens.rel.change        <- pmax(n_tar / pmax(n_now, n_floor), 1e-6)
    AbvGrndWood.rel.change <- pmax(R_eff ^ (1 - alpha), 1e-6)

  }
  
  
  # nstands - should always be 1 but lets make sure
  nstands <- unlist(model.state$nstands)
  # if(nstands != 1) warning("More than one Stand found in LPJ-GUESS state.  This possibly implies that land use has been enabled
  #                          which the PEcAn code might not be robust against.")
  
  if (length(nstands) == 0) nstands <- length(model.state$Stand)
  if (nstands < 1) stop("No Stand found.")
  
  nat_idx  <- integer(0)
  for (s in 1:nstands) {
    st <- model.state$Stand[[s]]
    lc <- st$landcovertype
    fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
    if (!is.null(lc) && as.integer(lc) == 4L && fr > 0) {   # 4 == NATURAL
      nat_idx <- c(nat_idx, s)
    }
  }
  if (!length(nat_idx)) {
    if (trace) message("No NATURAL stands with positive frac; nothing to do.")
    return(model.state)
  }
  
  #
  for(stand.counter in nat_idx) {
    
    # get the number of patches
    npatches <- model.state$Stand[[stand.counter]]$npatches
    if(npatches == 0) next
    
    # get list of all the PFTs included in this stand
    active.PFTs <- c()
    for(stand.pft.id in 1:length(model.state$Stand[[stand.counter]]$Standpft$active)) {
      if(model.state$Stand[[stand.counter]]$Standpft$active[[stand.pft.id]]) active.PFTs <- append(active.PFTs, stand.pft.id -1)
    }
    
    # loop through each patch
    for(patch.counter in 1:npatches) {
      
      this.patch <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]
      if(length(this.patch$Vegetation$Individuals) == 0) next
      
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
        if(original.individual$alive & diam >= min.diam) {
          
          
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
            } # # if initial AbvGrndWood adjustment very low
          } else if(target.AbvGrndWood.rel.change > 100) {
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
            } # if initial AbvGrndWood adjustment very high
            
          } else {# AbvGrndWood nudge is safe to try without adjusting density
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
            
            # --- NEW: recompute allometry after density change & write back ---
            allr <- allometry(
              lifeform = pft.params[pft.index, "lifeform"],
              cmass_leaf  = updated.individual$cmass_leaf,
              cmass_sap   = updated.individual$cmass_sap,
              cmass_heart = updated.individual$cmass_heart,
              densindiv   = updated.individual$densindiv,
              age         = updated.individual$age,
              fpc         = updated.individual$fpc,
              deltafpc    = updated.individual$deltafpc,
              sla         = pft.params[pft.index, "sla"],
              k_latosa    = pft.params[pft.index, "k_latosa"],
              k_rp        = pft.params[pft.index, "k_rp"],
              k_allom1    = pft.params[pft.index, "k_allom1"],
              k_allom2    = pft.params[pft.index, "k_allom2"],
              k_allom3    = pft.params[pft.index, "k_allom3"],
              wooddens    = pft.params[pft.index, "wooddens"],
              crownarea_max = pft.params[pft.index, "crownarea_max"],
              HEIGHT_MAX  = HEIGHT_MAX
            )
            

            updated.individual$lai_indiv <- allr$lai_indiv
            updated.individual$lai       <- allr$lai
            updated.individual$deltafpc  <- allr$deltafpc
            updated.individual$fpc       <- allr$fpc
            updated.individual$boleht    <- allr$boleht
            
            
            
            if(trace) {
              print(paste(" ------- AFTER INITIAL DENSITY ADJUSTMENT -------"))
              print(paste(" ***** Density =", final.densindiv))
              print(paste(" ***** AbvGrndWood =", AbvGrndWood(updated.individual)))
            }
            if(updated.individual$densindiv != original.individual$densindiv * current.target.densindiv.rel.change) {
              stop(" ***** Density adjustment failed, this is suprising and confusing...")
            }
            
          } else {
            if(trace) {
              print(paste(" ------- NO INITIAL DENSITY ADJUSTMENT REQUIRED -------"))
            } # if trace
            updated.individual <- original.individual  # Make sure the variable is initialized
          } # if no density adjustment required
          
          # After STEP 1 ends and before entering STEP 2
          post_density_baseline <- updated.individual
          post_density_agb      <- AbvGrndWood(post_density_baseline)
          
          ## ---------- Step 2 : biomass allocation (transactional) ----------
          MAX_DENSITY_RETRIES <- 3     # Adjustable: 2-5
          MAX_ATTEMPT_STEP    <- 6     # Search for the number of magnifications
          ENLARGE_FACTOR      <- 1.8
          FPC_CAP             <- 0.98  
          GEOM_CROWN_MIN      <- 1e-6  # Geometric "rationality" threshold (avoiding extremely small crownarea)
          GEOM_HEIGHT_MIN     <- 0.05  # m, avoid heights close to 0
          GEOM_LAI_IND_MAX    <- 1e6   # Unreasonable if LAI of a single plant is too large
          
          density_retry <- 0

          repeat {
            # Does this round need to "start over after parameter adjustment" due to a specific error?
            restart_round <- FALSE
            
            # Overall target multiple
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            
            # Accumulate the litter (area caliber, kgC m-2) generated during the Step-2 period
            litter_leaf_accum  <- 0.0
            litter_root_accum  <- 0.0
            exceeds_cmass_sum  <- 0.0
            
            ## Define the goals of this round based on the baseline after the density step
            pre_round_indiv       <- updated.individual        # <--- Starting point (for rollback)
            pre_round_agb         <- AbvGrndWood(pre_round_indiv)
            target.AbvGrndWood    <- pre_round_agb * current.target.AbvGrndWood.rel.change
            
            prev_gap <- Inf
            result.code <- "NOTCONVERGED"
            
            for (counter in 1:99) {
              
              current_agb <- AbvGrndWood(updated.individual)
              gap <- target.AbvGrndWood - current_agb
              if (abs(gap) / max(abs(target.AbvGrndWood), .Machine$double.eps) <= AbvGrndWood.epsilon) {
                result.code <- "FIRST"; break
              }
              
              ## ---- 1) Goal-oriented step size (with structural minimum and 30% upper limit) ----
              ltor_val <- updated.individual$ltor
              height_v <- updated.individual$height
              leaf_min <- max(
                0,
                pft.params[pft.index,"k_latosa"] * updated.individual$cmass_sap /
                  (pft.params[pft.index,"wooddens"] * height_v * pft.params[pft.index,"sla"]) -
                  updated.individual$cmass_leaf
              )
              root_min <- max(0, leaf_min / max(ltor_val, 1e-6))
              min_step <- leaf_min + root_min
              
              k_gain   <- 0.6
              step0    <- k_gain * gap
              max_step <- 0.3 * (abs(target.AbvGrndWood) + abs(current_agb))
              # base_inc <- sign(step0) * max(min_step, min(max_step, abs(step0)))
              if (gap > 0) {
                base_inc <- sign(step0) * max(min_step, min(max_step, abs(step0)))
              } else {
                base_inc <- sign(step0) * min(max_step, abs(step0)) 
              }
              
              ## ---- 2) Line search: If fall into "leaf-root-only", take a big step forward and try again ----
              attempt <- 1
              this.biomass.inc <- base_inc
              # To "roll back", keep the copy before the attempt
              indiv_before <- updated.individual
              
              repeat {
                try.list <- adjust.biomass.scaling.LPJGUESS(
                  individual = updated.individual,
                  biomass.inc = this.biomass.inc,
                  sla       = pft.params[pft.index,"sla"],
                  wooddens  = pft.params[pft.index,"wooddens"],
                  lifeform  = pft.params[pft.index,"lifeform"],
                  k_latosa  = pft.params[pft.index,"k_latosa"],
                  k_allom2  = pft.params[pft.index,"k_allom2"],
                  k_allom3  = pft.params[pft.index,"k_allom3"]
                )
                
                # Estimate the increment of wood per plant
                sap_inc_pt  <- (try.list$individual$cmass_sap   - updated.individual$cmass_sap)   / updated.individual$densindiv
                heart_inc_pt<- (try.list$individual$cmass_heart - updated.individual$cmass_heart) / updated.individual$densindiv
                
                # Determine whether it has fallen into leaf-root-only
                if (gap > 0 && abs(sap_inc_pt) < 1e-12 && abs(heart_inc_pt) < 1e-12 && attempt < MAX_ATTEMPT_STEP) {
                  #Take a big step forward and try again 
                  this.biomass.inc <- sign(this.biomass.inc) * min(abs(this.biomass.inc) * ENLARGE_FACTOR, max_step)
                  attempt <- attempt + 1
                } else {
                  # Accept this attempt (temporary storage) for geometric verification and "whether to move forward" determination
                  cand.indiv <- try.list$individual

                  cand.allo <- allometry(
                    lifeform = pft.params[pft.index,"lifeform"],
                    cmass_leaf  = cand.indiv$cmass_leaf,
                    cmass_sap   = cand.indiv$cmass_sap,
                    cmass_heart = cand.indiv$cmass_heart,
                    densindiv   = cand.indiv$densindiv,
                    age         = cand.indiv$age,
                    fpc         = cand.indiv$fpc,
                    deltafpc    = cand.indiv$deltafpc,
                    sla         = pft.params[pft.index,"sla"],
                    k_latosa    = pft.params[pft.index,"k_latosa"],
                    k_rp        = pft.params[pft.index,"k_rp"],
                    k_allom1    = pft.params[pft.index,"k_allom1"],
                    k_allom2    = pft.params[pft.index,"k_allom2"],
                    k_allom3    = pft.params[pft.index,"k_allom3"],
                    wooddens    = pft.params[pft.index,"wooddens"],
                    crownarea_max = pft.params[pft.index,"crownarea_max"],
                    HEIGHT_MAX  = HEIGHT_MAX
                  )
                  geom_ok <- is.finite(cand.allo$height)    && cand.allo$height    > GEOM_HEIGHT_MIN &&
                    is.finite(cand.allo$crownarea) && cand.allo$crownarea > GEOM_CROWN_MIN  &&
                    is.finite(cand.allo$lai_indiv)  && cand.allo$lai_indiv < GEOM_LAI_IND_MAX
                  
                  agb_before <- AbvGrndWood(updated.individual)
                  agb_after  <- AbvGrndWood(cand.indiv)
                  wood_forward <- (gap > 0 && agb_after > agb_before) || (gap < 0 && agb_after < agb_before)
                  
                  # ---- Process the specific result code by allometry$error.string ----
                  err <- if (!is.null(cand.allo$error.string)) as.character(cand.allo$error.string) else "OK"
                  
                  if (err != "OK") {
                    if (err == "NegligibleLeafMass") {
                      if (gap < 0 && attempt < MAX_ATTEMPT_STEP) {
                        this.biomass.inc <- this.biomass.inc * 0.5   # or 0.3 
                        attempt <- attempt + 1
                        next
                      } else {
                        restart_round <- TRUE            # Roll back to the starting point of this round
                        result.code <- "NEED_DENSITY_FALLBACK"
                        break
                      }
                    } else if (err == "LowWoodDensity") {
                      # current.target.AbvGrndWood.rel.change <- 1.1 * current.target.AbvGrndWood.rel.change
                      # current.target.densindiv.rel.change   <- target.overall.rel.change / current.target.AbvGrndWood.rel.change
                      # updated.individual <- pre_round_indiv
                      # restart_round <- TRUE
                      ## determine that this round of size nudge is not feasible
                      result.code   <- "LowWoodDensity"
                      restart_round <- FALSE   # Don't start this round over again
                      break         # Jump out of the current line-search/nudge loop
                    } else if (err == "MaxHeightExceeded") {
                      # Individual size too large: Density ↑10%, AGB target reduced by 1/1.1
                      current.target.densindiv.rel.change   <- current.target.densindiv.rel.change * 1.1
                      current.target.AbvGrndWood.rel.change <- current.target.AbvGrndWood.rel.change / 1.1
                      updated.individual <- pre_round_indiv
                      restart_round <- TRUE
                    } else {
                      # Other unknown codes: Take out result.code and hand it over to Step-3 for unified processing
                      result.code <- err
                    }
                  }
                  
                  # Exit the line-search repeat and go back to the top of the outer repeat
                  if (restart_round) break
                  # If other result.codes (not OK) are set, the inner loop should also be exited
                  if (exists("result.code") && result.code != "NOTCONVERGED") break
                  
                  
                  
                  if (geom_ok && wood_forward) {
                    updated.individual <- cand.indiv
                    updated.individual$height    <- cand.allo$height
                    updated.individual$crownarea <- cand.allo$crownarea
                    updated.individual$lai_indiv <- cand.allo$lai_indiv
                    updated.individual$lai       <- cand.allo$lai
                    updated.individual$deltafpc  <- cand.allo$deltafpc
                    updated.individual$fpc       <- cand.allo$fpc
                    updated.individual$boleht    <- cand.allo$boleht
                    
                    # Record the litter generated by this adjustment (adjusting.biomass.LPJGUESS returns the increment of "per plant")
                    litter_leaf_accum <- litter_leaf_accum + (if (is.null(try.list$litter_leaf_inc)) 0 else try.list$litter_leaf_inc) * cand.indiv$densindiv
                    litter_root_accum <- litter_root_accum + (if (is.null(try.list$litter_root_inc)) 0 else try.list$litter_root_inc) * cand.indiv$densindiv
                    exceeds_cmass_sum <- exceeds_cmass_sum + (if (is.null(try.list$exceeds_cmass)) 0 else try.list$exceeds_cmass)
                    
                  } else {
                    # Roll back this attempt
                    updated.individual <- indiv_before
                  }
                  break
                }
              } # end repeat line-search
              
              ## break out of the middle for loop and hand over to the outer repeat's `next` to "start over"
              if (restart_round) break   
              
              # If there is almost no improvement (<10%), take a small ±10% fallback step (still subject to the max_step limit).
              new_gap <- target.AbvGrndWood - AbvGrndWood(updated.individual)
              if (abs(new_gap) >= 0.9 * abs(prev_gap)) {
                bump <- if (gap > 0)  0.1 * current_agb else -0.1 * current_agb
                bump <- sign(bump) * min(abs(bump), max_step)
                
                # Try again (in the same transactional way)
                indiv_before <- updated.individual
                try.list <- adjust.biomass.scaling.LPJGUESS(
                  individual = updated.individual,
                  biomass.inc = bump,
                  sla       = pft.params[pft.index,"sla"],
                  wooddens  = pft.params[pft.index,"wooddens"],
                  lifeform  = pft.params[pft.index,"lifeform"],
                  k_latosa  = pft.params[pft.index,"k_latosa"],
                  k_allom2  = pft.params[pft.index,"k_allom2"],
                  k_allom3  = pft.params[pft.index,"k_allom3"]
                )
                cand.indiv <- try.list$individual
                
                agb_before <- AbvGrndWood(indiv_before)
                agb_after  <- AbvGrndWood(cand.indiv)
                
                gap_before <- target.AbvGrndWood - agb_before
                wood_forward <- (gap_before > 0 && agb_after > agb_before) ||
                  (gap_before < 0 && agb_after < agb_before)
                if (wood_forward) {
                  
                  cand.allo <- allometry(
                    lifeform = pft.params[pft.index,"lifeform"],
                    cmass_leaf  = cand.indiv$cmass_leaf,
                    cmass_sap   = cand.indiv$cmass_sap,
                    cmass_heart = cand.indiv$cmass_heart,
                    densindiv   = cand.indiv$densindiv,
                    age         = cand.indiv$age,
                    fpc         = cand.indiv$fpc,
                    deltafpc    = cand.indiv$deltafpc,
                    sla         = pft.params[pft.index,"sla"],
                    k_latosa    = pft.params[pft.index,"k_latosa"],
                    k_rp        = pft.params[pft.index,"k_rp"],
                    k_allom1    = pft.params[pft.index,"k_allom1"],
                    k_allom2    = pft.params[pft.index,"k_allom2"],
                    k_allom3    = pft.params[pft.index,"k_allom3"],
                    wooddens    = pft.params[pft.index,"wooddens"],
                    crownarea_max = pft.params[pft.index,"crownarea_max"],
                    HEIGHT_MAX  = HEIGHT_MAX
                  )
                  
                  err <- if (!is.null(cand.allo$error.string)) as.character(cand.allo$error.string) else "OK"
                  geom_ok <- is.finite(cand.allo$height)    && cand.allo$height    > GEOM_HEIGHT_MIN &&
                    is.finite(cand.allo$crownarea) && cand.allo$crownarea > GEOM_CROWN_MIN  &&
                    is.finite(cand.allo$lai_indiv) && cand.allo$lai_indiv < GEOM_LAI_IND_MAX
                  
                  if (err == "OK" && geom_ok){
                    updated.individual <- cand.indiv
                    
                    updated.individual$height    <- cand.allo$height
                    updated.individual$crownarea <- cand.allo$crownarea
                    updated.individual$lai_indiv <- cand.allo$lai_indiv
                    updated.individual$lai       <- cand.allo$lai
                    updated.individual$deltafpc  <- cand.allo$deltafpc
                    updated.individual$fpc       <- cand.allo$fpc
                    updated.individual$boleht    <- cand.allo$boleht
                    
                    litter_leaf_accum <- litter_leaf_accum +
                      (if (is.null(try.list$litter_leaf_inc)) 0 else try.list$litter_leaf_inc) * cand.indiv$densindiv
                    litter_root_accum <- litter_root_accum +
                      (if (is.null(try.list$litter_root_inc)) 0 else try.list$litter_root_inc) * cand.indiv$densindiv
                    exceeds_cmass_sum <- exceeds_cmass_sum +
                      (if (is.null(try.list$exceeds_cmass)) 0 else try.list$exceeds_cmass)
                    
                  }else{
                    ## allometry failed: Roll back
                    updated.individual <- indiv_before
                  }
                  
                } else {
                  updated.individual <- indiv_before
                }
                new_gap <- target.AbvGrndWood - AbvGrndWood(updated.individual)
              }
              prev_gap <- new_gap
              
              if (abs(new_gap) / max(abs(target.AbvGrndWood), .Machine$double.eps) <= AbvGrndWood.epsilon) {
                result.code <- "FIRST"; break
              }
              if (counter == 99) result.code <- "NOTCONVERGED"
            } # end inner for
            # If this round needs to be redone after parameter adjustment due to an error, directly proceed to the next round of repeat
            if (restart_round && result.code != "NEED_DENSITY_FALLBACK") next
            
            if (result.code != "NOTCONVERGED" && result.code != "NEED_DENSITY_FALLBACK") break
            # if (result.code != "NOTCONVERGED") break
            
            ## ---- 3) Line search still fails: Density compensation round (mild + FPC soft upper limit) ----
            if (density_retry >= MAX_DENSITY_RETRIES) break
            
            current_agb <- AbvGrndWood(updated.individual)
            residual_gap <- target.AbvGrndWood - current_agb
            if (residual_gap < 0){
              lambda <- max(0.5, 1 - 0.3 * min(1, abs(residual_gap)/max(pre_round_agb, 1e-12)))
              updated.individual <- adjust.density.LPJGUESS(updated.individual, lambda)
              allr <- allometry(
                lifeform = pft.params[pft.index,"lifeform"],
                cmass_leaf  = updated.individual$cmass_leaf,
                cmass_sap   = updated.individual$cmass_sap,
                cmass_heart = updated.individual$cmass_heart,
                densindiv   = updated.individual$densindiv,
                age         = updated.individual$age,
                fpc         = updated.individual$fpc,
                deltafpc    = updated.individual$deltafpc,
                sla         = pft.params[pft.index,"sla"],
                k_latosa    = pft.params[pft.index,"k_latosa"],
                k_rp        = pft.params[pft.index,"k_rp"],
                k_allom1    = pft.params[pft.index,"k_allom1"],
                k_allom2    = pft.params[pft.index,"k_allom2"],
                k_allom3    = pft.params[pft.index,"k_allom3"],
                wooddens    = pft.params[pft.index,"wooddens"],
                crownarea_max = pft.params[pft.index,"crownarea_max"],
                HEIGHT_MAX  = HEIGHT_MAX
                )
              # updated.individual$height    <- allr$height      
              # updated.individual$crownarea <- allr$crownarea  
              updated.individual$lai_indiv <- allr$lai_indiv
              updated.individual$lai       <- allr$lai
              updated.individual$deltafpc  <- allr$deltafpc
              updated.individual$fpc       <- allr$fpc
              updated.individual$boleht    <- allr$boleht
              density_retry <- density_retry + 1 
              next
            }
            
            lambda_extra_raw <- 1 + 0.3 * residual_gap / max(pre_round_agb, 1e-12) #maximum additional share for a single round is 30%
            lambda_extra_raw <- max(1.0, min(lambda_extra_raw, 1.5))
            
            # PFT-level FPC soft upper limit contraction
            fpc_now_pft <- calculateGridcellVariablePerPFT(model.state, "fpc", pft.params, min.diam = min.diam)
            shrink <- 1.0
            if (is.finite(fpc_now_pft[pft.index]) && fpc_now_pft[pft.index] > 0) {
              # shrink <- min(1.0, FPC_CAP / (fpc_now_pft[pft.index] * current.target.densindiv.rel.change * lambda_extra_raw))
              shrink <- min(1.0, FPC_CAP / (fpc_now_pft[pft.index] * lambda_extra_raw))
            }
            lambda_extra <- lambda_extra_raw * shrink
            
            # Apply density compensation and refresh the allometry immediately
            updated.individual <- adjust.density.LPJGUESS(updated.individual, lambda_extra)
            allr <- allometry(
              lifeform = pft.params[pft.index,"lifeform"],
              cmass_leaf  = updated.individual$cmass_leaf,
              cmass_sap   = updated.individual$cmass_sap,
              cmass_heart = updated.individual$cmass_heart,
              densindiv   = updated.individual$densindiv,
              age         = updated.individual$age,
              fpc         = updated.individual$fpc,
              deltafpc    = updated.individual$deltafpc,
              sla         = pft.params[pft.index,"sla"],
              k_latosa    = pft.params[pft.index,"k_latosa"],
              k_rp        = pft.params[pft.index,"k_rp"],
              k_allom1    = pft.params[pft.index,"k_allom1"],
              k_allom2    = pft.params[pft.index,"k_allom2"],
              k_allom3    = pft.params[pft.index,"k_allom3"],
              wooddens    = pft.params[pft.index,"wooddens"],
              crownarea_max = pft.params[pft.index,"crownarea_max"],
              HEIGHT_MAX  = HEIGHT_MAX
            )
            # updated.individual$height    <- allr$height
            # updated.individual$crownarea <- allr$crownarea
            updated.individual$lai_indiv <- allr$lai_indiv
            updated.individual$lai       <- allr$lai
            updated.individual$deltafpc  <- allr$deltafpc
            updated.individual$fpc       <- allr$fpc
            updated.individual$boleht    <- allr$boleht
            
            density_retry <- density_retry + 1
          } # end repeat (density retries)
          
          ## ---- 4) Final fallback: still NOTCONVERGED → Roll back and hit the target with "density only" ----
          if (result.code == "NOTCONVERGED") {
            # Return to the clean snapshot at the end of Step-1
            updated.individual <- post_density_baseline
            agb_now <- post_density_agb

            if (agb_now > 0) {
              lambda_need_raw <- target.AbvGrndWood / agb_now     # Just hand over the target directly to density
              fpc_now_pft <- calculateGridcellVariablePerPFT(model.state, "fpc", pft.params, min.diam = min.diam)
              shrink <- 1.0
              if (is.finite(fpc_now_pft[pft.index]) && fpc_now_pft[pft.index] > 0) {
                shrink <- min(1.0, FPC_CAP / (fpc_now_pft[pft.index] * current.target.densindiv.rel.change * lambda_need_raw))
              }
              lambda_final <- lambda_need_raw * shrink
              
              updated.individual <- adjust.density.LPJGUESS(updated.individual, lambda_final)
              allr <- allometry(
                lifeform = pft.params[pft.index,"lifeform"],
                cmass_leaf  = updated.individual$cmass_leaf,
                cmass_sap   = updated.individual$cmass_sap,
                cmass_heart = updated.individual$cmass_heart,
                densindiv   = updated.individual$densindiv,
                age         = updated.individual$age,
                fpc         = updated.individual$fpc,
                deltafpc    = updated.individual$deltafpc,
                sla         = pft.params[pft.index,"sla"],
                k_latosa    = pft.params[pft.index,"k_latosa"],
                k_rp        = pft.params[pft.index,"k_rp"],
                k_allom1    = pft.params[pft.index,"k_allom1"],
                k_allom2    = pft.params[pft.index,"k_allom2"],
                k_allom3    = pft.params[pft.index,"k_allom3"],
                wooddens    = pft.params[pft.index,"wooddens"],
                crownarea_max = pft.params[pft.index,"crownarea_max"],
                HEIGHT_MAX  = HEIGHT_MAX
              )
              # updated.individual$height    <- allr$height
              # updated.individual$crownarea <- allr$crownarea
              updated.individual$lai_indiv <- allr$lai_indiv
              updated.individual$lai       <- allr$lai
              updated.individual$deltafpc  <- allr$deltafpc
              updated.individual$fpc       <- allr$fpc
              updated.individual$boleht    <- allr$boleht
              
              result.code <- "DENSITY_ONLY"
            } else {
              # Extreme cases where AGB_now is 0: Directly mark the failure and remain as is
              result.code <- "FAILED_NO_WOOD"
            }
          }
          # ---------- end Step 2 ----------
          
          ## ---------- Step 3 : finalize by result.code ----------
          pp <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft
          
          OK_CODES        <- c("FIRST", "DENSITY_ONLY")
          RESEED_CODES    <- c("LowWoodDensity","MaxHeightExceeded", "FAILED_NO_WOOD", "NOTCONVERGED")
          
          if (result.code %in% OK_CODES) {
            # 1) Write back to the individual
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
            
            # 2) Pool litter 
            if (litter_leaf_accum != 0 || litter_root_accum != 0) {
              # Take the existing C:N; If not, it will be given as default
              leaf_litter_cton <- tryCatch(pp$litter_leaf[[pft.index]] / pp$nmass_litter_leaf[[pft.index]], error = function(e) NA_real_)
              root_litter_cton <- tryCatch(pp$litter_root[[pft.index]] / pp$nmass_litter_root[[pft.index]], error = function(e) NA_real_)
              if (!is.finite(leaf_litter_cton) || leaf_litter_cton <= 0) leaf_litter_cton <- 30.0
              if (!is.finite(root_litter_cton) || root_litter_cton <= 0) root_litter_cton <- 63.0
              
              pp$litter_leaf[[pft.index]] <- pp$litter_leaf[[pft.index]] + litter_leaf_accum
              pp$litter_root[[pft.index]] <- pp$litter_root[[pft.index]] + litter_root_accum
              pp$nmass_litter_leaf[[pft.index]] <- pp$litter_leaf[[pft.index]] / leaf_litter_cton
              pp$nmass_litter_root[[pft.index]] <- pp$litter_root[[pft.index]] / root_litter_cton
            }
            
            # 3) write back patch
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft <- pp
            
            if (trace) {
              message(sprintf("OK [%s]  AGB=%.6f  dens=%.6f", 
                              result.code, AbvGrndWood(updated.individual), updated.individual$densindiv))
            }
            
          } else if (result.code %in% RESEED_CODES) {
            # —— untrustworthy: Roll back to the clean baseline of Step-1 and mark need_seed
            updated.individual <- post_density_baseline
            
            # After rolling back, write it back to the individual
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
            
            # Do not put the litter accumulated in this round into the pool (because we rolled it back)
            if (!is.null(pp$need_seed)) {
              pp$need_seed[[pft.index]] <- TRUE
              model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft <- pp
            }
            
            if (trace) {
              warning(sprintf("RESEED [%s]  rollback to post-density baseline; marked need_seed for PFT %d",
                              result.code, this.pft.id))
            }
            
          } else {
            # Unknown code: Conservative handling - Write back to the individual but not into the pool litter, only alert警
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
            if (trace) warning(sprintf("UNKNOWN result.code = %s; wrote individual, skipped litter.", result.code))
          }
          
          # (Optional) Record the total amount of exceeds_cmass
          if (!isTRUE(all.equal(exceeds_cmass_sum, 0))) {
            warning(sprintf("Non-zero exceeds_cmass (%.3g) at stand %d patch %d PFT %d", 
                            exceeds_cmass_sum, stand.counter, patch.counter, this.pft.id))
          }
          ## ---------- end Step 3 ----------
          
        } # if individual is alive
        
      } # for each individual
      
    } # for each patch
  } # for each stand
  return(model.state)
  
}
