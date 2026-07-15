##' Adjust LPJ-GUESS state (both biomass and density)
##'
##' @title updateState.LPJGUESS
##'
##' @description Adjust LPJ-GUESS state variables based on input parameters by nudging density ONLY, with a robust bisection fallback to ensure physical viability.
##'
##'
##' @param model.state A large multiply-nested list containing the entire LPJ-GUESS state as read by 
##' function \code{readStateBinary.LPJGUESS} 
##' @param pft.params A data.frame containing the parameters for each PFT.  Each row represents one PFT (ordering must be consistent with the vectors below. 
##' The names of the columns describe the per-PFT parameter and must include: 
##' wooddens, crownarea_max, lifeform (1 = tree, 2 = grass), k_latosa, k_rp, k_allom1,  k_allom2, k_allom3, crownarea_max and sla. 
##' @param dens.initial A numeric vector of the initial stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  
##' @param dens.target A numeric vector of the target stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  
##' @param AbvGrndWood.initial A numeric vector of the initial stand-level above ground wood (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes. 
##' @param AbvGrndWood.target A numeric vector of the target stand-level above ground wood (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  
##' @param AbvGrndWood.epsilon A single numeric specifying how close the final above ground wood needs to be to the target
##' above ground stem biomass for each individual.  eg. 0.05 requires that the final above ground wood is within 5%
##' of the target above ground wood
##' @param trace Logical; if TRUE, prints detailed adjustment process information.
##' @param min.diam Minimum tree diameter (in cm) for inclusion in adjustments.
##' @param HEIGHT_MAX Maximum allowed height of an individual.  This is the maximum height that a tree
##' can have.  This is hard-coded in LPJ-GUESS to 150 m, but for SDA that might be unrealistically big, 
##' so this argument allows adjustment. 
##' @return  And updated model state (as a big old list o' lists)
##' @export update_state_both_LPJGUESS 
##' @author Yinghao Sun
update_state_both_LPJGUESS <- function(
    model.state, pft.params,
    dens.initial, dens.target,
    AbvGrndWood.initial, AbvGrndWood.target,
    AbvGrndWood.epsilon, trace, min.diam, HEIGHT_MAX = 150
) {
  
  # Target relative change 
  lambda_tot <- AbvGrndWood.target / AbvGrndWood.initial
  lambda_tot[!is.finite(lambda_tot)] <- 1
  
  nstands <- unlist(model.state$nstands)
  if (nstands != 1) warning("Multiple stands present; code was written assuming 1.")
  
  for (is in 1:nstands) {
    npatches <- model.state$Stand[[is]]$npatches
    
    active.PFTs <- c()
    for (j in 1:length(model.state$Stand[[is]]$Standpft$active)) {
      if (model.state$Stand[[is]]$Standpft$active[[j]]) active.PFTs <- c(active.PFTs, j-1)
    }
    
    for (ip in 1:npatches) {
      patch <- model.state$Stand[[is]]$Patch[[ip]]
      
      for (ii in 1:length(patch$Vegetation$Individuals)) {
        
        orig <- patch$Vegetation$Individuals[[ii]]
        pft_id <- orig$indiv.pft.id
        if (!pft_id %in% active.PFTs) stop(sprintf("Inactive PFT id=%d encountered.", pft_id))
        px <- pft_id + 1

        diam <- ((orig$height / pft.params[px,"k_allom2"])^(1.0 / pft.params[px,"k_allom3"])) * 100
        if (!(isTRUE(orig$alive) && is.finite(diam) && diam > min.diam)) next
        
        # The initial and target of this individual
        w0      <- AbvGrndWood(orig) ; if (!is.finite(w0) || w0 <= 0) w0 <- 1e-8
        lamPFT  <- lambda_tot[px]
        lamPFT  <- max(min(lamPFT, 100), 0.01)  
        w_tgt   <- w0 * lamPFT
        
        ind <- orig  
        
        ## ===== stage A: biomass tuning =====
        max_frac <- 0.20                 
        min_frac <- 0.002                
        ok_allom <- TRUE
        for (k in 1:99) {
          
          w_now <- AbvGrndWood(ind)
          diff  <- w_tgt - w_now
          rel   <- abs(diff) / max(w_tgt, .Machine$double.eps)
          if (rel <= AbvGrndWood.epsilon) break 
          
          prop  <- 0.35
          step  <- prop * diff
          cap   <- max_frac * max(w_now, 1e-8)
          mmin  <- min_frac * max(w_now, 1e-8)
          step  <- max(-cap, min(cap, step))
          if (abs(step) < mmin) step <- sign(diff) * mmin
          
          # Record the previous state for easy rollback
          pre <- ind
          adj <- adjust.biomass.LPJGUESS(
            individual = ind, biomass.inc = step,
            sla = pft.params[px,"sla"],
            wooddens = pft.params[px,"wooddens"],
            lifeform = pft.params[px,"lifeform"],
            k_latosa = pft.params[px,"k_latosa"],
            k_allom2 = pft.params[px,"k_allom2"],
            k_allom3 = pft.params[px,"k_allom3"]
          )
          ind <- adj$individual
          
          allo <- allometry(
            cmass_leaf = ind$cmass_leaf, cmass_sap = ind$cmass_sap, cmass_heart = ind$cmass_heart,
            densindiv = ind$densindiv, age = ind$age, fpc = ind$fpc, deltafpc = ind$deltafpc,
            lifeform = pft.params[px,"lifeform"], sla = pft.params[px,"sla"],
            k_latosa = pft.params[px,"k_latosa"], k_rp = pft.params[px,"k_rp"],
            k_allom1  = pft.params[px,"k_allom1"], k_allom2 = pft.params[px,"k_allom2"],
            k_allom3  = pft.params[px,"k_allom3"], wooddens = pft.params[px,"wooddens"],
            crownarea_max = pft.params[px,"crownarea_max"], HEIGHT_MAX = HEIGHT_MAX
          )
          
          if (identical(allo$error.string, "OK")) {
            ind$height    <- allo$height
            ind$crownarea <- allo$crownarea
            ind$lai_indiv <- allo$lai_indiv
            ind$lai       <- allo$lai
            ind$deltafpc  <- allo$deltafpc
            ind$fpc       <- allo$fpc
            ind$boleht    <- allo$boleht
            ok_allom <- TRUE
          } else {
            ok_allom <- FALSE
            try_ok <- FALSE
            step_try <- step
            for (kk in 1:3) {
              step_try <- 0.5 * step_try
              if (abs(step_try) < mmin) break
              ind <- pre
              adj <- adjust.biomass.LPJGUESS(
                individual = ind, biomass.inc = step_try,
                sla = pft.params[px,"sla"], wooddens = pft.params[px,"wooddens"],
                lifeform = pft.params[px,"lifeform"], k_latosa = pft.params[px,"k_latosa"],
                k_allom2 = pft.params[px,"k_allom2"], k_allom3 = pft.params[px,"k_allom3"]
              )
              ind <- adj$individual
              allo <- allometry(
                cmass_leaf = ind$cmass_leaf, cmass_sap = ind$cmass_sap, cmass_heart = ind$cmass_heart,
                densindiv = ind$densindiv, age = ind$age, fpc = ind$fpc, deltafpc = ind$deltafpc,
                lifeform = pft.params[px,"lifeform"], sla = pft.params[px,"sla"],
                k_latosa = pft.params[px,"k_latosa"], k_rp = pft.params[px,"k_rp"],
                k_allom1  = pft.params[px,"k_allom1"], k_allom2 = pft.params[px,"k_allom2"],
                k_allom3  = pft.params[px,"k_allom3"], wooddens = pft.params[px,"wooddens"],
                crownarea_max = pft.params[px,"crownarea_max"], HEIGHT_MAX = HEIGHT_MAX
              )
              if (identical(allo$error.string, "OK")) {
                ind$height    <- allo$height
                ind$crownarea <- allo$crownarea
                ind$lai_indiv <- allo$lai_indiv
                ind$lai       <- allo$lai
                ind$deltafpc  <- allo$deltafpc
                ind$fpc       <- allo$fpc
                ind$boleht    <- allo$boleht
                ok_allom <- TRUE ; try_ok <- TRUE ; break
              }
            } # kk
            if (!try_ok) break  
          } # if OK/else
        } # k loop (biomass)
        
        ## ===== stageB: density tuning: Leave the remaining targets to densindiv=====
        w_now <- AbvGrndWood(ind)
        lam_real <- w_now / w0
        lam_need <- lamPFT / max(lam_real, 1e-12)
        
        if (abs(lam_need - 1) > AbvGrndWood.epsilon) {
          # Single-step extreme value, avoid eating too much in one bite
          # Then search the feasible area by binary search
          lam_need <- max(min(lam_need, 5.0), 0.2)
          
          lo <- 1.0 ; hi <- lam_need
          if (lo > hi) {tmp <- lo; lo <- hi; hi <- tmp}
          
          last_ok <- 1.0
          last_res <- NULL
          
          for (b in 1:8) {  # 2^-8 ~ 0.4%
            test <- 0.5*(lo+hi)
            trial <- ind
            trial$densindiv <- ind$densindiv * test
            
            allo2 <- allometry(
              cmass_leaf = trial$cmass_leaf, cmass_sap = trial$cmass_sap, cmass_heart = trial$cmass_heart,
              densindiv = trial$densindiv, age = trial$age, fpc = trial$fpc, deltafpc = trial$deltafpc,
              lifeform = pft.params[px,"lifeform"], sla = pft.params[px,"sla"],
              k_latosa = pft.params[px,"k_latosa"], k_rp = pft.params[px,"k_rp"],
              k_allom1  = pft.params[px,"k_allom1"], k_allom2 = pft.params[px,"k_allom2"],
              k_allom3  = pft.params[px,"k_allom3"], wooddens = pft.params[px,"wooddens"],
              crownarea_max = pft.params[px,"crownarea_max"], HEIGHT_MAX = HEIGHT_MAX
            )
            
            if (identical(allo2$error.string, "OK")) {
              last_ok <- test ; last_res <- allo2
              if (lam_need > 1) lo <- test else hi <- test
            } else {
              if (lam_need > 1) hi <- test else lo <- test
            }
          }
          
          if (is.null(last_res)) {
            stop(sprintf("update_state_LPJGUESS: density bisection failed (no feasible allometry) for indiv %d (PFT %d).",
                         ii, pft_id))
          }
          
          ind$densindiv <- ind$densindiv * last_ok
          ind$fpc        <- last_res$fpc
          ind$deltafpc   <- last_res$deltafpc
          ind$height     <- last_res$height
          ind$crownarea  <- last_res$crownarea
          ind$lai_indiv  <- last_res$lai_indiv
          ind$lai        <- last_res$lai
          ind$boleht     <- last_res$boleht
        }
        
        # The final test to approach the target (without being overly "stubborn", leave it to the next year to absorb residuals)
        w_fin <- AbvGrndWood(ind)
        if (abs(w_fin - w_tgt)/max(w_tgt,1e-12) > (3*AbvGrndWood.epsilon)) {
          stop(sprintf("update_state_LPJGUESS: indiv %d (PFT %d) could not reach target within tolerance after combo nudge.",
                       ii, pft_id))
        }
        
        # write back
        model.state$Stand[[is]]$Patch[[ip]]$Vegetation$Individuals[[ii]] <- ind
      } # indiv
    } # patch
  } # stand
  
  return(model.state)
}
