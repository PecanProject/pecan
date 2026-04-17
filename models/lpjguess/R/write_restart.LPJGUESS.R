##' write_restart.LPJGUESS
##'
##' Write restart files for LPJGUESS
##' new.state includes X (AGB.pft) from Analysis
##' new.params includes LPJGUESS_state
##' 
##' @param outdir output directory
##' @param runid run ID
##' @param start.time start date and time for each SDA ensemble
##' @param stop.time stop date and time for each SDA ensemble
##' @param settings PEcAn settings object
##' @param new.state analysis state vector
##' @param new.params list of parameters to convert between different states 
##' @param inputs list of model inputs to use in write.configs.LPJGUESS
##' @param RENAME flag to either rename output file or not
##' @param verbose decide if we want to print the runid
##' 
##' @return NONE
##'
##' @export
##' @author Yinghao Sun
write_restart.LPJGUESS <- function(outdir, runid,
                                   start.time, stop.time, settings,
                                   new.state, new.params, inputs = NULL, 
                                   RENAME = TRUE, verbose = FALSE){
  
  # ###### DEBUG #####
  # if(identical(runid, 'ENS-00003-1000000650')) browser()
  
  rundir <- settings$host$rundir
  variables <- colnames(new.state)
  
  # ## ---- Rename old output, remove old clim ----
  # if (RENAME) {
  #   file.rename(file.path(outdir, runid, "lpjguess.out"),
  #               file.path(outdir, runid, paste0("lpjguess.", as.Date(start.time), ".out")))
  #   system(paste("rm", file.path(rundir, runid, "lpjguess.clim")))
  # } else {
  #   PEcAn.logger::logger.severe(paste("rename = FALSE: Restart cannot proceed without output file",
  #                                     "lpjguess.out being renamed for", start.time))
  #   stop("RENAME flag is FALSE. Must rerun this timestep before continuing.")
  # }
  
  settings$run$start.date <- start.time
  settings$run$end.date <- stop.time

  ## ---- Pull old state ----
  if (is.null(new.params$LPJGUESS_state))
    PEcAn.logger::logger.severe("LPJGUESS_state missing in new.params")
  # new.params$LPJGUESS_state include state, pos_list, siz_list
  Gridcell <- new.params$LPJGUESS_state$state
  pos_list <- new.params$LPJGUESS_state$pos_list
  siz_list <- new.params$LPJGUESS_state$siz_list
  
  ## ---- Build PFT parameter table from new.params ----
  # TODO: find accurate parameters; read params from settings
  pft_par_table <- data.frame()
  # PFTs <- c("Ace_rub","Bet_all","Fag_gra","Que_rub","Tsu_can")
  # PFTs <- names(new.params)
  PFTs <- setdiff(names(new.params), "LPJGUESS_state")
  # for(PFT in PFTs) {
  #   this.param.row <- c()
  #   this.param.row["sla"] <- new.params[[PFT]]$SLA
  #   this.param.row["k_latosa"] <- new.params[[PFT]]$sapwood_ratio
  #   this.param.row["wooddens"] <- 200   #kg/m-3
  #   # this.param.row["wooddens"] <- 0.2 #g/cm-3
  #   this.param.row["lifeform"] <- 1
  #   this.param.row["k_rp"] <- 1.6
  #   this.param.row["k_allom1"] <- 250
  #   this.param.row["k_allom2"] <- 60
  #   this.param.row["k_allom3"] <- 0.67
  #   this.param.row["crownarea_max"] <- 50
  #   # conifer special case
  #   if(PFT == "Tsu_can") {
  #     this.param.row["k_allom1"] <- 150
  #   }
  #     pft_par_table <- rbind(pft_par_table , this.param.row)
  # }
  for(PFT in PFTs) {
    this.param.row <- c()
    this.param.row["sla"] <- new.params[[PFT]]$SLA
    # leaflong <- new.params[[PFT]]$leaf_longevity
    # this.param.row["sla"] <- 0.2 * exp(6.15 - 0.46 * log(12 * leaflong))
    this.param.row["k_latosa"] <- new.params[[PFT]]$sapwood_ratio
    this.param.row["wooddens"] <- new.params[[PFT]]$wood_density    #kg/m-3
    # this.param.row["wooddens"] <- 0.2 #g/cm-3
    this.param.row["lifeform"] <- 1
    this.param.row["k_rp"] <- new.params[[PFT]]$k_rp
    this.param.row["k_allom1"] <- new.params[[PFT]]$k_allom1
    this.param.row["k_allom2"] <- new.params[[PFT]]$k_allom2
    this.param.row["k_allom3"] <- new.params[[PFT]]$k_allom3
    this.param.row["crownarea_max"] <- new.params[[PFT]]$crownarea_max
    
    pft_par_table <- rbind(pft_par_table , this.param.row)
  }
  names(pft_par_table) <- c("sla", "k_latosa", "wooddens", "lifeform", "k_rp", "k_allom1", "k_allom2", "k_allom3", "crownarea_max") 
  rownames(pft_par_table) <- PFTs
  
  ## --- Build initial & target AGB vectors (kg m-2) ---
  min.diam = 5
  agb.init <- calculateGridcellVariablePerPFT(Gridcell, "AbvGrndWood", min.diam=min.diam, pft.params=pft_par_table)
  if (any(grepl("^AGB.pft", variables))) {          # column names were set in read.restart
    agb.targ <- unlist(new.state[, grepl("^AGB.pft", variables), drop=TRUE])
  }
  
  # ##### FOR DEBUG #####
  # agb.init <- agb.targ*1.05
  ### dens will not change because we wont do dens SDA temporarily
  # average per PFT
  dens.init <- calculateGridcellVariablePerPFT(Gridcell, "densindiv", min.diam=min.diam, pft.params=pft_par_table)
  dens.targ <- dens.init 
  
  
  # ##### Break down ΔAGB into "density as the main factor, size as the secondary" #####
  # deltaB <- agb.targ - agb.init                      # ΔAGB (kg m-2)
  # bbar   <- ifelse(dens.init > 0, agb.init / dens.init, NA_real_)  # current average AGB per plant
  # bbar_fallback <- median(bbar[is.finite(bbar) & bbar > 0], na.rm = TRUE)
  # bbar[!is.finite(bbar) | bbar <= 0] <- ifelse(is.finite(bbar_fallback), bbar_fallback, 0.002)
  # 
  # frac_to_density <- 0.8   # 80% of ΔAGB for density, 20% for size
  # addN  <- (pmax(deltaB,  0) * frac_to_density) / pmax(bbar, 1e-6)
  # subN  <- (pmax(-deltaB, 0) * frac_to_density) / pmax(bbar, 1e-6)
  # 
  # # One-step magnification constraints prevent density from exploding or being emptied in an instant
  # sf_max <- 3
  # sf_min <- 0.5
  # dens.proposed <- dens.init + addN - subN
  # dens.upper    <- dens.init * sf_max
  # dens.lower    <- dens.init * sf_min
  # dens.targ     <- pmin(pmax(dens.proposed, dens.lower), dens.upper)
  # 
  # # small increment left for size to carry
  # agb.size.targ <- agb.init + (deltaB * (1 - frac_to_density))
  
  
  
  ## --- Update state ---
  # choose a minimum diameter
  Gridcell_updated <- update_state_LPJGUESS(Gridcell, pft_par_table,
                                            dens.init, dens.targ,
                                            agb.init, agb.targ,
                                            AbvGrndWood.epsilon = 0.05,
                                            trace = TRUE, min.diam, HEIGHT_MAX = 150)
  
  # Recalculate
  agb.post <- calculateGridcellVariablePerPFT(
    model.state = Gridcell_updated,
    variable    = "AbvGrndWood",
    pft.params  = pft_par_table,
    min.diam    = min.diam
  )
  cat("\nAGB.init =", paste(round(agb.init,3), collapse=" "),
      "\nAGB.targ =", paste(round(agb.targ,3), collapse=" "),
      "\nAGB.post =", paste(round(agb.post,3), collapse=" "), "\n")
  
  State_updated <- list(state = Gridcell_updated,
                        pos_list = pos_list,
                        siz_list = siz_list)
  
  # Comment for test 020426 ###
  # write_binary_LPJGUESS(State_updated, file.path(outdir, runid, "state", lubridate::year(stop.time)-1))
  write_binary_LPJGUESS_flexible(State_updated,
                                 file.path(outdir, runid, "state", lubridate::year(stop.time)-1),
                                 use_fast_inplace_if_possible = TRUE,
                                 verbose = FALSE)
  
  # Override the paths in settings with ensemble's exclusive inputs
  if (!is.null(inputs)) {
    for (tag in names(inputs)) {
      settings$run$inputs[[tag]] <- inputs[[tag]]
    }
  }
  
  ## --- Regenerate config for next run ---
  restart_list <- list(
    start.time = start.time,
    stop.time = stop.time
  )
  
  do.call(write.config.LPJGUESS,
          list(defaults     = NULL,
               trait.values = new.params,
               settings     = settings,
               run.id       = runid,
               restart      = restart_list)
  )
  

  if(verbose) PEcAn.logger::logger.info("restart written for", runid)
}
