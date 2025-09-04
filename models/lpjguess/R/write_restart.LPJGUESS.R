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
##' @param RENAME flag to either rename output file or not
##' @param new.params list of parameters to convert between different states 
##' @param inputs list of model inputs to use in write.configs.SIPNET
##' @param verbose decide if we want to print the runid
##' 
##' @return NONE
##'
##' @export
##' @author Yinghao Sun
write_restart.LPJGUESS <- function(outdir, runid,
                                   start.time, stop.time, settings,
                                   new.state, RENAME = TRUE,
                                   new.params, inputs = NULL, verbose = FALSE){
  
  rundir <- settings$host$rundir
  variables <- colnames(new.state)
  
  ## ---- Rename old output, remove old clim ----
  if (RENAME) {
    file.rename(file.path(outdir, runid, "lpjguess.out"),
                file.path(outdir, runid, paste0("lpjguess.", as.Date(start.time), ".out")))
    system(paste("rm", file.path(rundir, runid, "lpjguess.clim")))
  } else {
    PEcAn.logger::logger.severe(paste("rename = FALSE: Restart cannot proceed without output file",
                                      "lpjguess.out being renamed for", start.time))
    stop("RENAME flag is FALSE. Must rerun this timestep before continuing.")
  }
  
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
  PFTs <- names(new.params)
  for(PFT in PFTs) {
    this.param.row <- c()
    this.param.row["sla"] <- new.params[[PFT]]$SLA
    this.param.row["k_latosa"] <- new.params[[PFT]]$sapwood_ratio
    this.param.row["wooddens"] <- 200   #kg/m-3
    # this.param.row["wooddens"] <- 0.2 #g/cm-3
    this.param.row["lifeform"] <- 1
    this.param.row["k_rp"] <- 1.6
    this.param.row["k_allom1"] <- 250
    this.param.row["k_allom2"] <- 60
    this.param.row["k_allom3"] <- 0.67
    this.param.row["crownarea_max"] <- 50
    # conifer special case
    if(PFT == "Tsu_can") {
      this.param.row["k_allom1"] <- 150
    }
    pft_par_table <- rbind(pft_par_table , this.param.row)
  }
  names(pft_par_table) <- c("sla", "k_latosa", "wooddens", "lifeform", "k_rp", "k_allom1", "k_allom2", "k_allom3", "crownarea_max") 
  rownames(pft_par_table) <- PFTs
  
  ## --- Build initial & target AGB vectors (kg m-2) ---
  agb.init <- calculateGridcellVariablePerPFT(Gridcell, "AbvGrndWood", min.diam=min.diam, pft.params=pft_par_table)
  if (any(grepl("^AGB.pft", variables))) {          # column names were set in read.restart
    agb.targ <- PEcAn.utils::ud_convert(
      unlist(new.state[, grepl("^AGB.pft", variables), drop=TRUE]),
      "Mg/ha","kg/m^2")
  }
  
  ### dens will not change because we wont do dens SDA temporarily
  dens.init <- calculateGridcellVariablePerPFT(Gridcell, "densindiv", min.diam=min.diam, pft.params=pft_par_table)
  dens.targ <- dens.init 
  
  ## --- Update state ---
  # choose a minimum diameter
  min.diam = 0.5
    Gridcell_updated <- update_state_LPJGUESS(Gridcell, pft_par_table, 
                                            dens.init, dens.targ,
                                            agb.init, agb.targ,
                                            AbvGrndWood.epsilon = 0.05,
                                            trace = FALSE, min.diam)
  
  State_updated <- list(state = Gridcell_updated,
                        pos_list = pos_list,
                        siz_list = siz_list)
  
  write_binary_LPJGUESS(State_updated, file.path(outdir, runid))
  
  ## --- Regenerate config for next run ---
  do.call(write.config.LPJGUESS,
          list(defaults = NULL,
               trait.values = new.params,
               settings = settings,
               run.id   = runid)
  )
  
  if(verbose) PEcAn.logger::logger.info("restart written for", runid)
}
