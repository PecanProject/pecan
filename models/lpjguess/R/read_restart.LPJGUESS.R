#' Read Restart for LPJGUESS
#'
#' @param outdir      output directory
#' @param runid       run ID
#' @param stop.time   year that is being read
#' @param settings    PEcAn settings object
#' @param var.names   var.names to be extracted
#' @param params      passed on to return value
#'
#' @return X_tmp      vector of forecasts
#' @export
#' @examples
#' \dontrun{
#' settings <- PEcAn.settings::read.settings("pecan.xml")
#' params <- list()
#' rx <- read_restart.LPJGUESS(
#'          outdir   = "/projectnb/…/LPJ_output",
#'          runid    = "123456",
#'          stop.time = as.POSIXct("2001-12-31 23:59:59", tz = "UTC"),
#'          settings = settings,
#'          var.names = c("AGB.pft"),
#'          params = params)
#' }
#' @author Istem Fer, Yinghao Sun
read_restart.LPJGUESS <- function(outdir, runid, stop.time, settings, var.names, params){
  
  # which LPJ-GUESS version, the structure of state file depends a lot on version
  lpjguess_ver <- settings$model$revision
  
  # check if files required by read_binary_LPJGUESS exist
  needed_files <- paste0(c("guess.", "guess.", "parameters."), lpjguess_ver, c(".cpp", ".h", ".h"))
  file_check <- file.exists(system.file(needed_files, package = "PEcAn.LPJGUESS"))
  if(!all(file_check)){
    PEcAn.logger::logger.severe("read_binary_LPJGUESS need :", paste(needed_files[!file_check], collapse = " "))
  }
  
  # run directory for this specific ensemble member
  rundir_member <- file.path(settings$host$rundir, runid)
  # state directory for this specific ensemble member and year
  state_dir <- file.path(outdir, runid, "state", lubridate::year(stop.time))
  
  # Call read_binary_LPJGUESS with the correct rundir 
  if (dir.exists(state_dir)) {
    Gridcell_container <- read_binary_LPJGUESS(
      outdir = state_dir,
      rundir = rundir_member, # Pass the correct run directory
      version = lpjguess_ver
    )
  } else {
    PEcAn.logger::logger.warn("Binary state directory not found, skipping read:", state_dir)
    return(list(X = NA, params = params)) # Return NA if state doesn't exist
  }
  
  # DEBUG
  inds <- unlist(lapply(Gridcell_container$state$Stand[[1]]$Patch,
                        function(p) p$Vegetation$Individuals), recursive = FALSE)
  bad_idx <- which(!sapply(inds, function(ind) is.finite(ind$height)))
  length(bad_idx)  # See how many bad individuals there are
  if (length(bad_idx)) utils::str(inds[[bad_idx[1]]][c("indiv.pft.id","alive","height",
                                                "cmass_leaf","cmass_root","cmass_sap","cmass_heart")])
  
  
  ## ---- Build PFT parameter table from new.params ----
  # TODO: find accurate parameters; read params from settings
  pft_par_table <- data.frame()
  # PFTs <- c("Ace_rub","Bet_all","Fag_gra","Que_rub","Tsu_can")
  PFTs <- names(params)
  for(PFT in PFTs) {
    this.param.row <- c()
    this.param.row["sla"] <- params[[PFT]]$SLA
    # leaflong <- params[[PFT]]$leaf_longevity
    # this.param.row["sla"] <- 0.2 * exp(6.15 - 0.46 * log(12 * leaflong))
    this.param.row["k_latosa"] <- params[[PFT]]$sapwood_ratio
    this.param.row["wooddens"] <- params[[PFT]]$wood_density    #kg/m-3
    # this.param.row["wooddens"] <- 0.2 #g/cm-3
    this.param.row["lifeform"] <- 1
    this.param.row["k_rp"] <- params[[PFT]]$k_rp
    this.param.row["k_allom1"] <- params[[PFT]]$k_allom1
    this.param.row["k_allom2"] <- params[[PFT]]$k_allom2
    this.param.row["k_allom3"] <- params[[PFT]]$k_allom3
    this.param.row["crownarea_max"] <- params[[PFT]]$crownarea_max

    pft_par_table <- rbind(pft_par_table , this.param.row)
  }
  names(pft_par_table) <- c("sla", "k_latosa", "wooddens", "lifeform", "k_rp", "k_allom1", "k_allom2", "k_allom3", "crownarea_max") 
  rownames(pft_par_table) <- PFTs
  
  
  # Calculate AGB from the read state
  forecast <- list()
  
  # additional varnames for LPJ-GUESS?
  
  for (var_name in var.names) {
    
    if (grepl("AGB", var_name)) {
      
      # cmass_sap_perpft   <- calculateGridcellVariablePerPFT(model.state = Gridcell_container$state, pft.params = pft_par_table, variable = "cmass_sap")
      # cmass_heart_perpft <- calculateGridcellVariablePerPFT(model.state = Gridcell_container$state, pft.params = pft_par_table, variable = "cmass_heart")
      # 
      # cmass_wood <- cmass_sap_perpft + cmass_heart_perpft
      # # cmass_wood <- PEcAn.utils::ud_convert(cmass_wood, "kg/m^2", "Mg/ha")
      # 
      # # calculate below ground and subtract
      # # 0.23 magic number from Chojnacky Table 6
      # cmass_blwg_wood <- cmass_wood * 0.23
      # cmass_abvg_wood <- cmass_wood - cmass_blwg_wood
      
      cmass_abvg_wood <- calculateGridcellVariablePerPFT(model.state = Gridcell_container$state, 
                                                         pft.params = pft_par_table, 
                                                         variable = "AbvGrndWood",
                                                         min.diam = 5)
      
      ### Assign standard names to AGB per PFT
      pft_names <- paste0("AGB.pft.", unlist(Gridcell_container$state$meta_data$pft))
      names(cmass_abvg_wood) <- pft_names
      
      # forecast[[length(forecast) + 1]]    <- cmass_abvg_wood
      # names(forecast[[length(forecast)]]) <- paste0("AGB.pft.", unlist(Gridcell_container$state$meta_data$pft))
      if (var_name == "AGB") {
        # If the request is a universal "AGB", return a complete named vector containing all PFT data
        forecast[[length(forecast) + 1]] <- cmass_abvg_wood
      } else {
        # If the request is a specific PFT name ("AGB.pft.Ace_rub")
        # check whether the requested PFT exists in the calculation result
        if (var_name %in% names(cmass_abvg_wood)) {
          # If exists, only extract the value of that PFT and store it in the forecast list
          forecast[[length(forecast) + 1]] <- cmass_abvg_wood[var_name]
        } else {
          # If the requested PFT does not exist, give a warning
          PEcAn.logger::logger.warn("Requested AGB variable '", var_name, "' not found among active PFTs in this run. Skipping.")
        }
      }
      
      
    }
  }
  
  # Package results: params$LPJGUESS_state include state, pos_list, siz_list
  params$LPJGUESS_state <- Gridcell_container
  
  PEcAn.logger::logger.info("Finished reading restart for --", runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
  
} # read_restart.LPJGUESS
