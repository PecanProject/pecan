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
#'   rx <- read_restart.LPJGUESS(
#'            outdir   = "/projectnb/…/LPJ_output",
#'            runid    = "123456",
#'            stop.time = as.POSIXct("2001-12-31 23:59:59", tz = "UTC"),
#'            settings = settings,
#'            var.names = c("AGB.pft"),
#'            params = params)
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
  
  # read binary state file, takes a couple of minutes
  Gridcell_container <- read_binary_LPJGUESS(outdir  = file.path(outdir, runid), 
                                             version = lpjguess_ver)
  forecast <- list()
  
  # additional varnames for LPJ-GUESS?
  
  for (var_name in var.names) {
    
    if (var_name == "AGB.pft") {
      
      cmass_sap_perpft   <- calculateGridcellVariablePerPFT(model.state = Gridcell_container$state, variable = "cmass_sap")
      cmass_heart_perpft <- calculateGridcellVariablePerPFT(model.state = Gridcell_container$state, variable = "cmass_heart")
      
      cmass_wood <- cmass_sap_perpft + cmass_heart_perpft
      cmass_wood <- PEcAn.utils::ud_convert(cmass_wood, "kg/m^2", "Mg/ha")
      
      # calculate below ground and subtract
      # 0.23 magic number from Chojnacky Table 6
      cmass_blwg_wood <- cmass_wood * 0.23
      cmass_abvg_wood <- cmass_wood - cmass_blwg_wood
      
      forecast[[length(forecast) + 1]]    <- cmass_abvg_wood
      names(forecast[[length(forecast)]]) <- paste0("AGB.pft.", unlist(Gridcell_container$state$meta_data$pft))
      
    }
  }
  
  # params$LPJGUESS_state include state, pos_list, siz_list
  params$LPJGUESS_state <- Gridcell_container
  
  PEcAn.logger::logger.info("Finished --", runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
  
} # read_restart.LPJGUESS
