#' @title State data assimilation read-restart for ED2
#'
#' @author Alexey Shiklomanov, Istem Fer
#' @inheritParams PEcAn.ModelName::read_restart.ModelName
#' @examples
#' \dontrun{
#'   outdir <- "~/sda-hackathon/outputs"
#'   runid <- "99000000020"
#'   settings_file <- "outputs/pecan.CONFIGS.xml"
#'   settings <- PEcAn.settings::read.settings(settings_file)
#'   forecast <- read_restart.ED2(...)
#' }
#' 
#' @export
read_restart.ED2 <- function(outdir, 
                             runid,
                             stop.time,
                             settings, 
                             var.names, 
                             params) {
  
  # depends on code run on local or remote, currently runs locally
  rundir <- settings$rundir
  mod_outdir <- settings$modeloutdir # is there a case this is different than outdir?
  
  
  histfile <- get_restartfile.ED2(mod_outdir, runid, stop.time)
  if (is.null(histfile)) {
    PEcAn.logger::logger.severe("Failed to find ED2 history restart file.")
  }
  
  
  pft_names <- sapply(settings$pfts, '[[', 'name')
  
  
  # var.names <- c("AbvGrndWood", "GWBI", "TotLivBiom", "leaf_carbon_content") 
  histout <- read_S_files(sfile       = basename(histfile), 
                          outdir      = dirname(histfile), 
                          pft_names   = pft_names, 
                          pecan_names = var.names)
  
  # unit conversions and other aggregations
  forecast <- list()
  
  for (var_name in var.names) {
    
    # should there be a tag passed via settings to check for per pft assimilation vs totals?
    perpft <- FALSE # for now just working with totals for HF tree-ring DA
    
    if (var_name == "AGB") {
      
      forecast_tmp                        <- switch(perpft+1, sum(histout$AGB, na.rm = TRUE), histout$AGB) # kgC/m2
      forecast[[length(forecast)+1]]      <- udunits2::ud.convert(forecast_tmp, "kg/m^2", "Mg/ha") # conv to MgC/ha 
      names(forecast)[length(forecast)]   <- switch(perpft+1, "AGB", paste0("AGB.", pft_names))
      
    }
    
    if (var_name == "TotLivBiom") {
      
      forecast_tmp                        <- switch(perpft+1, sum(histout$TotLivBiom, na.rm = TRUE), histout$TotLivBiom) # kgC/m2
      forecast[[length(forecast)+1]]      <- udunits2::ud.convert(forecast_tmp, "kg/m^2", "Mg/ha") # conv to MgC/ha 
      names(forecast)[length(forecast)]   <- switch(perpft+1, "TotLivBiom", paste0("TotLivBiom.", pft_names))
      
    }
    
    if (var_name == "AbvGrndWood") {
      
      forecast_tmp                        <- switch(perpft+1, sum(histout$AbvGrndWood, na.rm = TRUE), histout$AbvGrndWood) # kgC/m2
      forecast[[length(forecast)+1]]      <- udunits2::ud.convert(forecast_tmp, "kg/m^2", "Mg/ha") # conv to MgC/ha 
      names(forecast)[length(forecast)]   <- switch(perpft+1, "AbvGrndWood", paste0("AbvGrndWood.", pft_names))
      
    }
    
    if (var_name == "leaf_carbon_content") {
      
      forecast_tmp                        <- switch(perpft+1, sum(histout$leaf_carbon_content, na.rm = TRUE), histout$leaf_carbon_content) # kgC/m2
      forecast[[length(forecast)+1]]      <- udunits2::ud.convert(forecast_tmp, "kg/m^2", "Mg/ha") # conv to MgC/ha 
      names(forecast)[length(forecast)]   <- switch(perpft+1, "leaf_carbon_content", paste0("leaf_carbon_content.", pft_names))
      
    }
    
    if (var_name == "storage_carbon_content") {
      
      forecast[[length(forecast)+1]]      <- switch(perpft+1, sum(histout$storage_carbon_content, na.rm = TRUE), histout$storage_carbon_content) # kgC/m2
      names(forecast)[length(forecast)]   <- switch(perpft+1, "storage_carbon_content", paste0("storage_carbon_content.", pft_names))
      
    }
    
    
    if (var_name == "GWBI") {
      
      forecast_tmp                        <- switch(perpft+1, sum(histout$GWBI, na.rm = TRUE), histout$GWBI) # kgC/m2/yr
      forecast[[length(forecast)+1]]      <- udunits2::ud.convert(forecast_tmp, "kg/m^2/yr", "Mg/ha/yr") # conv to MgC/ha/yr 
      names(forecast)[length(forecast)]   <- switch(perpft+1, "GWBI", paste0("GWBI.", pft_names))
      
    }
    
    if (var_name == "fast_soil_pool_carbon_content") {
      
      forecast[[length(forecast)+1]]      <- histout$fast_soil_pool_carbon_content # kgC/m2
      names(forecast)[length(forecast)]   <- "fast_soil_pool_carbon_content"
      
    }
    
    if (var_name == "structural_soil_pool_carbon_content") {
      
      forecast[[length(forecast)+1]]      <- histout$structural_soil_pool_carbon_content # kgC/m2
      names(forecast)[length(forecast)]   <- "structural_soil_pool_carbon_content"
      
    }
    
    
  } # var.names loop
  
  restart <- list()
  # pass certain things for write_restart to use (so that there we don't have to re-read and re-calculate stuff)
  # IMPORTANT NOTE: in the future, these "certain things" need to be confined to old states that will be used
  # to carry out deternimistic relationships, no other read/write restart should copy this logic
  restart$restart  <- histout$restart
  restart$histfile <- histfile
  
  params$restart <- restart
  
  PEcAn.logger::logger.info("Finished --", runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
  
} # read_restart.ED2


