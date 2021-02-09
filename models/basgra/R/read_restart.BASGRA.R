##' @title Read restart function for SDA with BASGRA
##' 
##' @author Istem Fer
##' 
##' @inheritParams PEcAn.ModelName::read_restart.ModelName
##' 
##' @description Read Restart for BASGRA
##' 
##' @return X.vec      vector of forecasts
##' @export
##' 
read_restart.BASGRA <- function(outdir, runid, stop.time, settings, var.names, params) {
  
  forecast <- list()

  # maybe have some checks here to make sure the first run is actually ran for the period you requested
    
  # Read ensemble output
  ens <- PEcAn.utils::read.output(runid = runid,
                                  outdir = file.path(outdir, runid), 
                                  start.year = lubridate::year(stop.time), 
                                  end.year = lubridate::year(stop.time),
                                  variables = var.names)
  
  last <- length(ens[[1]])
  
  params$restart <- c()
  
  if ("LAI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$LAI[last] ## m2 m-2 
    names(forecast[[length(forecast)]]) <- c("LAI")
  }
  
  if ("fast_soil_pool_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$fast_soil_pool_carbon_content[last] # kg C m-2
    names(forecast[[length(forecast)]]) <- c("fast_soil_pool_carbon_content")
  }
  
  if ("slow_soil_pool_carbon_content" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$slow_soil_pool_carbon_content[last] # kg C m-2
    names(forecast[[length(forecast)]]) <- c("slow_soil_pool_carbon_content")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last]  # kg C m-2
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }
  
  PEcAn.logger::logger.info(runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
  
} # read_restart.BASGRA
