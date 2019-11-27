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

  # Read ensemble output
  ens <- read.output(runid = runid, 
                     outdir = file.path(outdir, runid), 
                     start.year = lubridate::year(stop.time), 
                     end.year = lubridate::year(stop.time),
                     variables = var.names)
  
  last <- length(ens[[1]])
  
  params$restart <- c()
  
  #if("TotSoilCarb" %in% var.names){
  #  # fast/slow pool fractions needed maybe extract here or in write_restart
  #}
  
  if ("LAI" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$LAI[last] ## m2 m-2 
    names(forecast[[length(forecast)]]) <- c("LAI")
  }
  
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast) + 1]] <- ens$TotSoilCarb[last] # kg C m-2
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }
  
  PEcAn.logger::logger.info(runid)
  
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
  
} # read_restart.BASGRA
