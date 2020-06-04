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

  year <- lubridate::year(stop.time)

  start_date  <- as.POSIXlt(settings$run$start.date, tz = "UTC")
  end_date    <- as.POSIXlt(settings$run$end.date, tz = "UTC")
  start_year  <- lubridate::year(start_date)
  end_year    <- lubridate::year(end_date)
    
  # Read ensemble output
  ens <- read.output(runid = runid, 
                     outdir = file.path(outdir, runid), 
                     start.year = lubridate::year(stop.time), 
                     end.year = lubridate::year(stop.time),
                     variables = var.names)
  
  if(year == start_year & year != end_year){
    simdays <- seq(lubridate::yday(start_date), lubridate::yday(stop.time))
  }
  # To BE CONTINUED...
  #else if(year != start_year & year == end_year){
  #  simdays <- seq(1, lubridate::yday(end_date))
  #}else{
  #  simdays <- seq(lubridate::yday(start_date), lubridate::yday(end_date))
  #}
  
  last <- length(simdays)
  
  params$restart <- c()
  
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
