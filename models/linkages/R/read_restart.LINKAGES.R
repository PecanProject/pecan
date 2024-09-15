##' @title read_restart.LINKAGES
##' @name  read_restart.LINKAGES
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @param outdir      output directory
##' @param runid       run ID
##' @param stop.time   year that is being read
##' @param multi.settings    PEcAn settings object
##' @param var.names   var.names to be extracted
##' 
##' @description Read Restart for LINKAGES
##' 
##' @return X.vec      vector of forecasts
##' @export
##' 
read_restart.LINKAGES <- function(outdir, runid, stop.time, settings, var.names = NULL, params = NULL) {
  
  # Read ensemble output
  ens <- read.output(runid = runid, 
                     outdir = file.path(outdir, runid), 
                     start.year = lubridate::year(stop.time), 
                     end.year = lubridate::year(stop.time),
                     variables = var.names, pft.name = unlist(sapply(settings$pfts,'[[', "name")))  # change to just 'AGB' for plot level biomass
  if(!is.na(ens)){
  # Add PFT name to variable if applicable
  pft.names <- numeric(length(settings$pfts))
  for (i in seq_along(settings$pfts)) {
    pft.names[i] <- settings$pfts[i]$pft$name
  }
  #ens.pft.names <- grep("pft", names(ens))
  #names(ens[[grep("pft", names(ens))]]) <- pft.names
  
  forecast <- list()

  if ("Fcomp" %in% var.names) {
    forecast[[length(forecast)+1]] <- ens$AGB.pft #already has C  #* unit.conv 
    names(forecast[[length(forecast)]]) <- paste0('Fcomp.',pft.names)
  }
  
  if ("AGB.pft" %in% var.names) {
    forecast[[length(forecast)+1]] <- ens$AGB.pft #already has C  #* unit.conv 
    names(forecast[[length(forecast)]]) <- paste0('AGB.pft.',pft.names)
  }
    
  if ("TotSoilCarb" %in% var.names) {
    forecast[[length(forecast)+1]] <- ens$TotSoilCarb #PEcAn.utils::ud_convert(ens$TotSoilCarb, "kg/m^2", "Mg/ha") #* unit.conv 
    names(forecast[[length(forecast)]]) <- c("TotSoilCarb")
  }


  }else{
    forecast <- list()
    if ("AGB.pft" %in% var.names) {
      forecast[[length(forecast)+1]] <- rep(NA,length(settings$pfts))
    }
    if ("Fcomp" %in% var.names) {
      forecast[[length(forecast)+1]] <- rep(NA,length(settings$pfts)) #already has C  #* unit.conv
    }
    if ("TotSoilCarb" %in% var.names) {
      forecast[[length(forecast)+1]] <- NA
    }
  }
  # Put forecast into vector
  print(runid)
  X_tmp <- list(X = unlist(forecast), params = params)
  
  return(X_tmp)
}

