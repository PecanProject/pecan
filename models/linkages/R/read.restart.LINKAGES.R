##' @title read.restart.LINKAGES
##' @name  read.restart.LINKAGES
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
read.restart.LINKAGES <- function(outdir,runid,stop.time,settings,var.names=NULL,params=NULL){
 
  #Read ensemble output
  ens <- read.output(runid = runid,outdir = file.path(outdir, runid),
         start.year = year(stop.time),
         end.year=year(stop.time),
         variables = var.names) #change to just "AGB" for plot level biomass
  
  #Add PFT name to variable if applicable
  pft.names <- numeric(length(settings$pfts))
  for(i in 1:length(settings$pfts)){
    pft.names[i] <- settings$pfts[i]$pft$name
  }
  ens.pft.names <- grep("pft",names(ens))
  names(ens[[grep("pft",names(ens))]]) <- pft.names
  
  #Put forecast into vector
  X.vec <- t(unlist(ens))
  
  print(runid)

  return(X.vec)
}
