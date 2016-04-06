##' @title read.restart.LINKAGES
##' @name  read.restart.LINKAGES
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @param outdir      output directory
##' @param runid       run ID
##' @param time        year that is being read
##' @param settings    PEcAn settings object
##' @param variables   variables to be extracted
##' 
##' @description Read Restart for LINKAGES
##' 
##' @return X.vec      vector of forecasts
##' 
read.restart.LINKAGES <- function(outdir,runid,time,settings,variables){
 
  #Read ensemble output
  ens <- read.output(runid = runid,outdir = file.path(outdir, runid),
         start.year = time, end.year=time,
         variables=variables) #change to just "AGB" for plot level biomass
  
  #Add PFT name to variable if applicable
  pft.names <- numeric(length(settings$pft))
  for(i in 1:length(settings$pft)){
    pft.names[i] <- settings$pft[i]$pft$name
  }
  ens.pft.names <- grep("pft",names(ens))
  names(ens[[grep("pft",names(ens))]]) <- pft.names
  
  #Put forecast into vector
  X.vec <- t(unlist(ens))
  
  print(runid)

  return(X.vec)
}
