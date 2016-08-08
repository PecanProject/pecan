##' @title read.restart.SIPNET
##' @name  read.restart.SIPNET
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @param outdir      output directory
##' @param runid       run ID
##' @param time        year that is being read
##' @param settings    PEcAn settings object
##' @param variables   variables to be extracted
##' 
##' @description Read Restart for SIPNET
##' 
##' @return X.vec      vector of forecasts
##' @export
##' 
read.restart.SIPNET <- function(outdir,runid,time,settings,variables,sample_parameters=NULL){

  if(sample_parameters == TRUE){
    load(file.path(settings$outdir, "samples.Rdata"))
    prior.sla <- mean(ensemble.samples[[which(names(ensemble.samples)!='soil')[1]]]$SLA) #HACK
  }else{
    load(file.path(settings$outdir, paste0("ensemble.samples.",settings$state.data.assimilation$prior,".Rdata")))
    prior.sla <- ens.samples[[which(names(ensemble.samples)!='soil')[1]]]$SLA
  }
  
  forecast <- list()
  unit.conv <-  0.001*2#  kgC/ha/yr to Mg/ha/yr
  
  #Read ensemble output
  ens <- read.output(runid = runid,outdir = file.path(outdir, runid),
                     start.year = time, end.year=time,
                     variables=variables)
    
    last = length(ens$NPP)
    
    forecast<-numeric(8)
    
    unit.conv <- (10000/1)*(1/10000)*(365.25*24*60*60)
  
    #### PEcAn Standard Outputs
    forecast[1] <- mean(ens$NPP) * unit.conv ## kg m-2 s-1 -> Mg/ha/yr
    forecast[2] = ens$AbvGrndWood[last] / (1 - .2 - .2) ## kgC/m2
    forecast[3] = ens$LeafC[last] ## kgC/m2*m2/kg*2kg/kgC
    forecast[4] = ens$Litter[last]##kgC/m2
    forecast[5] = ens$TotSoilCarb[last]## kgC/m2
    forecast[6] = ens$SoilMoistFrac[last]## unitless
    forecast[7] = ens$SoilMoistFrac[last]## unitless
    forecast[8] = ens$SWE[last]## kg/m^2
  
  names(forecast)<-c("NPP","plantWood","lai","litter","soil","litterWFrac","soilWFrac","snow")
  X.vec = forecast
  
  print(runid)

  return(X.vec)
}
