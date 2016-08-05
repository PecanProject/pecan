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
  
    forecast[1] <- mean(ens$NPP)*unit.conv ## kg C m-2 s-1 -> Mg/ha/yr [Check]
    forecast[2] = ens$AbvGrndWood[last]*1000 ## kgC/m2 -> gC/m2
    forecast[3] = ens$LeafC[last]*prior.sla*2 ## kgC/m2*m2/kg*2kg/kgC -> m2/m2
    forecast[4] = ens$Litter[last]*1000 ##kgC/m2 -> gC/m2
    forecast[5] = ens$TotSoilCarb[last]*1000 ## kgC/m2 -> gC/m2
    forecast[6] = ens$SoilMoistFrac[last] ## unitless
    forecast[7] = ens$SWE[last]*0.1 ## kg/m2 -> cm
    
    forecast[8] = runif(1,0,0.01) #snow
    #forecast$microbe = NA
  
  names(forecast)<-c("NPP","plantWood","lai","litter","soil","litterWFrac","soilWFrac","snow") #,'snow'
  X.vec = forecast
  
  print(runid)

  return(X.vec)
}
