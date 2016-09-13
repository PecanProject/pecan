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
  
  #Read ensemble output
  ens <- read.output(runid = runid,outdir = file.path(outdir, runid),
                     start.year = time, end.year=time,
                     variables=variables)
    
    last = length(ens$NPP)
    
    forecast<-list()
    
    unit.conv <- (10000/1)*(1/10000)*(365.25*24*60*60)
  
    #### PEcAn Standard Outputs
    if("NPP" %in% variables){
      forecast[[1]] <- mean(ens$NPP) * unit.conv ## kgC m-2 s-1 -> MgC/ha/yr
      names(forecast[[1]])<-c("NPP")
    }
    
    if("AbvGrndWood" %in% variables){
      forecast[[2]] = ens$AbvGrndWood[last] / (1 - .2 - .2) ## kgC/m2
      names(forecast[[2]])<-c("AbvGrndWood")
    }
    
    if("LeafC" %in% variables){
      forecast[[3]] = ens$LeafC[last]## kgC/m2*m2/kg*2kg/kgC
      names(forecast[[3]])<-c("LeafC")
    }
    
    if("Litter" %in% variables){
      forecast[[4]] = ens$Litter[last]##kgC/m2
      names(forecast[[4]])<-c("Litter")
    }
    
    if("TotSoilCarb" %in% variables){
      forecast[[5]] = ens$TotSoilCarb[last]## kgC/m2
      names(forecast[[5]])<-c("TotSoilCarb")
    }
    
    if("SoilMoistFrac" %in% variables){
      forecast[[6]] = ens$SoilMoistFrac[last]## kgC/m2
      names(forecast[[6]])<-c("SoilMoistFrac")
    }
    
    if("SWE" %in% variables){
      forecast[[7]] = ens$SWE[last]## kgC/m2
      names(forecast[[7]])<-c("SWE")
    }
  
  X.vec = unlist(forecast)
  
  print(runid)

  return(X.vec)
}
