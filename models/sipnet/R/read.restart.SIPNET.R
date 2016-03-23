read.restart.SIPNET <- function(outdir,run.id,time,IC,prior){

  forecast = IC
  nens = nrow(IC)
  ens = list()
  unit.conv <-  0.001*2#  kgC/ha/yr to Mg/ha/yr
  
  for(i in 1:nens){
    ens[[i]] <- read.output(run.id[[i]],file.path(outdir, run.id[[i]]),
                            start.year = time,end.year=time,
                            variables=c("NPP","AbvGrndWood","TotSoilCarb","LeafC","SoilMoistFrac","SWE","Litter")
    )
    NPPm[i] <- mean(ens[[i]]$NPP)*unit.conv ## kg C m-2 s-1 -> Mg/ha/yr [Check]
    last = length(ens[[i]]$NPP)
    forecast$plantWood[i] = ens[[i]]$AbvGrndWood[last]*1000 ## kgC/m2 -> gC/m2
    forecast$lai[i] = ens[[i]]$LeafC[last]*prior$SLA[i]*2 ## kgC/m2*m2/kg*2kg/kgC -> m2/m2
    forecast$litter[i] = ens[[i]]$Litter[last]*1000 ##kgC/m2 -> gC/m2
    forecast$soil[i] = ens[[i]]$TotSoilCarb[last]*1000 ## kgC/m2 -> gC/m2
    forecast$litterWFrac[i] = ens[[i]]$SoilMoistFrac[last] ## unitless
    forecast$soilWFrac[i] = ens[[i]]$SoilMoistFrac[last] ## unitless
    forecast$snow[i] = ens[[i]]$SWE[last]*0.1 ## kg/m2 -> cm
    #forecast$microbe[i] = NA
  }
  
  X = cbind(NPPm,forecast)
  X$snow = runif(nens,0,0.01)

  return(X)
}
