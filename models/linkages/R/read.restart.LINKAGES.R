read.restart.LINKAGES <- function(outdir,run.id,time,IC,prior,spinup){
  
  forecast = IC
  nens = nrow(IC)
  ens = list()
  
  for(i in 1:nens){
    ncfiles <- list.files(path = file.path(outdir,run.id[[i]]), pattern="\\.nc$", full.names=TRUE)
    # skip ensemble member if no *.nc files selected/availible
    if(length(ncfiles) < spinup) next
    
    ens[[i]] <- read.output(runid = run.id[[i]],outdir = file.path(outdir, run.id[[i]]),
                            start.year = time,end.year=time,
                            variables=c("AGB.pft")) #change to just "AGB" for plot level biomass
    #last = length(ens[[i]]$AGB.pft)
    forecast$biomass_tsca[i] = ens[[i]]$AGB.pft[1]
    forecast$biomass_acsa3[i] = ens[[i]]$AGB.pft[2]
    forecast$biomass_beal2[i] = ens[[i]]$AGB.pft[4]
    forecast$biomass_thoc2[i] = ens[[i]]$AGB.pft[3]
    print(run.id[[i]])
  }
  
  X = forecast
  
  return(X)
}
