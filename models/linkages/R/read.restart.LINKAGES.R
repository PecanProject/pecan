read.restart.LINKAGES <- function(outdir,run.id,time,unit.conv,IC,prior){
  
  forecast = IC
  nens = nrow(IC)
  ens = list()
  
  for(i in 1:nens){
    ens[[i]] <- read.output(runid = run.id[[i]],outdir = file.path(outdir, run.id[[i]]),
                            start.year = time[i],end.year=time[i],
                            variables=c("AGB"))
    last = length(ens[[i]]$AGB)
    forecast$AGB[i] = ens[[i]]$AGB[last]*1000
  }
  
  X = forecast
  
  return(X)
}
