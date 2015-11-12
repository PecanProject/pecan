read.restart.LINKAGES <- function(outdir,run.id,time,IC,prior){
  
  forecast = IC
  nens = nrow(IC)
  ens = list()
  
  for(i in 1:nens){
    ens[[i]] <- read.output(runid = run.id[[i]],outdir = file.path(outdir, run.id[[i]]),
                            start.year = time,end.year=time,
                            variables=c("AGB"))
    last = length(ens[[i]]$AGB)
    forecast$AGB[i] = ens[[i]]$AGB[last]
  }
  
  X = forecast
  
  return(X)
}
