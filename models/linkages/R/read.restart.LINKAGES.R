read.restart.LINKAGES <- function(outdir,run.id,time,IC,prior){
  
  forecast = IC
  nens = nrow(IC)
  ens = list()
  
  for(i in 1:nens){
    ncfiles <- list.files(path = file.path(outdir,run.id[[i]]), pattern="\\.nc$", full.names=TRUE)
    # skip ensemble member if no *.nc files selected/availible
    if(length(ncfiles) == 0) next
    
    ens[[i]] <- read.output(runid = run.id[[i]],outdir = file.path(outdir, run.id[[i]]),
                            start.year = time,end.year=time,
                            variables=c("AGB"))
    last = length(ens[[i]]$AGB)
    forecast$AGB[i] = ens[[i]]$AGB[last]
  }
  
  X = forecast
  
  return(X)
}
