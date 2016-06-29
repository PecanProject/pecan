read.restart.LINKAGES <- function(outdir,run.id,time,spin.up,X.vec){
  
  ncfiles <- list.files(path = file.path(outdir,run.id), pattern="\\.nc$", full.names=TRUE)
  
  # skip ensemble member if no *.nc files selected/availible  
  if(length(ncfiles) < spin.up-1) forecast = X.vec
  
  forecast = X.vec  
  ens <- read.output(runid = run.id,outdir = file.path(outdir, run.id),
         start.year = time,end.year=time,
         variables=c("AGB.pft")) #change to just "AGB" for plot level biomass
  
  forecast$biomass_tsca = ens$AGB.pft[1]
  forecast$biomass_acsa3 = ens$AGB.pft[2]
  forecast$biomass_beal2 = ens$AGB.pft[4]
  forecast$biomass_thoc2 = ens$AGB.pft[3]
  print(run.id)

  return(forecast)
}
