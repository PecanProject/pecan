
for(i in 1:nens){
  #file.remove(file.path(outdir,run.id[[i]],"linkages.out.Rdata"))
  #file.remove(file.path(outdir,run.id[[i]],"1860linkages.out.Rdata"))
  
  file.rename(file.path(outdir,run.id[[i]],"1910linkages.out.Rdata"),
              file.path(outdir,run.id[[i]],"linkages.out.Rdata")) #save original output
  for(t in 1:15){
    file.remove(file.path(outdir,run.id[[i]],paste0(total.time[t],".nc")))
    file.remove(file.path(outdir,run.id[[i]],paste0(total.time[t],".nc.var")))
    file.remove(file.path(outdir,run.id[[i]],paste0(total.time[t],"linkages.out.Rdata")))
  }
  
}
