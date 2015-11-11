write.restart.LINKAGES<- function(nens,outdir,run.id,time,settings,prior,analysis){
  for(i in 1:nens){   
    file.rename(file.path(outdir,run.id[[i]],"sipnet.out"),file.path(outdir,run.id[[i]],paste0("sipnet.out",time[t])))
    file.remove(file.path(settings$rundir,run.id[[i]],"sipnet.clim"))
    do.call(my.write.config,args=list(defaults,list(pft=prior[i,],env=NA),
                                      settings, run.id[[i]],inputs = settings$run,IC=analysis[i,-1]))   
  }
}
