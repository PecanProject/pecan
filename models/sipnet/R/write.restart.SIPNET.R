write.restart.SIPNET<- function(out.dir, runid, time, settings, analysis.vec,
                                RENAME = TRUE, PLOT = FALSE, variables,
                                sample_parameters = FALSE,
                                trait.values = NA, my.write.config, met){
 
  if(RENAME == TRUE) {
    file.rename(file.path(outdir,runid,"sipnet.out"),
                file.path(outdir,runid,paste0("sipnet.out",time)))
    system(paste("rm",file.path(rundir,runid,"sipnet.clim")))
  }else{
    print(paste("Files not renamed -- Need to rerun year",time,"before next time step"))
  }
   #file.remove(file.path(settings$rundir,runid,"sipnet.clim"))
  
  settings$run$start.date <- paste0(time + 1,"/01/01")
  settings$run$end.date <- paste0(time + 1,"/12/31")
    
  if(sample_parameters == TRUE){
    load(file.path(settings$outdir, "samples.Rdata"))
    do.call(my.write.config, args = list(defaults = NULL, trait.values = trait.values,
                                         settings = settings, run.id = runid,
                                         inputs = list(met=list(path=met[grep(time+1,x=met)])),
                                         IC = analysis.vec))
  } else {
    load(file.path(settings$outdir, paste0("ensemble.samples.",settings$state.data.assimilation$prior,".Rdata")))
    do.call(my.write.config,args=list(defaults = NULL, trait.values = ens.samples, 
                                      settings=settings,run.id = runid,restart=FALSE,
                                      inputs = list(met=list(path=met[grep(time+1,x=met)])),
                                      IC = analysis.vec))
  }
  
    # do.call(my.write.config,args=list(defaults,list(pft=prior[i,],env=NA),
    #                                   settings, run.id[[i]],inputs = settings$run,
    #                                   IC=analysis[i,-1]))
print(runid)
  
}
