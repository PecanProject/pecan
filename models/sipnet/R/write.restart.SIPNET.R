##' @title write.restart.SIPNET
##' @name  write.restart.SIPNET
##' @author Ann Raiho \email{araiho@@nd.edu}
##' 
##' @param out.dir         output directory
##' @param runid           run ID
##' @param time            year that is being read
##' @param settings        PEcAn settings object
##' @param analysis.vec    analysis vector
##' @param RENAME          flag to either rename output file or not
##' @param variables
##' @param sample_parameters
##' @param trait.values
##' @param met
##' 
##' @description Write restart files for SIPNET
##' 
##' @return NONE
##' @export
##' 
write.restart.SIPNET<- function(out.dir, runid, time, settings, analysis.vec,
                                RENAME = TRUE, variables,
                                sample_parameters = FALSE,
                                trait.values = NA, met){
 
  if(RENAME == TRUE) {
    file.rename(file.path(outdir,runid,"sipnet.out"),
                file.path(outdir,runid,paste0("sipnet.out",time)))
    system(paste("rm",file.path(rundir,runid,"sipnet.clim")))
  }else{
    print(paste("Files not renamed -- Need to rerun year",time,"before next time step"))
  }
   #file.remove(file.path(settings$rundir,runid,"sipnet.clim"))
  
  settings$run$start.date <- paste0(time + 1,"/01/01") #TO FIX LATER #DONT WANT TO ASSUME YEARLY TIMESTEP
  settings$run$end.date <- paste0(time + 1,"/12/31")
  
  ## Converting to sipnet units
  if(sample_parameters == TRUE){
    load(file.path(settings$outdir, "samples.Rdata"))
    prior.sla <- mean(ensemble.samples[[which(names(ensemble.samples)!='soil')[1]]]$SLA) #HACK
  }else{
    load(file.path(settings$outdir, paste0("ensemble.samples.",settings$state.data.assimilation$prior,".Rdata")))
    prior.sla <- ens.samples[[which(names(ensemble.samples)!='soil')[1]]]$SLA
  }
  unit.conv <- 2*(10000/1)*(1/1000)*(3.154*10^7) # kgC/m2/s -> Mg/ha/yr
  
  analysis.vec$NPP<-analysis.vec$NPP*unit.conv
  analysis.vec$plantWood<-analysis.vec$plantWood*1000 #kgC/m2 -> gC/m2
  analysis.vec$lai<-analysis.vec$lai*prior.sla*2 ## kgC/m2*m2/kg*2kg/kgC -> m2/m2
  analysis.vec$litter<-analysis.vec$litter*1000 ##kgC/m2 -> gC/m2
  analysis.vec$soil<- analysis.vec$soil*1000 ## kgC/m2 -> gC/m2
  analysis.vec$litterWFrac<-analysis.vec$litterWFrac ## unitless
  analysis.vec$soilWFrac<-analysis.vec$soilWFrac ## unitless
  analysis.vec$snow<-analysis.vec$snow*0.1 ## kg/m2 -> cm
    
  do.call(write.config.SIPNET, args = list(defaults = NULL, trait.values = trait.values,
                                         settings = settings, run.id = runid,
                                         inputs = list(met=list(path=met[grep(time+1,x=met)])),
                                         IC = analysis.vec))

    # do.call(my.write.config,args=list(defaults,list(pft=prior[i,],env=NA),
    #                                   settings, run.id[[i]],inputs = settings$run,
    #                                   IC=analysis[i,-1]))
print(runid)
  
}
