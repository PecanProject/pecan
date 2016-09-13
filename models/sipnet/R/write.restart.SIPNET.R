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
  
  rundir<-settings$host$rundir
 
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
  prior.sla <- trait.values[[which(names(trait.values)!='soil')[1]]]$SLA[1]
  unit.conv <- 2*(10000/1)*(1/1000)*(3.154*10^7) # kgC/m2/s -> Mg/ha/yr
  
  analysis.save <- list()
  
  if('NPP' %in% variables){
    analysis.save[[1]] <- analysis.vec$NPP#*unit.conv -> Mg/ha/yr
    names(analysis.save[[1]])<-c('NPP')
  }
  
  if('AbvGrndWood' %in% variables){
    analysis.save[[2]] <- analysis.vec$AbvGrndWood*1000 #kgC/m2 -> g/m2 #no (1-.2-.2) because that's on sipnet side
    names(analysis.save[[2]])<-c('plantWood')
  }
  
  if('LeafC' %in% variables){
    analysis.save[[3]] <- analysis.vec$LeafC*prior.sla*2 ## kgC/m2*m2/kg*2kg/kgC -> m2/m2
    if(analysis.vec$LeafC<0) analysis.save[[3]] <- 0
    names(analysis.save[[3]])<-c('lai')
  }
  
  if('Litter' %in% variables){
    analysis.save[[4]] <- analysis.vec$Litter*1000 ##kgC/m2 -> gC/m2
    if(analysis.vec$Litter<0) analysis.save[[4]] <- 0
    names(analysis.save[[4]])<-c('litter')
  }
  
  if('TotSoilCarb' %in% variables){
    analysis.save[[5]] <- analysis.vec$TotSoilCarb*1000 ##kgC/m2 -> gC/m2
    names(analysis.save[[5]])<-c('soil')
  }
  
  if('SoilMoistFrac' %in% variables){
    analysis.save[[6]] <- analysis.vec$SoilMoistFrac ## unitless
    if(analysis.vec$SoilMoistFrac < 0 | analysis.vec$SoilMoistFrac > 1) analysis.save[[6]] <- .5
    names(analysis.save[[6]])<-c('litterWFrac')
    
    analysis.save[[7]] <- analysis.vec$SoilMoistFrac ## unitless
    if(analysis.vec$SoilMoistFrac < 0 | analysis.vec$SoilMoistFrac > 1) analysis.save[[7]] <- .5
    names(analysis.save[[7]])<-c('soilWFrac')
  }
  
  if('SWE' %in% variables){
    analysis.save[[8]] <- analysis.vec$SWE## unitless
    if(analysis.vec$SWE<0) analysis.vec$SWE <- 0
    names(analysis.save[[8]])<-c('snow')
  }
  
 analysis.save.mat <- data.frame(matrix(unlist(analysis.save,use.names=TRUE),nrow=1))
 colnames(analysis.save.mat)<-names(unlist(analysis.save))

  do.call(write.config.SIPNET, args = list(defaults = NULL, trait.values = trait.values,
                                         settings = settings, run.id = runid,
                                         inputs = list(met=list(path=met[grep(time+1,x=met)])),
                                         IC = analysis.save.mat))

    # do.call(my.write.config,args=list(defaults,list(pft=prior[i,],env=NA),
    #                                   settings, run.id[[i]],inputs = settings$run,
    #                                   IC=analysis[i,-1]))
print(runid)
  
}
