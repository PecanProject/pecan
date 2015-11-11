write.restart.LINKAGES<- function(nens,outdir,run.id,time,settings,prior,analysis){
  for(i in 1:nens){
    load(file.path(outdir,run.id[[i]],"linkages.out.Rdata")) #load output
    file.rename(file.path(outdir,run.id[[i]],"linkages.out.Rdata"),file.path(outdir,run.id[[i]],paste0("linkages.out.Rdata",time[t]))) #save original output
    
    #translate agb to dbh...I know it's questionable
   dbh.total <- (analysis[i,-1]/.113)^(1/2.4572)
   dbh.inc <- dbh.total/sum(ntrees.kill[,length(time),1])
   dbh.save[1:sum(ntrees.kill[,length(time),1]),length(time),1] <- dbh.save[1:sum(ntrees.kill[,length(time),1]),length(time),1] + dbh.inc
    
   dbh = dbh.save
   ncohrt <- ncohrt
   tyl <- tyl
   C.mat <- C.mat
   ntrees <- ntrees.kill[,length(time),1]
   nogro <- nogro.save[,length(time),1]
   ksprt <- ksprt <- matrix(0,1,nspec)
   iage <- iage.save[,length(time),1]
   
    output.file <- file.path(outdir,"linkages.out.Rdata")
    sprintf("%s",output.file)
    
    save(dbh, tyl, ntrees, nogro, ksprt, iage, C.mat = C.mat, tyl = tyl, ncohrt,
         file = output.file)
   
    do.call(my.write.config,args=list(settings=settings,run.id = run.id[[i]],restart = TRUE))   
  }
}
