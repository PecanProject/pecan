write.restart.LINKAGES<- function(nens,outdir,run.id,time,settings,prior,analysis){
  for(i in 1:nens){
    load(file.path(outdir,run.id[[i]],"linkages.out.Rdata")) #load output
    file.rename(file.path(outdir,run.id[[i]],"linkages.out.Rdata"),file.path(outdir,run.id[[i]],paste0(time[t],"linkages.out.Rdata"))) #save original output
    
   #translate agb to dbh...I know it's questionable
   dbh.total <- (analysis[i,]/.113)^(1/2.4572)
   dbh.inc <- dbh.total/sum(ntrees.kill[,ncol(ntrees.kill),1])
   dbh.save[1:sum(ntrees.kill[,ncol(ntrees.kill),1]),ncol(dbh.save),1] <- dbh.save[1:sum(ntrees.kill[,ncol(ntrees.kill),1]),ncol(dbh.save),1] + dbh.inc
    
   dbh = as.vector(dbh.save[,ncol(dbh.save),1])
   ncohrt <- ncohrt
   tyl <- tyl
   C.mat <- C.mat
   ntrees <- ntrees.kill[,ncol(ntrees.kill),1]
   nogro <- as.vector(nogro.save[,ncol(nogro.save),1])
   ksprt <- matrix(0,1,nspec)
   iage <- as.vector(iage.save[,ncol(iage.save),1])
   
    restart.file <- file.path(settings$rundir,run.id[[i]],"linkages.restart.Rdata")
    sprintf("%s",restart.file)
    
    save(dbh, tyl, ntrees, nogro, ksprt, iage, C.mat = C.mat, tyl = tyl, ncohrt,
         file = restart.file)
   
    #make a new settings with the right years
    #min start date and end date - fail in informative way
   
    settings$run$start.date <- paste0(total.time[t],strftime(settings$run$end.date,"/%m/%d"))
    settings$run$end.date <- paste0(total.time[t+1],strftime(settings$run$end.date,"/%m/%d"))
   
    do.call(my.write.config,args=list(settings=settings,run.id = run.id[[i]],restart = TRUE))   
  }
}
