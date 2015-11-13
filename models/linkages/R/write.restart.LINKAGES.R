write.restart.LINKAGES<- function(nens,outdir,run.id,time,settings,prior,analysis){
  for(i in 1:nens){
    load(file.path(outdir,run.id[[i]],"linkages.out.Rdata")) #load output
    file.rename(file.path(outdir,run.id[[i]],"linkages.out.Rdata"),file.path(outdir,run.id[[i]],paste0(time[t],"linkages.out.Rdata"))) #save original output
    
    ncohrt <- ncohrt
    tyl <- tyl
    C.mat <- C.mat
    ntrees <- ntrees.kill[,ncol(ntrees.kill),1]
    nogro <- as.vector(nogro.save[,ncol(nogro.save),1])
    ksprt <- matrix(0,1,nspec)
    iage <- as.vector(iage.save[,ncol(iage.save),1])
    
    dbh = as.vector(dbh.save[,ncol(dbh.save),1])
    
    biomass_function<-function(dbh){
      .1193 * dbh^2.393 + ((slta+sltb*dbh)/2)^2 * 3.14 * fwt * frt * .001
    }
    merit<-function(dbh){
      (b_obs - biomass_function(dbh))^2
    }
   #translate agb to dbh...I know it's questionable
   b_obs1 <- f.comp[,ncol(f.comp)]*analysis[i,]
   dbh_spp <- numeric(length(b_obs))
   
   nl = 1
   for(s in 1:nspec){
     if(ntrees[s]==0) next
     slta <- spp.params$SLTA[s]
     sltb <- spp.params$SLTB[s]
     fwt <- spp.params$FWT[s]
     frt <- spp.params$FRT[s]
     b_obs <- b_obs1[s]
     dbh_spp[s] <- optimize(merit, c(0,200))$minimum
     nu = nl + ntrees[s] - 1
     for(j in nl:nu){
       dbh[j] <- dbh[j] + (dbh_spp[s]/ntrees[s])
     }
     nl = nu+1
   }
   

   
   dbh.inc <- dbh.total/sum(ntrees.kill[,ncol(ntrees.kill),1])
   dbh[1:sum(ntrees.kill[,ncol(ntrees.kill),1]),ncol(dbh.save),1] <- dbh.save[1:sum(ntrees.kill[,ncol(ntrees.kill),1]),ncol(dbh.save),1] + dbh.inc
    
   
   
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
