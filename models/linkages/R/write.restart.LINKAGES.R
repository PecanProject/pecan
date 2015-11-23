write.restart.LINKAGES <- function(nens,outdir,run.id,time,settings,prior,analysis){
  
  
  ##HACK
  spp.params.default <- read.csv(system.file("spp_matrix.csv", package = "linkages")) #default spp.params
  nspec <- length(settings$pfts)
  spp.params.save <- numeric(nspec)
  for(i in 1:nspec){
    spp.params.save[i] <- which(spp.params.default[,1]%in%settings$pfts[i]$pft$name)
  }
  
  spp.params <- spp.params.default[spp.params.save,]
  ##HACK
  
  
  for(i in 1:nens){
    # skip ensemble member if no file availible  
    outfile = file.path(outdir,run.id[[i]],"linkages.out.Rdata")
    if(!file.exists(outfile)) next
        
    #load output
    load(outfile) 
    #save original output
    file.rename(file.path(outdir,run.id[[i]],"linkages.out.Rdata"),
                file.path(outdir,run.id[[i]],paste0(time,"linkages.out.Rdata"))) 
    
    nspec <- length(settings$pfts)
    ncohrt <- ncohrt
    tyl <- tyl
    C.mat <- C.mat
    ntrees <- ntrees.kill[,ncol(ntrees.kill),1]  # number of trees
    nogro <- as.vector(nogro.save[,ncol(nogro.save),1])  ## no growth indicator
    ksprt <- matrix(0,1,nspec)    ## kill sprout indicator
    iage <- as.vector(iage.save[,ncol(iage.save),1]) # individual age
    
    dbh = as.vector(dbh.save[,ncol(dbh.save),1])
    
    biomass_function<-function(dbh){
      .1193 * dbh^2.393 + ((slta+sltb*dbh)/2)^2 * 3.14 * fwt * frt * .001
    }
    merit<-function(dbh,b_obs){
      (b_obs - biomass_function(dbh))^2
    }
   #translate agb to dbh...I know it's questionable
#   diff <- analysis[i,] - ag.biomass[nrow(ag.biomass),]
#   b_obs1 <- f.comp[,ncol(f.comp)]*diff   ## f.comp = fractional composition
#   dbh_spp <- numeric(length(b_obs1))
   bcorr = analysis[i,] / ag.biomass[nrow(ag.biomass),]
   nl = 1 ## individual counter
   for(s in 1:nspec){
     if(ntrees[s]==0) next
     slta <- spp.params$SLTA[s]
     sltb <- spp.params$SLTB[s]
     fwt <- spp.params$FWT[s]
     frt <- spp.params$FRT[s]
#     dbh_spp[s] <- optimize(merit, c(0,200))$minimum
     nu <- nl + ntrees[s] - 1
     for(j in nl:nu){
       b_obs <- biomass_function(dbh[j])*bcorr
       dbh[j] <- optimize(merit, c(0,200),b_obs=b_obs)$minimum                                 
     }
     nl <- nu + 1 
   }

    file.rename(file.path(settings$rundir,run.id[[i]],"linkages.restart.Rdata"),file.path(settings$rundir,run.id[[i]],paste0(time,"linkages.restart.Rdata"))) #save original output
    restart.file <- file.path(settings$rundir,run.id[[i]],"linkages.restart.Rdata")
    sprintf("%s",restart.file)
    
    save(dbh, tyl, ntrees, nogro, ksprt, iage, C.mat, tyl, ncohrt,
         file = restart.file)
   
    #make a new settings with the right years
    #min start date and end date - fail in informative way

  settings$run$start.date <- paste0(time+1,"/01/01")
  settings$run$end.date <- paste0(time+1,"/12/31")
#    settings$run$start.date <- paste0(time,strftime(settings$run$end.date,"/%m/%d"))
#    settings$run$end.date <- paste0(time,strftime(settings$run$end.date,"/%m/%d"))
   
    do.call(my.write.config,args=list(settings=settings,run.id = run.id[[i]],restart = TRUE))   
  }
}
