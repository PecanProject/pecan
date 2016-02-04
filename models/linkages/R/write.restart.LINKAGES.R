write.restart.LINKAGES <- function(nens,outdir,run.id,time,settings,prior,analysis){
  
  biomass_function<-function(dbh){ #check units
    .1193 * dbh^2.393 + ((slta+sltb*dbh)/2)^2 * 3.14 * fwt * frt * .001
  }
  merit<-function(dbh,b_obs){
    (b_obs - biomass_function(dbh))^2
  }
  
  distance.matrix <- matrix(c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0),4,4)
  
  lnorm.est <- "
    model{ 
         for(i in 1:I){
            y[i] ~ dlnorm(mu,tau)
         }            
            mu ~ dgamma(.001,.001)
            exp.mu <- exp(mu)
            tau <- 1 / (sd ^ 2)
            sd ~ dunif(0,5)

          }" 
  
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
    ksprt <- matrix(0,1,nspec)    ## kill sprout indicator ## LOOK INTO THIS
    iage <- as.vector(iage.save[,ncol(iage.save),1]) # individual age
    
    dbh = as.vector(dbh.save[,ncol(dbh.save),1])
    
#     ### Match Successional Stage ###   
#     update = list(y=iage[iage>0],I=length(iage[iage>0]))
#     mod <- jags.model(file=textConnection(lnorm.est),
#                       data=update,
#                       n.adapt=1000,n.chains=3)
#     jdat <- coda.samples(mod,variable.names=c("exp.mu","mu","tau","sd"),n.iter=10000) 
#     
#     dat1 <- as.matrix(jdat)
#     
#     min.f1f2 <- function(x, mu1, mu2, sd1, sd2) {
#       f1 <- dlnorm(x, mean=log(mu1), sd=sd1)
#       f2 <- dlnorm(x, mean=log(mu2), sd=sd2)
#       pmin(f1, f2)
#     }
#     
#     mu1 <- mean(dat1[,1])
#     sd1 <- mean(dat1[,3])
#     
#     mu2 <- 20
#     sd2 <- 1
#     early <- integrate(min.f1f2, -Inf, Inf, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)
#     
#     mu2 <- 50
#     sd2 <- 1
#     mid <- integrate(min.f1f2, -Inf, Inf, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)
#     
#     mu2 <- 100
#     sd2 <- 1
#     late <- integrate(min.f1f2, -Inf, Inf, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)

    n.index = c(rep(1,ntrees[1]),rep(2,ntrees[2]),rep(3,ntrees[3]),rep(4,ntrees[4]))
    new.ntrees = numeric(4)
    for(s in 1:4){
      b_avg <- biomass_function(mean(dbh[which(n.index==s)])) #biomass avg. per individual
      fix <- analysis[i,s]/b_avg #number of ind needed to agree with analysis
      new.ntrees[s] <- round(fix*ntrees[s]) #new number of ind. of each species
    }

    #bcorr = analysis[i,] / agb.pft[,ncol(agb.pft),1]
    new.n.index = c(rep(1,new.ntrees[1]),rep(2,new.ntrees[2]),rep(3,new.ntrees[3]),
                      rep(4,new.ntrees[4]))

    dbh.temp <- numeric(length(dbh))
    iage.temp <- numeric(length(dbh))
    nogro.temp <- numeric(length(dbh))
      
    for(s in 1:nspec){
        select <- sample(size=new.ntrees[s],x=which(n.index==s),replace=T)
        
        dbh.temp[which(new.n.index==s)] <- dbh[select] + rnorm(new.ntrees[s],.001,.01)
        iage.temp[which(new.n.index==s)] <- iage[select]
        nogro.temp[which(new.n.index==s)] <- nogro[select] 
    }
    
   #translate agb to dbh
# 
#   n1 = 1
#   n2 = n1 + ntrees[1]
#   n3 = n2 + ntrees[2]
#   n4 = n3 + ntrees[3]
#   nl = 1
#   for(n in 1:nspec){  
#     if (agb.pft[n,ncol(agb.pft),1]==0 & analysis[i,n]>0){
#       abg.pft.temp <- sum(distance.matrix[,n]%*%t(agb.pft[n,ncol(agb.pft),1]))
#       ntrees.temp <- sum(distance.matrix[,n]%*%t(t(as.matrix(ntrees)))) 
#       dbh.temp <- dbh[sum(ntrees[1:n])-1]
#       for(j in 1:ntrees.temp){
#         b_obs <- biomass_function(dbh[j])*bcorr[n]
#         dbh.temp[j] <- optimize(merit, c(0,200),b_obs=b_obs)$minimum 
#       }
#     }
#     nu <- nl + ntrees[n] - 1
#     nl <- nu + 1 
#   }
#    dbh.temp1 = numeric(length(dbh))
#    nl = 1 ## individual counter
#    for(s in 1:nspec){
#      if(new.ntrees[s]==0) next
#      slta <- spp.params$SLTA[s]
#      sltb <- spp.params$SLTB[s]
#      fwt <- spp.params$FWT[s]
#      frt <- spp.params$FRT[s]
# #     dbh_spp[s] <- optimize(merit, c(0,200))$minimum
#      nu <- nl + new.ntrees[s] - 1
#      for(j in nl:nu){
#        b_obs <- analysis[i,s] / new.ntrees[s] #biomass_function(dbh.temp[j])*bcorr[s]
#        dbh.temp1[j] <- optimize(merit, c(0,200),b_obs=b_obs)$minimum                                 
#      }
#      nl <- nu + 1 
#    }

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
