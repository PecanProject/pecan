write.restart.LINKAGES <- function(nens,outdir,run.id,time,settings,prior,analysis){
  
  biomass_function<-function(dbh){ #kg/tree
      .1193 * dbh^2.393 + ((slta+sltb*dbh)/2)^2 * 3.14 * fwt * frt * .001
  }
  merit<-function(dbh){
    (b_obs - biomass_function(dbh))^2
  }
  
  distance.matrix <- rbind(c(0,3,1,2),
                           c(3,0,2,1),
                           c(1,2,0,3),
                           c(2,1,3,0))
  

  ##HACK
  spp.params.default <- read.csv(system.file("spp_matrix.csv", package = "linkages")) #default spp.params
  nspec <- length(settings$pfts)
  spp.params.save <- numeric(nspec)
  for(i in 1:nspec){
    spp.params.save[i] <- which(spp.params.default[,1]%in%settings$pfts[i]$pft$name)
  }
  
  spp.params <- spp.params.default[spp.params.save,]
  ##HACK
  
  analysis <- analysis[,c(1,2,4,3)] #doing this to match the linkages order #need to figure out how to generalize
  
  for(i in 1:nens){
    # skip ensemble member if no file availible  
    outfile = file.path(outdir,run.id[[i]],"1973linkages.out.Rdata")
    if(!file.exists(outfile)){
      print(paste0("missing outfile ens #",run.id[[i]]))
      next
    } 
        
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

    n.index = c(rep(1,ntrees[1]),rep(2,ntrees[2]),rep(3,ntrees[3]),rep(4,ntrees[4]))
    new.ntrees = numeric(4)
    
    if(PLOT == TRUE){
      n.name <- c(rep("Hemlock",ntrees[1]),rep("Maple",ntrees[2]),rep("Cedar",ntrees[3]),rep("Yellow Birch",ntrees[4]))
      data1 <- data.frame(DBH = dbh[dbh>0],Trees = as.character(n.name), AGE = iage[iage>0], NOGRO = nogro[1:sum(ntrees)])
      A<-qplot(DBH,data = data1,geom="histogram",group=Trees,fill=Trees,binwidth=1) + theme_bw()
      B<-qplot(AGE,data = data1,geom="histogram",group=Trees,fill=Trees,binwidth=1) + theme_bw()
      C<-qplot(NOGRO,data = data1,geom="histogram",group=Trees,fill=Trees,binwidth=1) + theme_bw()
      #grid.arrange(arrangeGrob(A,B,C,ncol=3,nrow=1), nrow=1)
    }

    
    ##### This takes the average individual biomass of each species from the model and computes
    ##### how many individuals you should keep to match the biomass estimated from the data.
    ##### Still have to correct for the total species biomass in the next step.

    ind.biomass <- numeric(sum(ntrees))
    
    #calculate biomass of each individual
    for(j in 1:sum(ntrees)){
      slta <- spp.params$SLTA[n.index[j]]
      sltb <- spp.params$SLTB[n.index[j]]
      fwt <- spp.params$FWT[n.index[j]]
      frt <- spp.params$FRT[n.index[j]]
      ind.biomass[j] <- biomass_function(dbh[j]) 
    }
    
    data2 = data.frame(ind.biomass = ind.biomass,n.index = n.index)
    mean.biomass.spp <- aggregate(ind.biomass ~ n.index,mean,data=data2) #calculate mean individual biomass for each species
    
    #calculate number of individuals needed to match analysis
    for(s in 1:4){      
      if(ntrees[s]>0){
        fix <- analysis[i,s]/mean.biomass.spp[mean.biomass.spp[,1]==s,2] #number of individuals needed to agree with analysis      
      }else{
        for(r in 1:3){
          s.select <- which(distance.matrix[s,] == r) #select a new spp. to clone from
          if(ntrees[s.select]>0) break
        }
        fix <- analysis[i,s] / mean.biomass.spp[mean.biomass.spp[,1]==s.select,2]
      }
      new.ntrees[s] <- ceiling(fix) #new number of ind. of each species
    }
  
    new.n.index = c(rep(1,new.ntrees[1]),rep(2,new.ntrees[2]),rep(3,new.ntrees[3]),
                      rep(4,new.ntrees[4]))

    dbh.temp <- numeric(length(dbh))
    iage.temp <- numeric(length(dbh))
    nogro.temp <- numeric(length(dbh))
      
    #sample from individuals to construct new states
    for(s in 1:nspec){
      if(new.ntrees[s] < ntrees[s]){ #new are less than the old of the same spp.
        print("new are less than the old of the same spp.")
        select <- sample(size = new.ntrees[s], x = which(n.index == s), replace = FALSE)
      }else{
        if(new.ntrees[s] > ntrees[s] & ntrees[s]!=0){ #new are greater than the old of the same spp. and there are old trees to clone
          print("new are greater than the old of the same spp. and there are old trees of same spp. to clone")
          select <- c(which(n.index == s), sample(size = new.ntrees[s] - ntrees[s], 
                                                  x = which(n.index == s), replace = FALSE))
        }else{
          print(paste0("clone needed for spp. ",s))
          for(r in 1:3){
            s.select <- which(distance.matrix[s,] == r) #select a new spp. to clone from
            if(ntrees[s.select]>0) break
          }
            print(s.select)
            select <- sample(size = new.ntrees[s.select], x = which(n.index == s.select), replace = T)
          }
        }
      print(select)
      dbh.temp[which(new.n.index==s)] <- dbh[select]
      iage.temp[which(new.n.index==s)] <- iage[select]
      nogro.temp[which(new.n.index==s)] <- nogro[select]
    }

    #fix dbh of sampled individuals to match analysis
    nl = 1 ## individual counter
    b_calc <- numeric(4) #biomass of sampled trees
    bcorr <- numeric(4) #biomass correction factor to analysis
    for(s in 1:nspec){
      if(new.ntrees[s]==0) next
      slta <- spp.params$SLTA[s]
      sltb <- spp.params$SLTB[s]
      fwt <- spp.params$FWT[s]
      frt <- spp.params$FRT[s]
      nu <- nl + new.ntrees[s] - 1
      for(j in nl:nu){
        b_calc[s] <- biomass_function(dbh.temp[j]) + b_calc[s]
      }
      bcorr[s] <- analysis[i,s] / b_calc[s]
      for(j in nl:nu){
        b_obs <- biomass_function(dbh.temp[j])*bcorr[s]
        dbh.temp[j] <- optimize(merit, c(0,200))$minimum 
      }
      nl <- nu + 1 
    }
  
    dbh <- dbh.temp
    iage <- iage.temp
    nogro <- nogro.temp
  
    ntrees <- new.ntrees
    
    if(PLOT == TRUE){
      #have to fix the colors
      n.name <- c(rep("Hemlock",ntrees[1]),rep("Maple",ntrees[2]),rep("Cedar",ntrees[3]),rep("Yellow Birch",ntrees[4]))
      data3 <- data.frame(DBH = dbh[dbh>0],Trees = as.character(n.name), AGE = iage[iage>0], NOGRO = nogro[1:sum(ntrees)])
      D<-qplot(DBH,data = data3,geom="histogram",group=Trees,fill=Trees,binwidth=1) + theme_bw()
      E<-qplot(AGE,data = data3,geom="histogram",group=Trees,fill=Trees,binwidth=1) + theme_bw()
      F<-qplot(NOGRO,data = data3,geom="histogram",group=Trees,fill=Trees,binwidth=1) + theme_bw()
      grid.arrange(arrangeGrob(A,B,C,D,E,F,ncol=3,nrow=2), nrow=1)
    }

   #translate agb to dbh

#dbh_spp[s] <- optimize(merit, c(0,200))$minimum
# bcorr = analysis[i,] / agb.pft[,ncol(agb.pft),1]
#*(bcorr[s]/ntrees[s])
#dbh.temp1[j] <- optimize(merit, c(0,200))$minimum

#   for(n in 1:nspec){  
#     slta <- spp.params$SLTA[n]
#     sltb <- spp.params$SLTB[n]
#     fwt <- spp.params$FWT[n]
#     frt <- spp.params$FRT[n]
#     if (agb.pft[n,ncol(agb.pft),1]==0 & analysis[i,n]>0){
#       abg.pft.temp <- sum(distance.matrix[,n]%*%t(agb.pft[n,ncol(agb.pft),1]))
#       ntrees.temp <- sum(distance.matrix[,n]%*%t(t(as.matrix(ntrees)))) 
#       dbh.temp <- dbh[sum(ntrees[1:n])-1]
#       for(j in 1:ntrees.temp){
#         b_obs <- biomass_function(dbh[j],slta=slta,sltb=sltb,fwt=fwt,frt=frt)*bcorr[n]
#         dbh.temp[j] <- optimize(merit, c(0,200),b_obs=b_obs)$minimum 
#       }
#     }
#     nu <- nl + ntrees[n] - 1
#     nl <- nu + 1 
#   }

    file.rename(file.path(settings$rundir,run.id[[i]],"linkages.restart.Rdata"),file.path(settings$rundir,run.id[[i]],paste0(time,"linkages.restart.Rdata"))) #save original output
    restart.file <- file.path(settings$rundir,run.id[[i]],"linkages.restart.Rdata")
    sprintf("%s",restart.file)
    
    save(dbh, tyl, ntrees, nogro, ksprt, iage, C.mat, ncohrt,
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
