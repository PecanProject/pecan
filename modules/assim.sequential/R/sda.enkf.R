###  State Variable Data Assimilation:
###     Ensemble Kalman Filter
###
###  Michael Dietze <dietze@bu.edu>
###

sda.enkf <- function(model,nens = 10){
  sda.demo <- TRUE  ## debugging flag
  unit.conv <-  0.001*2#  kgC/ha/yr to Mg/ha/yr
    
  ## extract time from one ensemble member
  Year <- read.output("ENS00001",settings$outdir,variables="Year",model=model)$Year
  time <- as.numeric(names(table(Year)))
  nt   <- length(time)
  
  ### Load Data
  if(sda.demo){
    ## use one of the ensemble members as the true data
    NPP <- read.output("ENS00001",settings$outdir,variables="NPP",model=model)$NPP
    ytrue = tapply(NPP,Year,mean)*unit.conv
    sd <- 0.3  ## pseudo data uncertainty
    y <- rnorm(nt,ytrue,sd) ## add noise
  } else {
    load(file.path(settings$outdir,"plot2AGB.Rdata"))
    mch = which(yrvec %in% time)
    y = mNPP[1,mch]   ## data mean
    sd = sNPP[1,mch]  ## data uncertainty 
  }
  
  ## split clim file
  full.met <- settings$run$site$met
  met <- split.met.SIPNET(full.met)
  
  ## generate inital ensemble members
  base.outdir = settings$outdir
  settings$run$site$met.data.header <- met[1]
  settings$ensemble$size = nens
  settings$outdir = paste(base.outdir,".",time[1],"/",sep="")
  dir.create(settings$outdir)
  run.write.configs(model) 
  start.model.runs(model)
  ens <- list()
  NPPm <- rep(NA,nens)
  IC = matrix(0,nrow=nens,ncol=8)
  colnames(IC) <- c("plantWood","lai","litter","soil","litterWFrac","soilWFrac","snow","microbe")
  for(i in 1:nens){
    ens[[i]] <- read.output("ENS00001",settings$outdir,
                variables=c("NPP","AbvGrndWood","TotSoilCarb","LeafC","SoilMoist","SWE"),
                model=model)
    NPPm[i] <- mean(ens[[i]]$NPP)*unit.conv
    last = length(ens[[i]]$NPP)
    IC$plantWood[i] = ens[[i]]$AbvGrndWood[last] #units? belowground fraction?
    ##### implemented to HERE
  }
  load(paste(settings$outdir,"samples.Rdata",sep=""))
  params <- ensemble.samples  ## have to be able to restart ensemble members with same parameters
  
  ## loop over time steps
  for(t in 1:nt){
    
    
  }
  
  ## write configs
  
  ## run time step
  
  ## load output
  
  ## calculate ensemble mean and covariance matrix
  
  ## calculate likelihood
  
  ## calculate new initial conditions
  
  ## end loop over time
  
#### Post-processing

  ### Diagnostic graphs  
  pdf(file.path(outfolder,"EnKF.pdf"))
  
  ## plot ensemble, filter, and data mean's and CI's
  par(mfrow=c(1,1))
  plot(time,y,ylim=range(c(y+1.96*sd,y-1.96*sd)),type='b',xlab="time",ylab="Mg/ha/yr")
  lines(time,y+1.96*sd,col=2)
  lines(time,y-1.96*sd,col=2)
  
  plot(time,y,ylim=range(Xci),type='b',xlab="time",ylab="Mg/ha/yr")
  lines(time,y+1.96*sd,col=2)
  lines(time,y-1.96*sd,col=2)
  if(sda.demo) lines(time,ensp[ref,],col=2,lwd=2)
  lines(time,Xbar[1:nt],col=6)
  lines(time,Xci[1,1:nt],col=6,lty=2)
  lines(time,Xci[2,1:nt],col=6,lty=2)

  lines(time,Xap,col=3,type='b',lwd=2)
  lines(time,XapCI[1,],col=3,lty=2,lwd=2)
  lines(time,XapCI[2,],col=3,lty=2,lwd=2)
  legend("topleft",c("True","Data","PF","ens","ensmean"),col=c(1:3,"grey",6),lty=c(1,0,1,3,1),pch=c(1,19,1,1,0),cex=1.5)
  
### Plots demonstrating how the constraint of your target variable 
### impacts the other model pools and fluxes
  
  ## Calculate long-term means for all ensemble extracted variables
  unit <- rep(unit.conv,5);unit[3] = 1
  ensp.conv <- list()
  for(i in 1:length(ensp.all)){
    ensp.conv[[i]] <- t(apply(ensp.all[[i]],1,tapply,Year,mean))*unit[i]
  }  
  ## plot scatter plots of outputs
  par(mfrow=c(2,2))
  for(i in 2:5){
    plot(ensp.conv[[1]][,nt],ensp.conv[[i]][,nt],xlab=names(ensp.all)[1],ylab=names(ensp.all)[i])
  }
  
  ##unweighted distributions
  for(i in c(1,2,4,5)){
    hist(ensp.conv[[i]][,nt],main=names(ensp.all)[i],probability=TRUE)
  }  
  
  ## Weighted distributions
  library(plotrix)
  for(i in c(1,2,4,5)){
    weighted.hist(ensp.conv[[i]][,nt],wc[,nt]/sum(wc[,nt]),main=names(ensp.all)[i])
  }
  
  for(i in c(1,2,4,5)){
    if(i == 5){
      weighted.hist(ensp.conv[[i]][,nt],wc[,nt]/sum(wc[,nt]),main=names(ensp.all)[i],col=2)
    }else{
      weighted.hist(ensp.conv[[i]][,nt],wc[,nt]/sum(wc[,nt]),main=names(ensp.all)[i],xlim=range(ensp.conv[[i]][,nt])*c(0.9,1.1),col=2)
    }
    hist(ensp.conv[[i]][,nt],main=names(ensp.all)[i],probability=TRUE,add=TRUE)
  }
  
  dev.off()
  
  ## save all outputs
  save.image(paste(settings$outdir,"sda.particle.Rdata"))
  
}
