#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##' convert composite ring & census data into AGB
##' 
##' @name  plot2AGB
##' @title plot2AGB
##' 
##' @param combined   data frame merging plot inventory and tree ring data
##' @param out        MCMC samples for diameter (sample x tree)
##' @param outfolder  output folder for graphs & data 
##' @param unit.conv  area conversion from sum(kg/tree) to kg/area
##' 
##' @author Mike Dietze \email{dietze@@bu.edu}
##' @export
plot2AGB <- function(combined,out,outfolder,allom.stats,unit.conv=0.02){
  
  require(mvtnorm)
  
  ## Jenkins: hemlock (kg)
  #b0 <- -2.5384
  #b1 <- 2.4814
  
  ## Allometric statistics
  b0 = allom.stats[[1]][[6]]$statistics["Bg0","Mean"]
  b1 = allom.stats[[1]][[6]]$statistics["Bg1","Mean"]
  B    = allom.stats[[1]][[6]]$statistics[c("Bg0","Bg1"),"Mean"]
  Bcov = allom.stats[[1]][[6]]$cov[c("Bg0","Bg1"),c("Bg0","Bg1")]
  Bsd  = sqrt(allom.stats[[1]][[6]]$statistics["Sg","Mean"])
  
  ## prep data
  out[out < 0.1] = 0.1
  nrep  = nrow(out)
  ntree = nrow(combined)
  nt = ncol(out)/ntree
  mplot = 1  ## later need to generalize to splitting up plots
  ijindex = matrix(1,ntree,1)
  yrvec = as.numeric(colnames(combined)); yrvec = yrvec[!is.na(yrvec)]
  
  ## set up storage
  NPP <- array(NA,c(mplot,nrep,nt-1))
  AGB <- array(NA,c(mplot,nrep,nt))
  
  ## sample over tree chronologies
  pb <- txtProgressBar(min = 0, max = nrep, style = 3)
  for(g in 1:nrep){
    
    ## Draw allometries
    b = rmvnorm(1,B,Bcov)
    
    ## convert tree diameter to biomass    
    biomass <- matrix(exp(b[1] + b[2]*log(out[g,])),ntree,nt)
  
    for(j in 1:mplot){
      
      ## aggregate to stand AGB      
      AGB[j,g,] <- apply(biomass,2,sum,na.rm=TRUE)*unit.conv
#      AGB[j,g,] <- apply(biomass[ijindex[,1]==j,],2,sum,na.rm=TRUE)*unit.conv
  
      ## diff to get NPP
      NPP[j,g,] <- diff(AGB[j,g,])
      
    }
    setTxtProgressBar(pb, g)
  }
  
  mAGB <- sAGB <- matrix(NA,mplot,nt)
  mNPP <- sNPP <- matrix(NA,mplot,nt-1)
  for(i in 1:mplot){
    mNPP[i,] <- apply(NPP[i,,],2,mean,na.rm=TRUE)
    sNPP[i,] <- apply(NPP[i,,],2,sd,na.rm=TRUE)
    mAGB[i,] <- apply(AGB[i,,],2,mean,na.rm=TRUE)
    sAGB[i,] <- apply(AGB[i,,],2,sd,na.rm=TRUE)
  }

  pdf(file.path(outfolder,"plot2AGB.pdf"))
  par(mfrow=c(2,1))
  for(i in 1:mplot){
    up = mNPP[i,]+sNPP[i,]*1.96
    low = mNPP[i,]-sNPP[i,]*1.96
    plot(yrvec[-1],mNPP[i,],ylim=range(c(up,low)),ylab="Mg/ha/yr",xlab="year" ) 
    lines(yrvec[-1],up)
    lines(yrvec[-1],low)
    upA = mAGB[i,]+sAGB[i,]*1.96
    lowA = mAGB[i,]-sAGB[i,]*1.96
    plot(yrvec,mAGB[i,],ylim=range(c(upA,lowA)),ylab="Mg/ha",xlab="year")
    lines(yrvec,upA)
    lines(yrvec,lowA)
  }
  dev.off()
  save(AGB,NPP,mNPP,sNPP,mAGB,sAGB,yrvec,file=file.path(outfolder,"plot2AGB.Rdata"))
  return(list(AGB=AGB,NPP=NPP))
  
}
