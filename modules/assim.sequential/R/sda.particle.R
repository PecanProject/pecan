###  State Variable Data Assimilation:
###     Particle Filter
###
###  Michael Dietze <dietze@bu.edu>
###

### Prerequisite:assumes that you have run an ensemble run

sda.particle <- function(model){
  sda.demo <- FALSE  ## debugging flag
  unit.conv <-  0.001*2#  kgC/ha/yr to Mg/ha/yr
  

  ## internal functions
  #  library(Hmisc)
  wtd.quantile <- function(x,wt,q){  ## weighted quantile
    ord <- order(x)
    wstar <- cumsum(wt[ord])/sum(wt)
    qi <- findInterval(q,wstar)
    return(x[ord[qi]])
  }
  

  ## extract time from one ensemble member
  Year <- read.output("ENS00001",settings$outdir,variables="Year",model=model)$Year
  time <- as.numeric(names(table(Year)))
  
### Load Ensemble
  ensp.all = read.ensemble.ts(model)
  ensp = ensp.all[[1]]
  ensp = t(apply(ensp,1,tapply,Year,mean))*unit.conv
  np = nrow(ensp)      ## number of particles
  nt = ncol(ensp)      ## number of time steps
  w = matrix(1,np,nt)  ## matrix of weights
 
### Load Data
  if(sda.demo){
    ## use one of the ensemble members as the true data
    sd <- apply(ensp,2,sd)*0.3  ## pseudo data uncertainty
    ref <- sample(1:np,1)       ## choose an ensemble member
    y <- rnorm(nt,ensp[ref,],sd) ## add noise
  } else {
    load(file.path(settings$outdir,"plot2AGB.Rdata"))
    mch = which(yrvec %in% time)
    y = mNPP[1,mch]   ## data mean
    sd = sNPP[1,mch]  ## data uncertainty 
  }
  
  ## diagnostic figures, not printed as part of normal operation
  if(FALSE){
    ## plot ensemble
    plot(0,0,type='n',ylim=range(ensp),xlim=range(time),ylab="Mg/ha/yr",xlab="year")
    for(i in 1:np){
      lines(time,ensp[i,])
    }
    ## plot data
    for(i in 1:nrow(mNPP)){
      up = mNPP[i,]+sNPP[i,]*1.96
      low = mNPP[i,]-sNPP[i,]*1.96
      lines(yrvec[-1],mNPP[i,],ylim=range(c(up,low)),col=2 ) 
      lines(yrvec[-1],up)
      lines(yrvec[-1],low)
    }
  }
  
###analysis: generate log-weights
  for(t in 1:nt){    
    if(!is.na(y[t])){
      for(j in 1:np){
        w[j,t] = sum(dnorm(y[t],ensp[j,t],sd[t],log=TRUE)) #loglik
      }
    }
  }

### calculate weighted mean and CI of the ensemble
wc = t(exp(apply(w,1,cumsum)))
wbar = apply(wc,2,mean)
Xap = 0
Pap = 0
XapCI = matrix(NA,2,nt)
for(i in 1:nt){
  Xap[i] = weighted.mean(ensp[,i],wc[,i])
  XapCI[,i] = wtd.quantile(ensp[,i],wc[,i]/wbar[i],c(0.025,0.975))
#  Pap[i] = wtd.var(ensp[,i],wc[,i]/wbar[i])
}
Pap[Pap < 0] <- 0
Xbar = apply(ensp,2,mean)
Xci = apply(ensp,2,quantile,c(0.025,0.975))

### Diagnostic graphs  
pdf(file.path(outfolder,"ParticleFilter.pdf"))
  
  ## plot ensemble, filter, and data mean's and CI's
  par(mfrow=c(1,1))
  plot(time,y,ylim=range(c(y+1.96*sd,y-1.96*sd)),type='b',xlab="time",ylab="Mg/ha/yr")
  lines(time,y+1.96*sd,col=2)
  lines(time,y-1.96*sd,col=2)
  

  plot(time,y,ylim=range(Xci),type='n',xlab="time",ylab="Mg/ha/yr")
  ## ensemble 
  lines(time,Xbar[1:nt],col=6)
  lines(time,Xci[1,1:nt],col=6,lty=2,lwd=2)
  lines(time,Xci[2,1:nt],col=6,lty=2,lwd=2)
  legend("topright",c("ens","Data","PF"),col=c(6,1,3),lty=1,pch=c(NA,1,2),cex=1.5)  
  ## DATA  
  lines(time,y,col=1,lwd=3,pch=1,type='b')
  lines(time,y+1.96*sd,col=1,lwd=2,lty=2)
  lines(time,y-1.96*sd,col=1,lwd=2,lty=2)
  
if(sda.demo) lines(time,ensp[ref,],col=2,lwd=2)
#for(i in 1:min(500,np)){
# lines(t2,ensp[,i],lty=3,col="grey")
#}
#lines(t,x,type='b')
#points(tvec,yvec,col=2,pch=19)

#legend("topleft",c("True","Data","ens","ensmean"),col=c(1:2,"grey",6),lty=c(1,0,3,1),pch=c(1,19,1,1),cex=1.5)

  ## assimilation
lines(time,Xap,col=3,type='b',lwd=3,pch=2)
lines(time,XapCI[1,],col=3,lty=2,lwd=2)
lines(time,XapCI[2,],col=3,lty=2,lwd=2)
#lines(t,Xap+1.96*sqrt(Pap),col=3,lty=2)
#lines(t,Xap-1.96*sqrt(Pap),col=3,lty=2)
legend("bottomleft",c("True","Data","PF","ens","ensmean"),col=c(1:3,"grey",6),lty=c(1,0,1,3,1),pch=c(1,19,1,1,0),cex=1.5)
  
### Plots demonstrating how the constraint of your target variable 
### impacts the other model pools and fluxes
  
  ## Calculate long-term means for all ensemble extracted variables
  unit <- rep(unit.conv,5);#unit[3] = 1
  ensp.conv <- list()
  for(i in 1:length(ensp.all)){
    ensp.conv[[i]] <- t(apply(ensp.all[[i]],1,tapply,Year,mean))*unit[i]
  }  
  ## plot scatter plots of outputs
  par(mfrow=c(1,2))
  for(i in 2:3){
    plot(ensp.conv[[1]][,nt],ensp.conv[[i]][,nt],xlab=names(ensp.all)[1],ylab=names(ensp.all)[i])
  }
  abline(0,1,col=2)
  
  ##unweighted distributions
  for(i in c(2,3)){
    hist(ensp.conv[[i]][,nt],main=names(ensp.all)[i],probability=TRUE)
  }  
  
  ## Weighted distributions
  library(plotrix)
  for(i in c(2,3)){
    weighted.hist(ensp.conv[[i]][,nt],wc[,nt]/sum(wc[,nt]),main=names(ensp.all)[i])
  }
  
#  for(i in c(1,2,4,5)){
  for(i in c(2,3)){
    if(i == 5){
      weighted.hist(ensp.conv[[i]][,nt],wc[,nt]/sum(wc[,nt]),main=names(ensp.all)[i],col=2)
    }else{
      weighted.hist(ensp.conv[[i]][,nt],wc[,nt]/sum(wc[,nt]),main=names(ensp.all)[i],xlim=range(ensp.conv[[i]][,nt])*c(0.9,1.1),col=2)
    }
    hist(ensp.conv[[i]][,nt],main=names(ensp.all)[i],probability=TRUE,add=TRUE)
  }

  for(i in c(2,3)){
    h=hist(ensp.conv[[i]][,nt],plot=FALSE)
    w = weighted.hist(ensp.conv[[i]][,nt],wc[,nt]/sum(wc[,nt]),plot=FALSE,breaks=h$breaks)
    dx = diff(h$breaks)[1]                    
    plot(w$mids-dx/2,w$density/dx,main=names(ensp.all)[i],xlim=range(ensp.conv[[i]][,nt])*c(0.9,1.1),col=2,type='s',lwd=2)
    lines(h$mids-dx/2,h$density,col=1,type='s',lwd=2)
#    hist(ensp.conv[[i]][,nt],main=names(ensp.all)[i],probability=TRUE,add=TRUE)
  }
  
  dev.off()
  
  ## save all outputs
  save.image(paste(settings$outdir,"sda.particle.Rdata"))
  
}
