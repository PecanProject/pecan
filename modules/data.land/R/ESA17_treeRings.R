## Dendro figures 8/8/17

## Note: must run data loading from RDriverFusion

library(rjags)
library(PEcAn.data.land)
jags.comb <- NULL
for(i in 24:30){
  load(paste0("~/Dropbox/Desktop/Projects/MargaretEvans/072517/IGF.1.",i,".RData"))
  if(is.null(jags.comb)){
    jags.comb <- jags.out
  } else {
    for(j in seq_along(jags.out)){
      x.cols <- grep("^x",colnames(jags.out[[j]]))
      if(length(x.cols)>0) jags.out[[j]] <- jags.out[[j]][,-x.cols]
      jags.comb[[j]]  <- rbind(jags.comb[[j]],jags.out[[j]])
    }
  }
}
for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.1.ESA.RData")

## standardized regression coef
out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})

save(out,jags.stuff,file="ESA17.RData")

## calculate an average tree
hist(z0)
x <- mean(z0,na.rm = TRUE)
Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
SDI <- mean(cov.data$SDI,na.rm = TRUE)
SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)
wintP <- mean(time_data$wintP.JJ,na.rm = TRUE)
wintPhl <- quantile(time_data$wintP.JJ,c(1/6,5/6),na.rm = TRUE)
SI <- mean(cov.data$SICOND, na.rm = TRUE)
SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)

colnames(betas)

stdBeta <- rep(NA,11)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_wintP.JJ"])/SDI/wintP
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[5] <- mean(betas[,"betaSICOND_wintP.JJ"])/SI/wintP
stdBeta[6] <- mean(betas[,"betaX"])/x
stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_wintP.JJ"])/x/wintP
stdBeta[11] <- mean(betas[,"betawintP.JJ"])/wintP
names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)

## Size
ns = 500 ## number of samples
i = sample.int(nrow(betas),ns)
xrng <- range(z0,na.rm = TRUE)
xseq <- seq(xrng[1],xrng[2],by=1)
xseq <- 1:58
incX <- matrix(NA,ns,length(xseq))
for(k in seq_along(i)){
  j <- i[k]
  incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintP + betas[j,"betaX"]*xseq + betas[j,"betaX2"]*xseq*xseq +
    betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.JJ"]*xseq*wintP +
    betas[j,"betawintP.JJ"]*wintP
}
CIX <- apply(incX,2,quantile,c(0.025,0.5,0.975))
plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
PEcAn.visualization::ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
lines(xseq,CIX[2,],lwd=2)
abline(h=0)

##SDI
hist(cov.data$SDI)
SDIseq <- seq(0,400,by=10)
incSDI <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.JJ"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.JJ"]*x*wintP +
    betas[j,"betawintP.JJ"]*wintP
}
CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))
plot(SDIseq,CIsdi[2,],ylim=c(0,max(CIsdi)),type='n',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
PEcAn.visualization::ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
lines(SDIseq,CIsdi[2,],lwd=2)
abline(h=0)

## SDI * size
incSDIXhi <- matrix(NA,ns,length(SDIseq))
incSDIXlo <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.JJ"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.JJ"]*Xhl[1]*wintP +
    betas[j,"betawintP.JJ"]*wintP
  incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.JJ"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.JJ"]*Xhl[2]*wintP +
    betas[j,"betawintP.JJ"]*wintP
}
CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))
plot(SDIseq,CIsdi[2,],ylim=c(0,max(CIsdi)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
#PEcAn.visualization::ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
lines(SDIseq,CIsdiXlo[2,],lwd=3,col="blue")
lines(SDIseq,CIsdiXhi[2,],lwd=3,col="red")
legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)

abline(h=0)

##SI
hist(cov.data$SI)
SIseq <- 20:60
incSI <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.JJ"]*SIseq*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.JJ"]*x*wintP +
    betas[j,"betawintP.JJ"]*wintP
}
CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))
plot(SIseq,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
PEcAn.visualization::ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
lines(SIseq,CIsi[2,],lwd=2)
abline(h=0)

## SI x DBH
incSIXlo <- matrix(NA,ns,length(SDIseq))
incSIXmed <- matrix(NA,ns,length(SDIseq))
incSIXhi <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.JJ"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.JJ"]*Xhl[1]*wintP +
    betas[j,"betawintP.JJ"]*wintP
  incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.JJ"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.JJ"]*Xhl[2]*wintP +
    betas[j,"betawintP.JJ"]*wintP
  incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.JJ"]*SIseq*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.JJ"]*x*wintP +
    betas[j,"betawintP.JJ"]*wintP
}
CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))
plot(SIseq,CIsiXm[2,],ylim=c(0,0.35),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
#PEcAn.visualization::ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
lines(SIseq,CIsiXl[2,],lwd=3,col="blue")
lines(SIseq,CIsiXh[2,],lwd=3,col="red")
legend("topleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1.5)

## wintP
hist(time_data$wintP.JJ)
wintPseq <- 0:800
incP <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.JJ"]*x*wintPseq +
    betas[j,"betawintP.JJ"]*wintPseq
}
CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))
plot(wintPseq,CIwintP[2,],ylim=c(0,max(CIwintP)),type='n',xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
lines(wintPseq,CIwintP[2,],lwd=2)
abline(h=0)

## PRECIP x SDI
incPSDIlo <- matrix(NA,ns,length(wintPseq))
incPSDIhi <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.JJ"]*SDIhl[1]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.JJ"]*x*wintPseq +
    betas[j,"betawintP.JJ"]*wintPseq
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.JJ"]*SDIhl[2]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.JJ"]*x*wintPseq +
    betas[j,"betawintP.JJ"]*wintPseq
}

CIwintPSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))
plot(wintPseq,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
#PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
lines(wintPseq,CIwintPSDIlo[2,],lwd=2,col="blue")
lines(wintPseq,CIwintPSDIhi[2,],lwd=2,col="red")
legend("topleft",legend=c("low SDI","mean","high SDI"),col=c("blue",1,2),lwd=3)

## PRECIP x DBH
incPXlo <- matrix(NA,ns,length(wintPseq))
incPXhi <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.JJ"]*Xhl[1]*wintPseq +
    betas[j,"betawintP.JJ"]*wintPseq
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.JJ"]*Xhl[2]*wintPseq +
    betas[j,"betawintP.JJ"]*wintPseq
}
CIwintPXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIwintPXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))
plot(wintPseq,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
#PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
lines(wintPseq,CIwintPXlo[2,],lwd=2,col="blue")
lines(wintPseq,CIwintPXhi[2,],lwd=2,col="red")
legend("topleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
