# Adapted from M.E. Evans "ESAcode2017"

# This script plots the effects of SI, SICOND, Tree size, climate, etc on diameter increment. 
# We do this for:
# 1. January - July precipitation + all plot factors + all interactions
# 2. Water year precipitation + all plot factors + all interactions
# 3. Spring-fall Tmax + all plot factors + all interactions
# 4. Model with both Water year ppt and spring-fall tmax

# Note: This should probably be converted into a function:

## Note: must run data loading from RDriverFusion


#####################################################################################
# 1. Plot Effects for Winter Precip full model
#####################################################################################
jags.comb <- NULL

for(i in 125:150){
  load(paste0("/home/rstudio/pecan/modules/data.land/R/WintPPT.only.15000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      new.out[[j]] <- new.out[[j]][,-x.cols]}
    
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
saveRDS(jags.comb,file="IGF.Jan.Jul.PPT.rds")

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="Jan.Jul.PPT.RData")

# check for convergence using gelman.diag
gelman.diag(jags.comb)



# check for convergence via traceplots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WintP_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.JJ"], main = expression(beta~DBH_JanJulPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.JJ"], main = expression(beta~JanJulPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.JJ"], main = expression(beta~SDI_JanJulPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.JJ"], main = expression(beta~SI_JanJulPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()

# save autocorrelation plots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/JanJulPPT_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_wintP.JJ"], main = expression(beta~DBH_JanJulPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.JJ"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.JJ"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.JJ"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l)
dev.off()

# plot the correlation between posterior plots + histograms



png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/JanJulPPT_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", "mu", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()

# pairs(as.matrix(jags.comb[, c("deviance", "mu", colnames(betas))]), , gap=1/10)
# dev.off()
# 
# # plot histograms of the posterior samples of parameters
# params <- as.matrix(jags.comb[, c("deviance", "mu", colnames(betas))])



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

wateryrP <- mean(time_data$wintP.JJ,na.rm = TRUE)
wateryrPhl <- quantile(time_data$wintP.JJ,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)


# standardize the beta coefficients:

stdBeta <- rep(NA,11)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_wintP.JJ"])/SDI/wintP
stdBeta[5] <- mean(betas[,"betaSICOND_wintP.JJ"])/SI/wintP
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[6] <- mean(betas[,"betaX"])/x
stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_wintP.JJ"])/x/wintP
stdBeta[11] <- mean(betas[,"betawintP.JJ"])/wintP


names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)


# create plots of conditional effects 
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

CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X

source("/Users/kah/Documents/docker_pecan/pecan/visualization/R/ciEnvelope.R") # read in ci.envelope function

# plot as pseudo object to save for later
Tree.Size.Effect %<a-% {
  plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
  ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
  lines(xseq,CIX[2,],lwd=2)
  abline(h=0)
}


##SDI
hist(cov.data$SDI)

# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

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

SDI.Effect %<a-% {
  plot(
    SDIseq.real,
    CIsdi[2, ],
    ylim = c(0, max(CIsdi)),
    type = 'n',
    xlab = "Stand Density Index",
    ylab = "Diameter Increment (cm)",
    cex.lab = 1.5)
  ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
  lines(SDIseq.real, CIsdi[2, ], lwd = 2)
  abline(h = 0)
}

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

# SDI plot
SDI.DBH.Effect %<a-% {
  plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
  #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
  lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
  lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
  legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
  abline(h=0)
}

##SI
hist(cov.data$SI)
# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SIseq.real <- 20:60
SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
#SIseq <- 20:60
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

SI.Effect %<a-% {
  plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsi[2,],lwd=2)
  abline(h=0)
}


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

SI.DBH.Effect %<a-% {
  plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
  lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
  legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
}

## wintP
clim.data <- readRDS("/home/rstudio/pecan/FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$wintP.JJ)
wintPseq.real <- 0:800
wintPseq <- (wintPseq.real-mean(clim.data$wintP.JJ))/sd(clim.data$wintP.JJ)
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

Climate.Effect %<a-% {
  plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real, CIwintP[2,],lwd=2)
  abline(h=0)
}

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

Climate.SDI.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintPSDIlo[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSDIlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

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

Climate.DBH.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPXlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Precip X SI
incP_SIlo <- matrix(NA,ns,length(wintPseq))
incP_SIhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintPseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.JJ"]*x[1]*wintPseq +
    betas[j,"betawintP.JJ"]*wintPseq
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.JJ"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintPseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.JJ"]*x[1]*wintPseq +
    betas[j,"betawintP.JJ"]*wintPseq
}
CIwintPSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SI.Effect %<a-% {
  plot(wintPseq.real,CIwintPSIhi[2,],ylim=c(0,max(CIwintPSIhi[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSIlo[2,],lwd=2,col="grey")
  lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



## SI X SDI

incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
incSDI_SIhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.JJ"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintP + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.JJ"]*x[1]*wintP +
    betas[j,"betawintP.JJ"]*wintP
  
  incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.JJ"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    betas[j,"betaSICOND_wintP.JJ"]*SI*wintP + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.JJ"]*x[1]*wintP +
    betas[j,"betawintP.JJ"]*wintP
}
CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))

SDI.SI.Effect %<a-% {
  plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
  # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
  lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
  #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



plot.effects.pryr %<a-% {
  split.screen(c(2, 2))
  # plot effects
  screen(1)
  SI.Effect
  
  screen(2)
  SDI.Effect
  screen(3)
  Tree.Size.Effect
  screen(4)
  Climate.Effect
  close.screen(all=TRUE)
}

png(height = 10, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_effects_WintP.png")
plot.effects.pryr
dev.off()



# plot interactions
plot.interaction.effects.pryr %<a-%{
  split.screen(c(3, 2))
  screen(1)
  SI.DBH.Effect
  screen(2)
  SDI.DBH.Effect
  screen(3)
  Climate.DBH.Effect
  
  screen(4)
  Climate.SI.Effect
  screen(5)
  Climate.SDI.Effect
  screen(6)
  SDI.SI.Effect
  close.screen(all=TRUE)
}

png(height = 14, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_interaction_effects_WintP.png")
plot.interaction.effects.pryr
dev.off()


#####################################################################################
# 2. Plot Effects for Water Year Precip full model
#####################################################################################
jags.comb <- NULL

for(i in 140:150){ # note this model stopped early b/c convergence
  load(paste0("/home/rstudio/pecan/modules/data.land/R/WateryearPPT.only.15000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
    }
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
#save(jags.comb,file="IGF.waterYear.PPT.RData")
saveRDS(jags.comb,file="IGF.waterYear.PPT.rds")

# check for convergence via gelman-rubin
gelman.diag(jags.comb)


# check for convergence via traceplots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()

png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_JanJulPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l)
dev.off()




## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="WaterYear.PPT.RData")

png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", "mu", colnames(betas))]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()

# check to see if the plot randome effects are explained by any of the covariates:
alphas <- out[,grep(pattern = "alpha",colnames(out))]
alpha.m <- reshape2::melt(alphas)

alpha.summary <- alpha.m %>% group_by(Var2) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                          ci.lo = quantile(value, 0.025, na.rm =TRUE), 
                                                          ci.hi = quantile(value, 0.975, na.rm =TRUE))

PLOT.df <- data.frame(PLOT = unique(cov.data$PLOT), 
                      plt.num = 1:344)
alpha.summary$plt.num <- 1:344
cov.plots <- left_join(cov.data, PLOT.df)

alpha.summary.plt <- left_join(alpha.summary, cov.plots)

a <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= ELEV, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= ELEV, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

b <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SDI, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= SDI, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= SDI, y =mean), method = "lm")+ylab(expression(alpha~Plot))

c <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SICOND, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= SICOND, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

d <-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SLOPE, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= SLOPE, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

e <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= ASPECT, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= ASPECT, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

f <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= TRTCD1, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= TRTCD1, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

g <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= DSTRBCD1, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= DSTRBCD1, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

h <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STAGE2, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= STAGE2, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= STAGE2, y =mean), method = "lm")+ylab(expression(alpha~Plot))

i<- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STAGE3, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= STAGE3, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

j<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STDAGE, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= STDAGE, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= STDAGE, y =mean), method = "lm")+ylab(expression(alpha~Plot))

k<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= MAP, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= MAP, y =mean))+stat_smooth(data = alpha.summary.plt, aes(x= MAP, y =mean), method = "lm")+theme_bw()+ylab(expression(alpha~Plot))

l<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= MAT, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= MAT, y =mean))+stat_smooth(data = alpha.summary.plt, aes(x= MAT, y =mean), method = "lm")+theme_bw()+ylab(expression(alpha~Plot))


#ggplot()+geom_point(data = alpha.summary.plt, aes(x= MAP, y = SDI, color =mean))+scale_color_gradient(low = "blue", high = "red")+theme_bw()+ylab(expression(alpha~Plot))

png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_alpha_effects_by_envt.png")
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l)
dev.off()

# need to also look at climate variables:

## calculate an average tree
hist(z0)
x <- mean(z0,na.rm = TRUE)
Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
SDI <- mean(cov.data$SDI,na.rm = TRUE)
SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)
wintP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wintPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)
SI <- mean(cov.data$SICOND, na.rm = TRUE)
SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)

wateryrP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wateryrPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)


# standardize the beta coefficients:

stdBeta <- rep(NA,11)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_wintP.wateryr"])/SDI/wateryrP
stdBeta[5] <- mean(betas[,"betaSICOND_wintP.wateryr"])/SI/wateryrP
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[6] <- mean(betas[,"betaX"])/x
stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_wintP.wateryr"])/x/wateryrP
stdBeta[11] <- mean(betas[,"betawintP.wateryr"])/wateryrP


names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)


# create plots of conditional effects 
## Size
ns = 500 ## number of samples
i = sample.int(nrow(betas),ns)
xrng <- range(z0,na.rm = TRUE)
xseq <- seq(xrng[1],xrng[2],by=1)
xseq <- 1:58
incX <- matrix(NA,ns,length(xseq))
for(k in seq_along(i)){
  j <- i[k]
  incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + betas[j,"betaX"]*xseq + betas[j,"betaX2"]*xseq*xseq +
    betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
    betas[j,"betawintP.wateryr"]*wateryrP
}

CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X

# plot as pseudo object to save for later
Tree.Size.Effect %<a-% {
  plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
  ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
  lines(xseq,CIX[2,],lwd=2)
  abline(h=0)
}


##SDI
hist(cov.data$SDI)

# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

incSDI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))

SDI.Effect %<a-% {
  plot(
    SDIseq.real,
    CIsdi[2, ],
    ylim = c(0, max(CIsdi)),
    type = 'n',
    xlab = "Stand Density Index",
    ylab = "Diameter Increment (cm)",
    cex.lab = 1.5)
  ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
  lines(SDIseq.real, CIsdi[2, ], lwd = 2)
  abline(h = 0)
}

## SDI * size
incSDIXhi <- matrix(NA,ns,length(SDIseq))
incSDIXlo <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))

# SDI plot
SDI.DBH.Effect %<a-% {
  plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
  #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
  lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
  lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
  legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
  abline(h=0)
}

##SI
hist(cov.data$SI)
# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SIseq.real <- 20:60
SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
#SIseq <- 20:60
incSI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))

SI.Effect %<a-% {
  plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsi[2,],lwd=2)
  abline(h=0)
}


## SI x DBH
incSIXlo <- matrix(NA,ns,length(SDIseq))
incSIXmed <- matrix(NA,ns,length(SDIseq))
incSIXhi <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}

CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))

SI.DBH.Effect %<a-% {
  plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
  lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
  legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
}

## wintP
clim.data <- readRDS("/home/rstudio/pecan/FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$wintP.wateryr)
wintPseq.real <- 0:800
wintPseq <- (wintPseq.real-mean(clim.data$wintP.wateryr))/sd(clim.data$wintP.wateryr)
incP <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}

CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))

Climate.Effect %<a-% {
  plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real, CIwintP[2,],lwd=2)
  abline(h=0)
}

## PRECIP x SDI
incPSDIlo <- matrix(NA,ns,length(wintPseq))
incPSDIhi <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}

CIwintPSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SDI.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintPSDIlo[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSDIlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## PRECIP x DBH
incPXlo <- matrix(NA,ns,length(wintPseq))
incPXhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}

CIwintPXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIwintPXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

Climate.DBH.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPXlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Precip X SI
incP_SIlo <- matrix(NA,ns,length(wintPseq))
incP_SIhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}
CIwintPSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SI.Effect %<a-% {
  plot(wintPseq.real,CIwintPSIhi[2,],ylim=c(0,max(CIwintPSIhi[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSIlo[2,],lwd=2,col="grey")
  lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



## SI X SDI

incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
incSDI_SIhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  
  incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))

SDI.SI.Effect %<a-% {
  plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
  # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
  lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
  #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



plot.effects.pryr %<a-% {
  split.screen(c(2, 2))
  # plot effects
  screen(1)
  SI.Effect
  
  screen(2)
  SDI.Effect
  screen(3)
  Tree.Size.Effect
  screen(4)
  Climate.Effect
  close.screen(all=TRUE)
}

png(height = 10, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_effects_WaterYrP.png")
plot.effects.pryr
dev.off()



# plot interactions
plot.interaction.effects.pryr %<a-%{
  split.screen(c(3, 2))
  screen(1)
  SI.DBH.Effect
  screen(2)
  SDI.DBH.Effect
  screen(3)
  Climate.DBH.Effect
  
  screen(4)
  Climate.SI.Effect
  screen(5)
  Climate.SDI.Effect
  screen(6)
  SDI.SI.Effect
  close.screen(all=TRUE)
}

png(height = 14, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_interaction_effects_WaterYrP.png")
plot.interaction.effects.pryr
dev.off()


#####################################################################################
# 3. Plot Effects for Fall-Spring Tmax full model:
#####################################################################################
jags.comb <- NULL

for(i in 125:150){ # This model may not have converged
  load(paste0("/home/rstudio/pecan/modules/data.land/R/tmax.fallspr.Tmax.only.15000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      new.out[[j]] <- new.out[[j]][,-x.cols]
    }
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.Fall.spr.Tmax.RData")

# check for convergence via gelman-rubin
gelman.diag(jags.comb)

# check for convergence via traceplots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/tmax.fallspr_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~DBH_tmax.fallspr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~tmax.fallspr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~SDI_tmax.fallspr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~SI_tmax.fallspr), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()


png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/tmax.fallspr_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~DBH_tmax.fallspr), aspect = 1)
i<- acfplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~tmax.fallspr), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~SDI_tmax.fallspr), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~SI_tmax.fallspr), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l)
dev.off()


## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="FallSpr.Tmax.RData")


## calculate an average tree
hist(z0)
x <- mean(z0,na.rm = TRUE)
Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
SDI <- mean(cov.data$SDI,na.rm = TRUE)
SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)

SI <- mean(cov.data$SICOND, na.rm = TRUE)
SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)


# standardize the beta coefficients:

stdBeta <- rep(NA,11)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_tmax.fallspr"])/SDI/sprfallTmax
stdBeta[5] <- mean(betas[,"betaSICOND_tmax.fallspr"])/SI/sprfallTmax
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[6] <- mean(betas[,"betaX"])/x
stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_tmax.fallspr"])/x/sprfallTmax
stdBeta[11] <- mean(betas[,"betatmax.fallspr"])/sprfallTmax


names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)


# create plots of conditional effects 
## Size
ns = 500 ## number of samples
i = sample.int(nrow(betas),ns)
xrng <- range(z0,na.rm = TRUE)
xseq <- seq(xrng[1],xrng[2],by=1)
xseq <- 1:58
incX <- matrix(NA,ns,length(xseq))
for(k in seq_along(i)){
  j <- i[k]
  incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX"]*xseq + betas[j,"betaX2"]*xseq*xseq +
    betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_tmax.fallspr"]*xseq*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
}

CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X

# plot as pseudo object to save for later
Tree.Size.Effect %<a-% {
  plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
  ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
  lines(xseq,CIX[2,],lwd=2)
  abline(h=0)
}


##SDI
hist(cov.data$SDI)

# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

incSDI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
}
CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))

SDI.Effect %<a-% {
  plot(
    SDIseq.real,
    CIsdi[2, ],
    ylim = c(0, max(CIsdi)),
    type = 'n',
    xlab = "Stand Density Index",
    ylab = "Diameter Increment (cm)",
    cex.lab = 1.5)
  ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
  lines(SDIseq.real, CIsdi[2, ], lwd = 2)
  abline(h = 0)
}

## SDI * size
incSDIXhi <- matrix(NA,ns,length(SDIseq))
incSDIXlo <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
  incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
}
CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))

# SDI plot
SDI.DBH.Effect %<a-% {
  plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
  #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
  lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
  lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
  legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
  abline(h=0)
}

##SI
hist(cov.data$SI)
# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SIseq.real <- 20:60
SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
#SIseq <- 20:60
incSI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
}
CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))

SI.Effect %<a-% {
  plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsi[2,],lwd=2)
  abline(h=0)
}


## SI x DBH
incSIXlo <- matrix(NA,ns,length(SDIseq))
incSIXmed <- matrix(NA,ns,length(SDIseq))
incSIXhi <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
  incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
  incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
}

CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))

SI.DBH.Effect %<a-% {
  plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
  lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
  legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
}

## sprfallTmax
clim.data <- readRDS("/home/rstudio/pecan/FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$tmax.fallspr)
sprfallTmaxseq.real <- 15:30
sprfallTmaxseq <- (sprfallTmaxseq.real-mean(clim.data$tmax.fallspr))/sd(clim.data$tmax.fallspr)
incP <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq
}

CIsprfallTmax <- apply(incP,2,quantile,c(0.025,0.5,0.975))

Climate.Effect %<a-% {
  plot(sprfallTmaxseq.real, CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax + 0.05)),type='n',xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(sprfallTmaxseq.real,CIsprfallTmax[1,], CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real, CIsprfallTmax[2,],lwd=2)
  abline(h=0)
}

## TMAX x SDI
incPSDIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPSDIhi <- matrix(NA,ns,length(sprfallTmaxseq))
for(k in seq_along(i)){
  j <- i[k]
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmaxseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmaxseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq
}

CIsprfallTmaxSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SDI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmaxSDIlo[2,])),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)), ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## Tmax x DBH
incPXlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPXhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq
}

CIsprfallTmaxXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

Climate.DBH.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax)),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Tmax X SI
incP_SIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incP_SIhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq
}
CIsprfallTmaxSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],ylim=c(0,max(CIsprfallTmaxSIhi[2,])),type='l',lwd=3, xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIlo[2,],lwd=2,col="grey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



## SI X SDI

incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
incSDI_SIhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
  
  incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_tmax.fallspr"]*x[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax
}
CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))

SDI.SI.Effect %<a-% {
  plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
  # #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "purple")
  lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
  #lines(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



plot.effects.pryr %<a-% {
  split.screen(c(2, 2))
  # plot effects
  screen(1)
  SI.Effect
  
  screen(2)
  SDI.Effect
  screen(3)
  Tree.Size.Effect
  screen(4)
  Climate.Effect
  close.screen(all=TRUE)
}

png(height = 10, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_effects_sprfallTmax.png")
plot.effects.pryr
dev.off()



# plot interactions
plot.interaction.effects.pryr %<a-%{
  split.screen(c(3, 2))
  screen(1)
  SI.DBH.Effect
  screen(2)
  SDI.DBH.Effect
  screen(3)
  Climate.DBH.Effect
  
  screen(4)
  Climate.SI.Effect
  screen(5)
  Climate.SDI.Effect
  screen(6)
  SDI.SI.Effect
  close.screen(all=TRUE)
}

png(height = 14, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_interaction_effects_sprfallTmax.png")
plot.interaction.effects.pryr
dev.off()

#####################################################################################
# 4. Plot Effects for Water Year Precip + Spring - Fall Tmax full model
#####################################################################################
jags.comb <- NULL

for(i in 148:150){ # note this model stopped early b/c convergence
  load(paste0("/home/rstudio/pecan/modules/data.land/R/ppt.wateryear.tmax.fallspr.Tmax.only.15000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
    }
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.waterYear.PPT.fall.spr.TMAX.RData")

# check for convergence via gelman-rubin
gelman.diag(jags.comb)

# check for convergence via traceplots
png(height = 12, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_Fall.SprTMAX_traceplots_effects.png")
par(mfrow=c(6,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_JanJulPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betatmax.fallspr_wintP.wateryr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")

dev.off()

png(height = 12, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_Fall.SprTMAX_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_JanJulPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
m <- acfplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
n <- acfplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
o <- acfplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
p <- acfplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
q <- acfplot(jags.comb[, "betatmax.fallspr_wintP.wateryr"], main = expression(beta~SI_TMAX.fs), aspect = 1)


grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l, m,n,o,p)
dev.off()




## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="WaterYear.PPT.Fall.spr.TMAX.RData")



## calculate an average tree
hist(z0)
x <- mean(z0,na.rm = TRUE)
Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
SDI <- mean(cov.data$SDI,na.rm = TRUE)
SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)
wintP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wintPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)
SI <- mean(cov.data$SICOND, na.rm = TRUE)
SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)

wateryrP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wateryrPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)


# standardize the beta coefficients:

stdBeta <- rep(NA,17)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_wintP.wateryr"])/SDI/wateryrP
stdBeta[5] <- mean(betas[,"betaSICOND_wintP.wateryr"])/SI/wateryrP
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[6] <- mean(betas[,"betaX"])/x
stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_wintP.wateryr"])/x/wateryrP
stdBeta[11] <- mean(betas[,"betawintP.wateryr"])/wateryrP
stdBeta[12] <- mean(betas[,"betaX_tmax.fallspr"])/x/sprfallTmax
stdBeta[12] <- mean(betas[,"betatmax.fallspr"])/sprfallTmax
stdBeta[13] <- mean(betas[,"betaSDI_tmax.fallspr"])/SDI/sprfallTmax
stdBeta[14] <- mean(betas[,"betaSICOND_tmax.fallspr"])/SI/sprfallTmax
stdBeta[15] <- mean(betas[,"betatmax.fallspr_wintP.wateryr"])/wateryrP/sprfallTmax

names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)


# create plots of conditional effects 
## Size
ns = 500 ## number of samples
i = sample.int(nrow(betas),ns)
xrng <- range(z0,na.rm = TRUE)
xseq <- seq(xrng[1],xrng[2],by=1)
xseq <- 1:58
incX <- matrix(NA,ns,length(xseq))
for(k in seq_along(i)){
  j <- i[k]
  incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + betas[j,"betaX"]*xseq + betas[j,"betaX2"]*xseq*xseq +
    betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
    betas[j,"betawintP.wateryr"]*wateryrP + 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*xseq*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  
}


CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X

# plot as pseudo object to save for later
Tree.Size.Effect %<a-% {
  plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
  ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
  lines(xseq,CIX[2,],lwd=2)
  abline(h=0)
}


##SDI
hist(cov.data$SDI)

# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

incSDI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}
CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))

SDI.Effect %<a-% {
  plot(
    SDIseq.real,
    CIsdi[2, ],
    ylim = c(0, max(CIsdi)),
    type = 'n',
    xlab = "Stand Density Index",
    ylab = "Diameter Increment (cm)",
    cex.lab = 1.5)
  ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
  lines(SDIseq.real, CIsdi[2, ], lwd = 2)
  abline(h = 0)
}

## SDI * size
incSDIXhi <- matrix(NA,ns,length(SDIseq))
incSDIXlo <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}
CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))

# SDI plot
SDI.DBH.Effect %<a-% {
  plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
  #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
  lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
  lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
  abline(h=0)
}

##SI
hist(cov.data$SI)
# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SIseq.real <- 20:60
SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
#SIseq <- 20:60
incSI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}
CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))

SI.Effect %<a-% {
  plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsi[2,],lwd=2)
  abline(h=0)
}


## SI x DBH
incSIXlo <- matrix(NA,ns,length(SDIseq))
incSIXmed <- matrix(NA,ns,length(SDIseq))
incSIXhi <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}

CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))

SI.DBH.Effect %<a-% {
  plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
  lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
  legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
}

## wintP
clim.data <- readRDS("/home/rstudio/pecan/FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$wintP.wateryr)
wintPseq.real <- 0:800
wintPseq <- (wintPseq.real-mean(clim.data$wintP.wateryr))/sd(clim.data$wintP.wateryr)
incP <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}

CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))

Climate.Effect %<a-% {
  plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real, CIwintP[2,],lwd=2)
  abline(h=0)
}

## PRECIP x SDI
incPSDIlo <- matrix(NA,ns,length(wintPseq))
incPSDIhi <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}

CIwintPSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SDI.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintPSDIlo[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSDIlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## PRECIP x DBH
incPXlo <- matrix(NA,ns,length(wintPseq))
incPXhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}

CIwintPXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIwintPXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

Climate.DBH.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPXlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Precip X SI
incP_SIlo <- matrix(NA,ns,length(wintPseq))
incP_SIhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}
CIwintPSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SI.Effect %<a-% {
  plot(wintPseq.real,CIwintPSIhi[2,],ylim=c(0,max(CIwintPSIhi[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSIlo[2,],lwd=2,col="grey")
  lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



## SI X SDI

incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
incSDI_SIhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintP
  
  
  incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + betas[j,"betaX2"]*x[1]*x[1] +
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP +
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintP
  
}
CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))

SDI.SI.Effect %<a-% {
  plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
  # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
  lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
  #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}

# also plot all temperature effects and interactions:

## sprfallTmax
clim.data <- readRDS("/home/rstudio/pecan/FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$tmax.fallspr)
sprfallTmaxseq.real <- 15:30
sprfallTmaxseq <- (sprfallTmaxseq.real-mean(clim.data$tmax.fallspr))/sd(clim.data$tmax.fallspr)
incP <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
}

CIsprfallTmax <- apply(incP,2,quantile,c(0.025,0.5,0.975))

TMAX.Effect %<a-% {
  plot(sprfallTmaxseq.real, CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax + 0.05)),type='n',xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(sprfallTmaxseq.real,CIsprfallTmax[1,], CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real, CIsprfallTmax[2,],lwd=2)
  abline(h=0)
}

## TMAX x SDI
incPSDIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPSDIhi <- matrix(NA,ns,length(sprfallTmaxseq))
for(k in seq_along(i)){
  j <- i[k]
  
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
}


CIsprfallTmaxSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

TMAX.SDI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmaxSDIlo[2,])),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)), ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## Tmax x DBH
incPXlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPXhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + betas[j,"betaX2"]*Xhl[1]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + betas[j,"betaX2"]*Xhl[2]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
}

CIsprfallTmaxXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

TMAX.DBH.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax)),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Tmax X SI
incP_SIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incP_SIhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1]+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SIhl[1]*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1]+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SIhl[2]*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
}

CIsprfallTmaxSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

TMAX.SI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],ylim=c(0,max(CIsprfallTmaxSIlo[2,])),type='l',lwd=3, xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIlo[2,],lwd=2,col="grey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}


# finally: Interaction between water year PPT and Tmax

## Tmax X PPT
incP_Tmaxlo <- matrix(NA,ns,length(wintPseq))
incP_Tmaxhi <- matrix(NA,ns,length(wintPseq))

Tmaxhl <- quantile(sprfallTmaxseq,c(1/6,5/6),na.rm = TRUE)

for(k in seq_along(i)){
  j <- i[k]
  incP_Tmaxlo [k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq  +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*Tmaxhl[1]+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*Tmaxhl[1]+ 
    betas[j,"betaX_tmax.fallspr"]*x*Tmaxhl[1] +
    betas[j,"betatmax.fallspr"]*Tmaxhl[1] + betas[j,"betatmax.fallspr"]*Tmaxhl[1]*wintPseq 
  
  incP_Tmaxhi [k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + betas[j,"betaX2"]*x*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq  +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*Tmaxhl[2]+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*Tmaxhl[2]+ 
    betas[j,"betaX_tmax.fallspr"]*x*Tmaxhl[2] +
    betas[j,"betatmax.fallspr"]*Tmaxhl[2] + betas[j,"betatmax.fallspr"]*Tmaxhl[2]*wintPseq 
  
}
CIsprfallTmaxPPTlo <- apply(incP_Tmaxlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxPPThi <- apply( incP_Tmaxhi,2,quantile,c(0.025,0.5,0.975))

TMAX.PPT.Effect %<a-% {
  plot(wintPseq.real,CIsprfallTmaxPPThi[2,],ylim=c(0,0.5),type='l',lwd=3, xlab=expression("Water Year Precipitation  (mm)"),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(wintPseq.real,CIsprfallTmaxPPTlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIsprfallTmaxPPThi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low Temp","high Temp"),col=c("blue", "red"),lwd=3)
}


plot.effects.pryr %<a-% {
  split.screen(c(3, 2))
  # plot effects
  screen(1)
  SI.Effect
  
  screen(2)
  SDI.Effect
  screen(3)
  Tree.Size.Effect
  screen(4)
  Climate.Effect
  screen(5)
  TMAX.Effect
  close.screen(all=TRUE)
}

png(height = 12, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_effects_WaterYrP_TmaxFallSpr.png")
plot.effects.pryr
dev.off()



# plot interactions
plot.interaction.effects.pryr %<a-%{
  split.screen(c(4, 3))
  screen(1)
  SI.DBH.Effect
  screen(2)
  SDI.DBH.Effect
  screen(3)
  Climate.DBH.Effect
  
  screen(4)
  Climate.SI.Effect
  screen(5)
  Climate.SDI.Effect
  screen(6)
  SDI.SI.Effect
  
  screen(7)
  TMAX.DBH.Effect
  
  screen(8)
  TMAX.SI.Effect
  screen(9)
  TMAX.SDI.Effect
  screen(10)
  TMAX.PPT.Effect
  close.screen(all=TRUE)
}

png(height = 14, width = 10, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_interaction_effects_WaterYrP_TmaxFallSpr.png")
plot.interaction.effects.pryr
dev.off()



#####################################################################################
# 5. Plot Traces for Water Year Precip without plot random effects full model
#####################################################################################

jags.comb <- NULL

for(i in 148:150){ 
  load(paste0("/home/rstudio/pecan/modules/data.land/R/WateryearPPT.no.random.15000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
    }
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.waterYear.PPT.no.random.RData")

# check for convergence via gelman-rubin
gelman.diag(jags.comb)

# check for convergence via traceplots
png(height = 12, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_Fall.no.random_traceplots_effects.png")
par(mfrow=c(6,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()

png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_Fall.no.random_ACF_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_JanJulPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l)
dev.off()


## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="WaterYear.PPT.no.random.RData")

png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_no_random_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", "mu", colnames(betas))]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()


#####################################################################################
#  Plot Effects for Water year Precip full model (with out Mu)
#####################################################################################
jags.comb <- NULL

for(i in 125:150){
  load(paste0("/home/rstudio/pecan/modules/data.land/R/WateryearPPT.noMu.r.15000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      new.out[[j]] <- new.out[[j]][,-x.cols]}
    
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.WaterYear.PPT.noMU.r.RData")

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
#B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="WaterYear.PPT.noMU.r.RData")

# check for convergence using gelman.diag
gelman.diag(jags.comb)



# check for convergence via traceplots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WaterYear_noMU_r_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()

# save autocorrelation plots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WaterYearPPT_noMu_r_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
#b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,c,d,e,f,g,h,i,j,k,l)
dev.off()

# plot the correlation between posterior plots + histograms



png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/wateryearPPT_noMu_r_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()


#####################################################################################
#  Plot Effects for Water year Precip reduced model (with out Mu & without X interactions) 
#####################################################################################
jags.comb <- NULL

for(i in 125:150){
  load(paste0("/home/rstudio/pecan/modules/data.land/R/redWateryearPPT.noMu.15000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      new.out[[j]] <- new.out[[j]][,-x.cols]}
    
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.red.WaterYear.PPT.noMU.RData")

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
#B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="redWaterYear.PPT.noMU.RData")

# check for convergence using gelman.diag
gelman.diag(jags.comb)



# check for convergence via traceplots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/redWaterYear_noMU_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()

# save autocorrelation plots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/redWaterYearPPT_noMu_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
#b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
#f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
#g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
#h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
#j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
#k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
#l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,c,d,e,i)
dev.off()

# plot the correlation between posterior plots + histograms



png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/red_wateryearPPT_noMu_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()


#####################################################################################
#  Plot Effects for Water year Precip reduced model (without X interactions) 
#####################################################################################
jags.comb <- NULL

for(i in 25:50){
  load(paste0("/home/rstudio/pecan/modules/data.land/R/PPT.noXint.only.l0000.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      new.out[[j]] <- new.out[[j]][,-x.cols]}
    
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.PPT.noXint.RData")

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="PPT.noXint.RData")

# check for convergence using gelman.diag
gelman.diag(jags.comb)



# check for convergence via traceplots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPTWaterYear_noXint_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()

# save autocorrelation plots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WaterYearPPT_noXint_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
#f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
#g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
#h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,d,e,i, j, k, l)
dev.off()

# plot the correlation between posterior plots + histograms



png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT_noXint_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()


#####################################################################################
#  Plot Effects for Water year Precip reduced model (without X2) 
#####################################################################################
jags.comb <- NULL

for(i in 90:100){
  load(paste0("/Users/kah/Documents/docker_pecan/pecan/IGF_PIPO_AZ_mcmc/PPT.noX2.r.10000.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      new.out[[j]] <- new.out[[j]][,-x.cols]}
    
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.PPT.noX2.noMu.r2.RData")

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="PPT.noX2.noMu.r2.RData")

# check for convergence using gelman.diag
gelman.diag(jags.comb)



# check for convergence via traceplots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPTWaterYear_noX2_noMu_r2_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND"], main = expression(beta~SICOND), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
dev.off()

# save autocorrelation plots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WaterYearPPT_noX2_noMu_r2_ACFplots_effects.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
d<- acfplot(jags.comb[, "betaSICOND"], main = expression(beta~SI), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,d,e,f,g,h,i, j, k, l)
dev.off()

# plot the correlation between posterior plots + histograms



png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT_noX2_noMu_r2_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()



# check to see if the plot randome effects are explained by any of the covariates:
alphas <- out[,grep(pattern = "alpha",colnames(out))]
alpha.m <- reshape2::melt(alphas)

alpha.summary <- alpha.m %>% group_by(Var2) %>% summarise(mean = mean(value, na.rm = TRUE), 
                                                          ci.lo = quantile(value, 0.025, na.rm =TRUE), 
                                                          ci.hi = quantile(value, 0.975, na.rm =TRUE))

PLOT.df <- data.frame(PLOT = unique(cov.data$PLOT), 
                      plt.num = 1:344)
alpha.summary$plt.num <- 1:344
cov.plots <- left_join(cov.data, PLOT.df)

alpha.summary.plt <- left_join(alpha.summary, cov.plots)

a <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= ELEV, ymin = ci.lo, ymax = ci.hi, color = FIRE), width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= ELEV, y =mean, color = FIRE))+theme_bw()+ylab(expression(alpha~Plot))


b <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SDI, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= SDI, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= SDI, y =mean), method = "lm")+ylab(expression(alpha~Plot))

c <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SICOND, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= SICOND, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

d <-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= SLOPE, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= SLOPE, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

e <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= ASPECT, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= ASPECT, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

f <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= TRTCD1, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= TRTCD1, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

g <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= DSTRBCD1, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= DSTRBCD1, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

h <- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STAGE2, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= STAGE2, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= STAGE2, y =mean), method = "lm")+ylab(expression(alpha~Plot))

i<- ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STAGE3, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= STAGE3, y =mean))+theme_bw()+ylab(expression(alpha~Plot))

j<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= STDAGE, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= STDAGE, y =mean))+theme_bw()+stat_smooth(data = alpha.summary.plt, aes(x= STDAGE, y =mean), method = "lm")+ylab(expression(alpha~Plot))

k<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= MAP, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= MAP, y =mean))+stat_smooth(data = alpha.summary.plt, aes(x= MAP, y =mean), method = "lm")+theme_bw()+ylab(expression(alpha~Plot))

l<-ggplot()+geom_errorbar(data = alpha.summary.plt, aes(x= MAT, ymin = ci.lo, ymax = ci.hi), color = "darkgrey", width = 0)+
  geom_point(data = alpha.summary.plt, aes(x= MAT, y =mean))+stat_smooth(data = alpha.summary.plt, aes(x= MAT, y =mean), method = "lm")+theme_bw()+ylab(expression(alpha~Plot))


#ggplot()+geom_point(data = alpha.summary.plt, aes(x= MAP, y = SDI, color =mean))+scale_color_gradient(low = "blue", high = "red")+theme_bw()+ylab(expression(alpha~Plot))

png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_noX2_alpha_effects_by_envt.png")
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l)
dev.off()

# need to also look at climate variables:

## calculate an average tree
hist(z0)
x <- mean(z0,na.rm = TRUE)
Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
SDI <- mean(cov.data$SDI,na.rm = TRUE)
SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)
wintP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wintPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)
SI <- mean(cov.data$SICOND, na.rm = TRUE)
SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)

wateryrP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wateryrPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)


# standardize the beta coefficients:

stdBeta <- rep(NA,11)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_wintP.wateryr"])/SDI/wateryrP
stdBeta[5] <- mean(betas[,"betaSICOND_wintP.wateryr"])/SI/wateryrP
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[6] <- mean(betas[,"betaX"])/x
#stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_wintP.wateryr"])/x/wateryrP
stdBeta[11] <- mean(betas[,"betawintP.wateryr"])/wateryrP


names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)


# create plots of conditional effects 
## Size
ns = 500 ## number of samples
i = sample.int(nrow(betas),ns)
xrng <- range(z0,na.rm = TRUE)
xseq <- seq(xrng[1],xrng[2],by=1)
xseq <- 1:58
incX <- matrix(NA,ns,length(xseq))


for(k in seq_along(i)){
  j <- i[k]
  incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + betas[j,"betaX"]*xseq + 
    betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
    betas[j,"betawintP.wateryr"]*wateryrP
}

CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X

# plot as pseudo object to save for later
Tree.Size.Effect %<a-% {
  plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
  ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
  lines(xseq,CIX[2,],lwd=2)
  abline(h=0)
}


##SDI
hist(cov.data$SDI)

# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

incSDI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x +
    betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))

SDI.Effect %<a-% {
  plot(
    SDIseq.real,
    CIsdi[2, ],
    ylim = c(0, max(CIsdi)),
    type = 'n',
    xlab = "Stand Density Index",
    ylab = "Diameter Increment (cm)",
    cex.lab = 1.5)
  ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
  lines(SDIseq.real, CIsdi[2, ], lwd = 2)
  abline(h = 0)
}

## SDI * size
incSDIXhi <- matrix(NA,ns,length(SDIseq))
incSDIXlo <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))

# SDI plot
SDI.DBH.Effect %<a-% {
  plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
  #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
  lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
  lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
  legend("topright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
  abline(h=0)
}

##SI
hist(cov.data$SI)
# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SIseq.real <- 20:60
SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
#SIseq <- 20:60
incSI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))

SI.Effect %<a-% {
  plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsi[2,],lwd=2)
  abline(h=0)
}


## SI x DBH
incSIXlo <- matrix(NA,ns,length(SDIseq))
incSIXmed <- matrix(NA,ns,length(SDIseq))
incSIXhi <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] +
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}

CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))

SI.DBH.Effect %<a-% {
  plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
  lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
  legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
}

## wintP
clim.data <- readRDS("/home/rstudio/pecan/FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$wintP.wateryr)
wintPseq.real <- 0:800
wintPseq <- (wintPseq.real-mean(clim.data$wintP.wateryr))/sd(clim.data$wintP.wateryr)
incP <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}

CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))

Climate.Effect %<a-% {
  plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real, CIwintP[2,],lwd=2)
  abline(h=0)
}

## PRECIP x SDI
incPSDIlo <- matrix(NA,ns,length(wintPseq))
incPSDIhi <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}

CIwintPSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SDI.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintPSDIlo[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSDIlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## PRECIP x DBH
incPXlo <- matrix(NA,ns,length(wintPseq))
incPXhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}

CIwintPXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIwintPXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

Climate.DBH.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPXlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Precip X SI
incP_SIlo <- matrix(NA,ns,length(wintPseq))
incP_SIhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
}
CIwintPSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SI.Effect %<a-% {
  plot(wintPseq.real,CIwintPSIhi[2,],ylim=c(0,max(CIwintPSIhi[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSIlo[2,],lwd=2,col="grey")
  lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



## SI X SDI

incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
incSDI_SIhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
  
  incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP
}
CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))

SDI.SI.Effect %<a-% {
  plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
  # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
  lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
  #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



plot.effects.pryr %<a-% {
  split.screen(c(2, 2))
  # plot effects
  screen(1)
  SI.Effect
  
  screen(2)
  SDI.Effect
  screen(3)
  Tree.Size.Effect
  screen(4)
  Climate.Effect
  close.screen(all=TRUE)
}

png(height = 10, width = 6.5, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/Full_effects_WaterYrP_noX2.png")
plot.effects.pryr
dev.off()



# plot interactions
plot.interaction.effects.pryr %<a-%{
  split.screen(c(3, 2))
  screen(1)
  SI.DBH.Effect
  screen(2)
  SDI.DBH.Effect
  screen(3)
  Climate.DBH.Effect
  
  screen(4)
  Climate.SI.Effect
  screen(5)
  Climate.SDI.Effect
  screen(6)
  SDI.SI.Effect
  close.screen(all=TRUE)
}

png(height = 14, width = 6.5, units = "in", res = 200, "IGF_outputs/Full_interaction_effects_WaterYrP_noX2.png")
plot.interaction.effects.pryr
dev.off()


# plot all beta params side by side:

betas.m <- reshape2::melt(betas)
colnames(betas.m) <- c("MCMC", "beta", "estimates")
betas.summary <- betas.m %>% group_by(beta) %>% summarise (med = quantile(estimates, 0.5, na.rm =TRUE), 
                                                           ci.lo = quantile(estimates, 0.025, na.rm = TRUE), 
                                                           ci.hi = quantile(estimates, 0.975, na.rm = TRUE))


ggplot(betas.summary, aes(beta, med))+geom_point()+
  geom_errorbar(data = betas.summary, aes(x = beta, ymin = ci.lo, ymax = ci.hi), width = 0)+theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("Parameters")+xlab(" ")




#####################################################################################
#  Plot Effects for Water year Precip + TMax reduced model (without X2) 
#####################################################################################
jags.comb <- NULL

for(i in 225:250){
  load(paste0("/Users/kah/Documents/docker_pecan/pecan/IGF_PIPO_AZ_mcmc/PPT.TMAXfs.noX2.25000.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      new.out[[j]] <- new.out[[j]][,-x.cols]}
    
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
saveRDS(jags.comb,file="IGF.PPT.Tmax.noX2.noMu.rds")

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out,  file="PPT.noX2.noMu.r2.RData")

# check for convergence using gelman.diag
gelman.diag(jags.comb)



# check for convergence via traceplots
png(height = 14, width = 10, units = "in", res = 200, "IGF_outputs/PPT.Tmaxfs.WaterYear_noX2_traceplots_effects.png")
par(mfrow=c(6,4))
# plot all the traces for all monitored parameters except for the plot random effects:
alpha.cols <- grep(pattern = "alpha", colnames(jags.comb[[1]]))
jags.comb.subset <- list()
for(j in 1:3){
  jags.comb.subset[[j]] <- jags.comb[[j]][,-alpha.cols]
}

traceplot(jags.comb.subset)
dev.off()

# save autocorrelation plots
png(height = 10, width = 8, units = "in", res = 200, "IGF_outputs/PPT.Tmax.WaterYear_noX2_ACFplots_effects.png")

a <- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b <- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c <- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
#d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e <- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f <- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g <- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h <- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), aspect = 1)
i <- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j <- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k <- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l <- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,e,f,g,h,i, j, k, l)
dev.off()

# plot the correlation between posterior plots + histograms



png(height = 10, width = 12, units = "in", res = 200, "IGF_outputs/PPT.Tmax.fs.WaterYear_noX2_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, file="WaterYear.PPT.Fall.spr.TMAX.noX2.RData")



## calculate an average tree
hist(z0)
x <- mean(z0,na.rm = TRUE)
Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
SDI <- mean(cov.data$SDI,na.rm = TRUE)
SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)
wintP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wintPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)
SI <- mean(cov.data$SICOND, na.rm = TRUE)
SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)

wateryrP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wateryrPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)


# standardize the beta coefficients:

stdBeta <- rep(NA,17)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_wintP.wateryr"])/SDI/wateryrP
stdBeta[5] <- mean(betas[,"betaSICOND_wintP.wateryr"])/SI/wateryrP
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[6] <- mean(betas[,"betaX"])/x
stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_wintP.wateryr"])/x/wateryrP
stdBeta[11] <- mean(betas[,"betawintP.wateryr"])/wateryrP
stdBeta[12] <- mean(betas[,"betaX_tmax.fallspr"])/x/sprfallTmax
stdBeta[12] <- mean(betas[,"betatmax.fallspr"])/sprfallTmax
stdBeta[13] <- mean(betas[,"betaSDI_tmax.fallspr"])/SDI/sprfallTmax
stdBeta[14] <- mean(betas[,"betaSICOND_tmax.fallspr"])/SI/sprfallTmax
stdBeta[15] <- mean(betas[,"betatmax.fallspr_wintP.wateryr"])/wateryrP/sprfallTmax

names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)


# create plots of conditional effects 
## Size
ns = 500 ## number of samples
i = sample.int(nrow(betas),ns)
xrng <- range(z0,na.rm = TRUE)
xseq <- seq(xrng[1],xrng[2],by=1)
xseq <- 1:58
incX <- matrix(NA,ns,length(xseq))
for(k in seq_along(i)){
  j <- i[k]
  incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + betas[j,"betaX"]*xseq + 
    betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
    betas[j,"betawintP.wateryr"]*wateryrP + 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*xseq*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  
}


CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X

# plot as pseudo object to save for later
Tree.Size.Effect %<a-% {
  plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
  ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
  lines(xseq,CIX[2,],lwd=2)
  abline(h=0)
}


##SDI
hist(cov.data$SDI)

# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

incSDI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x
  betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}
CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))

SDI.Effect %<a-% {
  plot(
    SDIseq.real,
    CIsdi[2, ],
    ylim = c(0, max(CIsdi)),
    type = 'n',
    xlab = "Stand Density Index",
    ylab = "Diameter Increment (cm)",
    cex.lab = 1.5)
  ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
  lines(SDIseq.real, CIsdi[2, ], lwd = 2)
  abline(h = 0)
}

## SDI * size
incSDIXhi <- matrix(NA,ns,length(SDIseq))
incSDIXlo <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}
CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))

# SDI plot
SDI.DBH.Effect %<a-% {
  plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
  #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
  lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
  lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
  abline(h=0)
}

##SI
hist(cov.data$SI)
# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SIseq.real <- 20:60
SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
#SIseq <- 20:60
incSI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}
CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))

SI.Effect %<a-% {
  plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsi[2,],lwd=2)
  abline(h=0)
}


## SI x DBH
incSIXlo <- matrix(NA,ns,length(SDIseq))
incSIXmed <- matrix(NA,ns,length(SDIseq))
incSIXhi <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
  incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wateryrP
  
}

CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))

SI.DBH.Effect %<a-% {
  plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
  lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
  legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
}

## wintP
clim.data <- readRDS("FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$wintP.wateryr)
wintPseq.real <- 0:800
wintPseq <- (wintPseq.real-mean(clim.data$wintP.wateryr))/sd(clim.data$wintP.wateryr)
incP <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}

CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))

Climate.Effect %<a-% {
  plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real, CIwintP[2,],lwd=2)
  abline(h=0)
}

## PRECIP x SDI
incPSDIlo <- matrix(NA,ns,length(wintPseq))
incPSDIhi <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}

CIwintPSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SDI.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintPSDIlo[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSDIlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## PRECIP x DBH
incPXlo <- matrix(NA,ns,length(wintPseq))
incPXhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}

CIwintPXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIwintPXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

Climate.DBH.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPXlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Precip X SI
incP_SIlo <- matrix(NA,ns,length(wintPseq))
incP_SIhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintPseq
  
}
CIwintPSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SI.Effect %<a-% {
  plot(wintPseq.real,CIwintPSIhi[2,],ylim=c(0,max(CIwintPSIhi[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSIlo[2,],lwd=2,col="grey")
  lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



## SI X SDI

incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
incSDI_SIhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintP
  
  
  incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP +
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr"]*sprfallTmax*wintP
  
}
CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))

SDI.SI.Effect %<a-% {
  plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
  # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
  lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
  #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}

# also plot all temperature effects and interactions:

## sprfallTmax
clim.data <- readRDS("FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$tmax.fallspr)
sprfallTmaxseq.real <- 15:30
sprfallTmaxseq <- (sprfallTmaxseq.real-mean(clim.data$tmax.fallspr))/sd(clim.data$tmax.fallspr)
incP <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
}

CIsprfallTmax <- apply(incP,2,quantile,c(0.025,0.5,0.975))

TMAX.Effect %<a-% {
  plot(sprfallTmaxseq.real, CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax + 0.05)),type='n',xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(sprfallTmaxseq.real,CIsprfallTmax[1,], CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real, CIsprfallTmax[2,],lwd=2)
  abline(h=0)
}

## TMAX x SDI
incPSDIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPSDIhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
}


CIsprfallTmaxSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

TMAX.SDI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmaxSDIlo[2,])),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)), ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## Tmax x DBH
incPXlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPXhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
}

CIsprfallTmaxXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

TMAX.DBH.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax)),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Tmax X SI
incP_SIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incP_SIhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1]+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SIhl[1]*wintP + betas[j,"betaX"]*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1]+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SIhl[2]*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr"]*sprfallTmaxseq*wintP
  
}

CIsprfallTmaxSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

TMAX.SI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],ylim=c(0,max(CIsprfallTmaxSIlo[2,])),type='l',lwd=3, xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIlo[2,],lwd=2,col="grey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}


# finally: Interaction between water year PPT and Tmax

## Tmax X PPT
incP_Tmaxlo <- matrix(NA,ns,length(wintPseq))
incP_Tmaxhi <- matrix(NA,ns,length(wintPseq))

Tmaxhl <- quantile(sprfallTmaxseq,c(1/6,5/6),na.rm = TRUE)

for(k in seq_along(i)){
  j <- i[k]
  incP_Tmaxlo [k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq  +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*Tmaxhl[1]+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*Tmaxhl[1]+ 
    betas[j,"betaX_tmax.fallspr"]*x*Tmaxhl[1] +
    betas[j,"betatmax.fallspr"]*Tmaxhl[1] + betas[j,"betatmax.fallspr"]*Tmaxhl[1]*wintPseq 
  
  incP_Tmaxhi [k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x +
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq  +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*Tmaxhl[2]+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*Tmaxhl[2]+ 
    betas[j,"betaX_tmax.fallspr"]*x*Tmaxhl[2] +
    betas[j,"betatmax.fallspr"]*Tmaxhl[2] + betas[j,"betatmax.fallspr"]*Tmaxhl[2]*wintPseq 
  
}
CIsprfallTmaxPPTlo <- apply(incP_Tmaxlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxPPThi <- apply( incP_Tmaxhi,2,quantile,c(0.025,0.5,0.975))

TMAX.PPT.Effect %<a-% {
  plot(wintPseq.real,CIsprfallTmaxPPThi[2,],ylim=c(0,0.5),type='l',lwd=3, xlab=expression("Water Year Precipitation  (mm)"),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(wintPseq.real,CIsprfallTmaxPPTlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIsprfallTmaxPPThi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low Temp","high Temp"),col=c("blue", "red"),lwd=3)
}


plot.effects.pryr %<a-% {
  split.screen(c(3, 2))
  # plot effects
  screen(1)
  SI.Effect
  
  screen(2)
  SDI.Effect
  screen(3)
  Tree.Size.Effect
  screen(4)
  Climate.Effect
  screen(5)
  TMAX.Effect
  close.screen(all=TRUE)
}

png(height = 12, width = 6.5, units = "in", res = 200, "IGF_outputs/Full_effects_WaterYrP_TmaxFallSpr_noX2.png")
plot.effects.pryr
dev.off()



# plot interactions
plot.interaction.effects.pryr %<a-%{
  split.screen(c(4, 3))
  screen(1)
  SI.DBH.Effect
  screen(2)
  SDI.DBH.Effect
  screen(3)
  Climate.DBH.Effect
  
  screen(4)
  Climate.SI.Effect
  screen(5)
  Climate.SDI.Effect
  screen(6)
  SDI.SI.Effect
  
  screen(7)
  TMAX.DBH.Effect
  
  screen(8)
  TMAX.SI.Effect
  screen(9)
  TMAX.SDI.Effect
  screen(10)
  TMAX.PPT.Effect
  close.screen(all=TRUE)
}

png(height = 14, width = 10, units = "in", res = 200, "IGF_outputs/Full_interaction_effects_WaterYrP_TmaxFallSpr_noX2.png")
plot.interaction.effects.pryr
dev.off()




#####################################################################################
#  Water year Precip model (without X2) With trees with no cores!
#####################################################################################
jags.comb <- NULL

for(i in 125:150){
  load(paste0("/home/rstudio/pecan/modules/data.land/R/IGF.0.",i,".RData"))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols) < 1 ){
        new.out[[j]] <- new.out[[j]]
      }else{
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }}
    jags.comb <- new.out
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
saveRDS(jags.comb,file="IGF.PPT.noX2.all.trees.rds")

## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out, jags.stuff, file="PPT.noX2.all.trees.RData")

# check for convergence using gelman.diag
gelman.diag(jags.comb)



# check for convergence via traceplots
png(height = 12, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.noX2_all_trees_traceplots_effects.png")
par(mfrow=c(4,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSI"], main = expression(beta~SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_WateryearPPT), xlab = "last 2500 iterations")

#traceplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~DBH_tmax_fallspr), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~tmax_fallspr), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~tmax_fallspr), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~tmax_fallspr), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~tmax_fallspr), xlab = "last 2500 iterations")


traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_WateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betatmax.fallspr_wintP.wateryr"], main = expression(beta~tmax_fallspr), xlab = "last 2500 iterations")

dev.off()

# save autocorrelation plots
png(height = 10, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.WaterYear_noX2_all_trees_ACFplots_effects.png")

a <- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b <- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c <- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
#d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e <- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f <- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g <- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h <- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), aspect = 1)
i <- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j <- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k <- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l <- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
grid.arrange(a,b,c,e,f,g,h,i, j, k, l)
dev.off()

# plot the correlation between posterior plots + histograms



png(height = 10, width = 12, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/PPT.WaterYear_all_trees_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()


#####################################################################################
# 7. Plot Effects for Water Year Precip + Spring - Fall Tmax full model without X2 !
#####################################################################################
jags.comb <- NULL

for(i in 125:250){ # note this model stopped early b/c convergence
  load(paste0("/Users/kah/Documents/docker_pecan/pecan/IGF_PIPO_AZ_mcmc/PPT.TMAXfs.noX2.25000.",i,".RData" ))
  new.out <- jags.out 
  if(is.null(jags.comb)){
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
    }
    jags.comb <- new.out[,-x.cols]
    
  } else {
    for(j in seq_along(new.out)){
      x.cols <- grep("^x",colnames(new.out[[j]]))
      
      if(length(x.cols)>0){ 
        new.out[[j]] <- new.out[[j]][,-x.cols]
      }else{
        new.out[[j]] <- new.out[[j]]
      }
      
      jags.comb[[j]]  <- rbind(jags.comb[[j]], new.out[[j]])
      rm(jags.out)
    }
  }
}

for(i in 1:3){
  jags.comb[[i]] <- as.mcmc(jags.comb[[i]])
}
jags.comb <- as.mcmc.list(jags.comb)
save(jags.comb,file="IGF.waterYear.PPT.fall.spr.TMAX.noX2.RData")

# check for convergence via gelman-rubin
gelman.diag(jags.comb)
#summary(jags.comb)

# check for convergence via traceplots
#png(height = 12, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_Fall.SprTMAX_traceplots_effects.png")
png(height = 12, width = 8, units = "in", res = 200, "/Users/kah/Documents/docker_pecan/pecan/IGF_outputs/WateryearPPT_Fall.SprTMAX_traceplots_effects_noX2.png")

par(mfrow=c(6,3))
traceplot(jags.comb[, "deviance"], main = expression(deviance), xlab = "last 2500 iterations")
traceplot(jags.comb[, "mu"], main = expression(mu), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX"], main = expression(beta~DBH), xlab = "last 2500 iterations")
#traceplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSDI"], main = expression(beta~SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND"], main = expression(beta~SICOND), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SICOND), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), xlab = "last 2500 iterations")

traceplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")
traceplot(jags.comb[, "betatmax.fallspr_wintP.wateryr"], main = expression(beta~SI_TMAX.fs), xlab = "last 2500 iterations")

dev.off()

#png(height = 12, width = 8, units = "in", res = 200, "/home/rstudio/pecan/IGF_outputs/WateryearPPT_Fall.SprTMAX_ACFplots_effects_noX2.png")
png(height = 12, width = 8, units = "in", res = 200, "/Users/kah/Documents/docker_pecan/pecan/IGF_outputs/WtyrPPT_Fall.SprTMAX_ACFplots_effects_noX2.png")

a<- acfplot(jags.comb[, "deviance"], main = "deviance", aspect = 1)
b<- acfplot(jags.comb[, "mu"], main = expression(mu), aspect = 1)
c<- acfplot(jags.comb[, "betaX"], main = expression(beta~DBH), aspect = 1)
#d<- acfplot(jags.comb[, "betaX2"], main = expression(beta~DBH^2), aspect = 1)
e<- acfplot(jags.comb[, "betaSDI"], main = expression(beta~SDI),  aspect = 1)
f<- acfplot(jags.comb[, "betaX_SDI"], main = expression(beta~DBH_SDI), aspect = 1)
g<- acfplot(jags.comb[, "betaX_SICOND"], main = expression(beta~DBH_SI), aspect = 1)
h<- acfplot(jags.comb[, "betaX_wintP.wateryr"], main = expression(beta~DBH_wateryearPPT), aspect = 1)
i<- acfplot(jags.comb[, "betawintP.wateryr"], main = expression(beta~wateryearPPT), aspect = 1)
j<- acfplot(jags.comb[, "betaSDI_wintP.wateryr"], main = expression(beta~SDI_wateryearPPT), aspect = 1)
k<- acfplot(jags.comb[, "betaSICOND_wintP.wateryr"], main = expression(beta~SI_wateryearPPT), aspect = 1)
l<- acfplot(jags.comb[, "betaSICOND_SDI"], main = expression(beta~SI_SDI), aspect = 1)
m <- acfplot(jags.comb[, "betaSDI_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
n <- acfplot(jags.comb[, "betaSICOND_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
o <- acfplot(jags.comb[, "betaX_tmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
p <- acfplot(jags.comb[, "betatmax.fallspr"], main = expression(beta~SI_TMAX.fs), aspect = 1)
q <- acfplot(jags.comb[, "betatmax.fallspr_wintP.wateryr"], main = expression(beta~SI_TMAX.fs), aspect = 1)


grid.arrange(a,b,c,e,f,g,h,i,j,k,l, m,n,o,p, q)
dev.off()


png(height = 10, width = 12, units = "in", res = 200, "IGF_outputs/PPT.WaterYear_TMAX_fs_noX2_posterior_param_cors.png")
pairs.panels(as.matrix(jags.comb[, c("deviance", colnames(betas), "tau_dbh", "tau_inc", "tau_add")]), 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()


## standardized regression coef

out <- as.matrix(jags.comb)
summary(out)
betas <- out[,grep(pattern = "beta",colnames(out))]
B0 <- out[,"mu"]
summary(as.mcmc(betas))
apply(betas,2,summary)
apply(betas,2,function(x){sum(x>0)/length(x)})
save(out,  file="WaterYear.PPT.Fall.spr.TMAX.noX2.RData")



## calculate an average tree
hist(z0)
x <- mean(z0,na.rm = TRUE)
Xhl <- quantile(z0,c(1/6,5/6),na.rm=TRUE)
SDI <- mean(cov.data$SDI,na.rm = TRUE)
SDIhl <- quantile(cov.data$SDI,c(1/6,5/6),na.rm = TRUE)
wintP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wintPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)
SI <- mean(cov.data$SICOND, na.rm = TRUE)
SIhl <- quantile(cov.data$SICOND,c(1/6,5/6),na.rm = TRUE)

wateryrP <- mean(time_data$wintP.wateryr,na.rm = TRUE)
wateryrPhl <- quantile(time_data$wintP.wateryr,c(1/6,5/6),na.rm = TRUE)

sprfallTmax <- mean(time_data$tmax.fallspr,na.rm = TRUE)
sprfallTmaxhl <- quantile(time_data$tmax.fallspr,c(1/6,5/6),na.rm = TRUE)


# standardize the beta coefficients:

stdBeta <- rep(NA,17)
stdBeta[1] <- mean(betas[,"betaSDI"])/SDI
stdBeta[2] <- mean(betas[,"betaSDI_wintP.wateryr"])/SDI/wateryrP
stdBeta[5] <- mean(betas[,"betaSICOND_wintP.wateryr"])/SI/wateryrP
stdBeta[3] <- mean(betas[,"betaSICOND"])/SI
stdBeta[4] <- mean(betas[,"betaSICOND_SDI"])/SI/SDI
stdBeta[6] <- mean(betas[,"betaX"])/x
stdBeta[7] <- mean(betas[,"betaX2"])/(x^2)
stdBeta[8] <- mean(betas[,"betaX_SDI"])/x/SDI
stdBeta[9] <- mean(betas[,"betaX_SICOND"])/x/SI
stdBeta[10] <- mean(betas[,"betaX_wintP.wateryr"])/x/wateryrP
stdBeta[11] <- mean(betas[,"betawintP.wateryr"])/wateryrP
stdBeta[12] <- mean(betas[,"betaX_tmax.fallspr"])/x/sprfallTmax
stdBeta[12] <- mean(betas[,"betatmax.fallspr"])/sprfallTmax
stdBeta[13] <- mean(betas[,"betaSDI_tmax.fallspr"])/SDI/sprfallTmax
stdBeta[14] <- mean(betas[,"betaSICOND_tmax.fallspr"])/SI/sprfallTmax
stdBeta[15] <- mean(betas[,"betatmax.fallspr_wintP.wateryr"])/wateryrP/sprfallTmax

names(stdBeta) <- colnames(betas)
format(stdBeta*10^6,scientific = FALSE)
format(sort(abs(stdBeta*10^6),decreasing=TRUE),scientific = FALSE)


# create plots of conditional effects 

## Size
ns = 500 ## number of samples
i = sample.int(nrow(betas),ns)
xrng <- range(z0,na.rm = TRUE)
xseq <- seq(xrng[1],xrng[2],by=1)
xseq <- 1:58
incX <- matrix(NA,ns,length(xseq))

for(k in seq_along(i)){
  j <- i[k]
  incX[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wateryrP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wateryrP + betas[j,"betaX"]*xseq  +
    betas[j,"betaX_SDI"]*xseq*SDI + betas[j,"betaX_SICOND"]*xseq*SI + betas[j,"betaX_wintP.wateryr"]*xseq*wateryrP +
    betas[j,"betawintP.wateryr"]*wateryrP + 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*xseq*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
  
}


CIX <- apply(incX, 2, quantile,c(0.025,0.5,0.975)) # get CI on X

# plot as pseudo object to save for later
Tree.Size.Effect %<a-% {
  plot(xseq,CIX[2,],ylim=c(0,max(CIX)),type='n',ylab="Diameter Increment (cm)",xlab="DBH (cm)",cex.lab=1.5)
  ciEnvelope(xseq,CIX[1,],CIX[3,],col = "lightgrey")
  lines(xseq,CIX[2,],lwd=2)
  abline(h=0)
}


##SDI
hist(cov.data$SDI)

# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SDIseq.real <- seq(0,400,by=10)
SDIseq <- (SDIseq.real-mean(temp2$SDI))/sd(temp2$SDI)

incSDI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x +
    betas[j,"betaX_SDI"]*x*SDIseq + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
}
CIsdi <- apply(incSDI,2,quantile,c(0.025,0.5,0.975))

SDI.Effect %<a-% {
  plot(
    SDIseq.real,
    CIsdi[2, ],
    ylim = c(0, max(CIsdi)),
    type = 'n',
    xlab = "Stand Density Index",
    ylab = "Diameter Increment (cm)",
    cex.lab = 1.5)
  ciEnvelope(SDIseq.real, CIsdi[1, ], CIsdi[3, ], col = "lightgrey")
  lines(SDIseq.real, CIsdi[2, ], lwd = 2)
  abline(h = 0)
}

## SDI * size
incSDIXhi <- matrix(NA,ns,length(SDIseq))
incSDIXlo <- matrix(NA,ns,length(SDIseq))
for(k in seq_along(i)){
  j <- i[k]
  incSDIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax +betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
  incSDIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIseq*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDIseq + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
}
CIsdiXlo <- apply(incSDIXlo,2,quantile,c(0.025,0.5,0.975))
CIsdiXhi <- apply(incSDIXhi,2,quantile,c(0.025,0.5,0.975))

# SDI plot
SDI.DBH.Effect %<a-% {
  plot(SDIseq.real,CIsdi[2,],ylim=c(0,max(CIsdi[,1]+0.05)),type='l',xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5,lwd=3)
  #ciEnvelope(SDIseq,CIsdi[1,],CIsdi[3,],col = "lightgrey")
  lines(SDIseq.real,CIsdiXlo[2,],lwd=3,col="blue")
  lines(SDIseq.real,CIsdiXhi[2,],lwd=3,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
  abline(h=0)
}

##SI
hist(cov.data$SI)
# KH note: need to standardize SDIseq first since I ran the model with standardized covariates
SIseq.real <- 20:60
SIseq <- (SIseq.real-mean(temp2$COND_SICOND))/sd(temp2$COND_SICOND)
#SIseq <- 20:60
incSI <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSI[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
}
CIsi <- apply(incSI,2,quantile,c(0.025,0.5,0.975))

SI.Effect %<a-% {
  plot(SIseq.real,CIsi[2,],ylim=c(0,max(CIsi)),type='n',xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(SIseq.real,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsi[2,],lwd=2)
  abline(h=0)
}


## SI x DBH
incSIXlo <- matrix(NA,ns,length(SDIseq))
incSIXmed <- matrix(NA,ns,length(SDIseq))
incSIXhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSIXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
  incSIXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SIseq + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
  incSIXmed[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIseq + betas[j,"betaSICOND_SDI"]*SDI*SIseq + 
    betas[j,"betaSICOND_wintP.wateryr"]*SIseq*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIseq + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIseq*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wateryrP
  
}

CIsiXl <- apply(incSIXlo,2,quantile,c(0.025,0.5,0.975))
CIsiXh <- apply(incSIXhi,2,quantile,c(0.025,0.5,0.975))
CIsiXm <- apply(incSIXmed,2,quantile,c(0.025,0.5,0.975))

SI.DBH.Effect %<a-% {
  plot(SIseq.real,CIsiXm[2,],ylim=c(0,max(CIsi)),type='l',lwd=3,xlab="Site Index",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #ciEnvelope(SIseq,CIsi[1,],CIsi[3,],col = "lightgrey")
  lines(SIseq.real,CIsiXl[2,],lwd=3,col="blue")
  lines(SIseq.real,CIsiXh[2,],lwd=3,col="red")
  legend("bottomright",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3,cex=1)
}

## wintP
clim.data <- readRDS("FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$wintP.wateryr)
wintPseq.real <- 0:800
wintPseq <- (wintPseq.real-mean(clim.data$wintP.wateryr))/sd(clim.data$wintP.wateryr)
incP <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
  
}

CIwintP <- apply(incP,2,quantile,c(0.025,0.5,0.975))

Climate.Effect %<a-% {
  plot(wintPseq.real, CIwintP[2,],ylim=c(0,max(CIwintP + 0.05)),type='n',xlab="Water Year Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(wintPseq.real,CIwintP[1,], CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real, CIwintP[2,],lwd=2)
  abline(h=0)
}

## PRECIP x SDI
incPSDIlo <- matrix(NA,ns,length(wintPseq))
incPSDIhi <- matrix(NA,ns,length(wintPseq))
for(k in seq_along(i)){
  j <- i[k]
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[1]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
  
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl[2]*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
  
}

CIwintPSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SDI.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintPSDIlo[2,])),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSDIlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## PRECIP x DBH
incPXlo <- matrix(NA,ns,length(wintPseq))
incPXhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[1] +
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
  
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq
  betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
  
}

CIwintPXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIwintPXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

Climate.DBH.Effect %<a-% {
  plot(wintPseq.real,CIwintP[2,],ylim=c(0,max(CIwintP)),type='l',lwd=3,xlab="Winter Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPXlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIwintPXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Precip X SI
incP_SIlo <- matrix(NA,ns,length(wintPseq))
incP_SIhi <- matrix(NA,ns,length(wintPseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
  
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintPseq + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDI + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintPseq +
    betas[j,"betawintP.wateryr"]*wintPseq+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintPseq
  
}
CIwintPSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIwintPSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

Climate.SI.Effect %<a-% {
  plot(wintPseq.real,CIwintPSIhi[2,],ylim=c(0,max(CIwintPSIhi[2,])),type='l',lwd=3,xlab="Water year Precipitation (mm)",ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "lightgrey")
  lines(wintPseq.real,CIwintPSIlo[2,],lwd=2,col="grey")
  lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}



## SI X SDI

incSDI_SIlo <- matrix(NA,ns,length(SDIseq))
incSDI_SIhi <- matrix(NA,ns,length(SDIseq))

for(k in seq_along(i)){
  j <- i[k]
  incSDI_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[1] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP
  
  
  incSDI_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIseq + betas[j,"betaSDI_wintP.wateryr"]*SDIseq*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDIseq*SIhl[2] + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x[1] + 
    betas[j,"betaX_SDI"]*x[1]*SDIseq + betas[j,"betaX_SICOND"]*x[1]*SIhl[2] + betas[j,"betaX_wintP.wateryr"]*x[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP +
    betas[j,"betaSDI_tmax.fallspr"]*SDIseq*sprfallTmax+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmax+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmax +
    betas[j,"betatmax.fallspr"]*sprfallTmax + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmax*wintP
  
}
CISDISIlo <- apply(incSDI_SIlo,2,quantile,c(0.025,0.5,0.975))
CISDISIhi <- apply(incSDI_SIhi,2,quantile,c(0.025,0.5,0.975))

SDI.SI.Effect %<a-% {
  plot(SDIseq.real,CISDISIhi[2,],ylim=c(0,max(CISDISIhi[2,])),type='l',lwd=3,xlab="Stand Density Index",ylab="Diameter Increment (cm)",cex.lab=1.5, col = "purple")
  # #PEcAn.visualization::ciEnvelope(wintPseq,CIwintP[1,],CIwintP[3,],col = "purple")
  lines(SDIseq.real,CISDISIlo[2,],lwd=2,col="grey")
  #lines(wintPseq.real,CIwintPSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}

# also plot all temperature effects and interactions:

## sprfallTmax
clim.data <- readRDS("FIA_inc_data/PRISM_non_scaled.rds")
hist(time_data$tmax.fallspr)
sprfallTmaxseq.real <- 15:30
sprfallTmaxseq <- (sprfallTmaxseq.real-mean(clim.data$tmax.fallspr))/sd(clim.data$tmax.fallspr)
incP <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxseq*wintP
}

CIsprfallTmax <- apply(incP,2,quantile,c(0.025,0.5,0.975))

TMAX.Effect %<a-% {
  plot(sprfallTmaxseq.real, CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax + 0.05)),type='n',xlab=expression("Spring & Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  ciEnvelope(sprfallTmaxseq.real,CIsprfallTmax[1,], CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real, CIsprfallTmax[2,],lwd=2)
  abline(h=0)
}

## TMAX x SDI
incPSDIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPSDIhi <- matrix(NA,ns,length(sprfallTmaxseq))
for(k in seq_along(i)){
  j <- i[k]
  
  incPSDIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[1] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[1]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[1] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[1]*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxseq*wintP
  
  incPSDIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDIhl[2] + betas[j,"betaSDI_wintP.wateryr"]*SDIhl*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDIhl[2]*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDIhl[2] + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDIhl[2]*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxseq*wintP
}


CIsprfallTmaxSDIlo <- apply(incPSDIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSDIhi <- apply(incPSDIhi,2,quantile,c(0.025,0.5,0.975))

TMAX.SDI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmaxSDIlo[2,])),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)), ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSDIhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low SDI","mean SDI","high SDI"),col=c("blue",1,2),lwd=3)
}

## Tmax x DBH
incPXlo <- matrix(NA,ns,length(sprfallTmaxseq))
incPXhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incPXlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[1] + 
    betas[j,"betaX_SDI"]*Xhl[1]*SDI + betas[j,"betaX_SICOND"]*Xhl[1]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[1]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[1]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxseq*wintP
  
  incPXhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI + 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*Xhl[2] + 
    betas[j,"betaX_SDI"]*Xhl[2]*SDI + betas[j,"betaX_SICOND"]*Xhl[2]*SI + betas[j,"betaX_wintP.wateryr"]*Xhl[2]*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*Xhl[2]*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxseq*wintP
  
}

CIsprfallTmaxXlo <- apply(incPXlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxXhi <- apply(incPXhi,2,quantile,c(0.025,0.5,0.975))

TMAX.DBH.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmax[2,],ylim=c(0,max(CIsprfallTmax)),type='l',lwd=3,xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXlo[2,],lwd=2,col="blue")
  lines(sprfallTmaxseq.real,CIsprfallTmaxXhi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("small","mean","large"),col=c("blue",1,2),lwd=3)
}

## Tmax X SI
incP_SIlo <- matrix(NA,ns,length(sprfallTmaxseq))
incP_SIhi <- matrix(NA,ns,length(sprfallTmaxseq))

for(k in seq_along(i)){
  j <- i[k]
  incP_SIlo[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIhl[1] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1]+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SIhl[1]*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[1]*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxseq*wintP
  
  incP_SIhi[k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintP + 
    betas[j,"betaSICOND"]*SIhl[2] + betas[j,"betaSICOND_SDI"]*SDI*SIhl[1]+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SIhl[2]*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SIhl[1] + betas[j,"betaX_wintP.wateryr"]*x*wintP +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*sprfallTmaxseq+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SIhl[2]*sprfallTmaxseq+ 
    betas[j,"betaX_tmax.fallspr"]*x*sprfallTmaxseq +
    betas[j,"betatmax.fallspr"]*sprfallTmaxseq + betas[j,"betatmax.fallspr_wintP.wateryr"]*sprfallTmaxseq*wintP
  
}

CIsprfallTmaxSIlo <- apply(incP_SIlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxSIhi <- apply(incP_SIhi,2,quantile,c(0.025,0.5,0.975))

TMAX.SI.Effect %<a-% {
  plot(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],ylim=c(0,max(CIsprfallTmaxSIlo[2,])),type='l',lwd=3, xlab=expression("Spring-Fall Tmax "*(degree~C)),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIlo[2,],lwd=2,col="grey")
  lines(sprfallTmaxseq.real,CIsprfallTmaxSIhi[2,],lwd=2,col="purple")
  legend("bottomleft",legend=c("low SI","high SI"),col=c("grey", "purple"),lwd=3)
}


# finally: Interaction between water year PPT and Tmax

## Tmax X PPT
incP_Tmaxlo <- matrix(NA,ns,length(wintPseq))
incP_Tmaxhi <- matrix(NA,ns,length(wintPseq))

Tmaxhl <- quantile(sprfallTmaxseq,c(1/6,5/6),na.rm = TRUE)
Tmaxhl.real <- quantile(sprfallTmaxseq.real,c(1/6,5/6),na.rm = TRUE)

for(k in seq_along(i)){
  j <- i[k]
  incP_Tmaxlo [k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq  +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*Tmaxhl[1]+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*Tmaxhl[1]+ 
    betas[j,"betaX_tmax.fallspr"]*x*Tmaxhl[1] +
    betas[j,"betatmax.fallspr"]*Tmaxhl[1] + betas[j,"betatmax.fallspr_wintP.wateryr"]*Tmaxhl[1]*wintPseq 
  
  incP_Tmaxhi [k,] <- B0[j] + betas[j,"betaSDI"]*SDI + betas[j,"betaSDI_wintP.wateryr"]*SDI*wintPseq + 
    betas[j,"betaSICOND"]*SI + betas[j,"betaSICOND_SDI"]*SDI*SI+ 
    betas[j,"betaSICOND_wintP.wateryr"]*SI*wintP + betas[j,"betaX"]*x + 
    betas[j,"betaX_SDI"]*x*SDI + betas[j,"betaX_SICOND"]*x*SI + betas[j,"betaX_wintP.wateryr"]*x*wintPseq  +
    betas[j,"betawintP.wateryr"]*wintP+ 
    betas[j,"betaSDI_tmax.fallspr"]*SDI*Tmaxhl[2]+ 
    betas[j,"betaSICOND_tmax.fallspr"]*SI*Tmaxhl[2]+ 
    betas[j,"betaX_tmax.fallspr"]*x*Tmaxhl[2] +
    betas[j,"betatmax.fallspr"]*Tmaxhl[2] + betas[j,"betatmax.fallspr_wintP.wateryr"]*Tmaxhl[2]*wintPseq 
  
}
CIsprfallTmaxPPTlo <- apply(incP_Tmaxlo,2,quantile,c(0.025,0.5,0.975))
CIsprfallTmaxPPThi <- apply( incP_Tmaxhi,2,quantile,c(0.025,0.5,0.975))

TMAX.PPT.Effect %<a-% {
  plot(wintPseq.real,CIsprfallTmaxPPThi[2,],ylim=c(0,0.5),type='l',lwd=3, xlab=expression("Water Year Precipitation  (mm)"),ylab="Diameter Increment (cm)",cex.lab=1.5)
  #PEcAn.visualization::ciEnvelope(sprfallTmaxseq,CIsprfallTmax[1,],CIsprfallTmax[3,],col = "lightgrey")
  lines(wintPseq.real,CIsprfallTmaxPPTlo[2,],lwd=2,col="blue")
  lines(wintPseq.real,CIsprfallTmaxPPThi[2,],lwd=2,col="red")
  legend("bottomleft",legend=c("low Temp (17.5 degC)","high Temp (27.5 degC)"),col=c("blue", "red"),lwd=3)
}


plot.effects.pryr %<a-% {
  split.screen(c(3, 2))
  # plot effects
  screen(1)
  SI.Effect
  
  screen(2)
  SDI.Effect
  screen(3)
  Tree.Size.Effect
  screen(4)
  Climate.Effect
  screen(5)
  TMAX.Effect
  close.screen(all=TRUE)
}

png(height = 12, width = 6.5, units = "in", res = 200, "IGF_outputs/Full_effects_WaterYrP_TmaxFallSpr.png")
plot.effects.pryr
dev.off()



# plot interactions
plot.interaction.effects.pryr %<a-%{
  split.screen(c(4, 3))
  screen(1)
  SI.DBH.Effect
  screen(2)
  SDI.DBH.Effect
  screen(3)
  Climate.DBH.Effect
  
  screen(4)
  Climate.SI.Effect
  screen(5)
  Climate.SDI.Effect
  screen(6)
  SDI.SI.Effect
  
  screen(7)
  TMAX.DBH.Effect
  
  screen(8)
  TMAX.SI.Effect
  screen(9)
  TMAX.SDI.Effect
  screen(10)
  TMAX.PPT.Effect
  close.screen(all=TRUE)
}

png(height = 14, width = 10, units = "in", res = 200, "IGF_outputs/Full_interaction_effects_WaterYrP_TmaxFallSpr.png")
plot.interaction.effects.pryr
dev.off()




