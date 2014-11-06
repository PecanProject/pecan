####################################################################################################
# 
#   MICMoR RTM Lab
#   KIT/IMK-IFU, July 2014
#
#
#   PROSPECT Info:
#   - Jacquemoud, S., & Baret, F. (1990). PROSPECT - A model of leaf optical-properties of spectra. 
#     Remote Sensing of Environment, 34, 75-91
#   - Jacquemoud, S., Ustin, S.L., Verdebout, J., Schmuck, G., Andreoli, G., & Hosgood, B. (1996). 
#     Estimating leaf biochemistry using the PROSPECT leaf optical properties model. Remote Sensing 
#     of Environment, 56, 194-202
#   - Feret, J.B., Francois, C., Asner, G.P., Gitelson, A.A., Martin, R.E., Bidel, L.P.R., 
#     Ustin, S.L., le Maire, G., & Jacquemoud, S. (2008). PROSPECT-4 and 5: Advances in the leaf 
#     optical properties model separating photosynthetic pigments. Remote Sensing of Environment, 
#     112, 3030-3043
#   - Feret, J.-B., Francois, C., Gitelson, A., Asner, G.P., Barry, K.M., Panigada, C., Richardson, 
#     A.D., & Jacquemoud, S. (2011). Optimizing spectral indices and chemometric analysis of leaf 
#     chemical properties using radiative transfer modeling. Remote Sensing of Environment, 115, 
#     2742-2750
#
#
#
#    --- Last updated:  07.24.2014 By Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files

# Install required package. Use a personal lib (yes)
#install.packages('lhs')  

# Load required libraries
library(lhs)

# Load RMSE function. For inversion
rmse <- function(meas.refl,mod.refl,meas.tran,mod.tran) {
  refl.err <- rowMeans( (sweep(mod.refl, 2, meas.refl, FUN = "-"))^2,na.rm=T ) 
  tran.err <- rowMeans( (sweep(mod.tran, 2, meas.tran, FUN = "-"))^2,na.rm=T ) 
  rmse.out <- sqrt(refl.err+tran.err)
  invisible(rmse.out)
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Link to PROSPECT compiled binaries.
# If downloaded to PROSPECT-4 / PROSPECT-5B
# E.g. PROSPECT4 <- 'C:\PROSPECT-4\./PROSPECT_4'
# E.g. PROSPECT5 <- 'C:\PROSPECT-5B\./PROSPECT_5B'
PROSPECT4 <- '/Users/shawnserbin/Data/Dropbox/Courses/FWE-875_2013/Labs/RTMs/PROSPECT-4/./PROSPECT_4'
PROSPECT5B <- '/Users/shawnserbin/Data/Dropbox/Courses/FWE-875_2013/Labs/RTMs/PROSPECT-5B/./PROSPECT_5B'

# E.g. Windows computers
#PROSPECT4 <- 'C:/Users/labuser/Downloads/PROSPECT_4.exe'
#PROSPECT5B <- 'C:/Users/labuser/Downloads/PROSPECT_5B.exe'
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Set Working Directory. Where to place output from PROSPECT.
# E.g. To put output in PROSPECT_Lab directory
# setwd('C:\Data\PROSPECT_Lab\')
setwd('~/Data/Dropbox/Meetings/MICMoR/RTM_Lab/PROSPECT_Lab/')

# E.g. Lab Computers
#setwd('C:/Users/labuser/Downloads/')
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
# =================================================================================================#
### Run PROSPECT models in forward mode
# =================================================================================================#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Run PROSPECT-4
N <- 2            #!! N Parameter (mesophyll thickness)
Cab <- 45         #!! Chlorophyll a+b concetration
Cw <- 0.015       #!! Water content
Cm <- 0.009       #!! Dry matter content (LMA)

# Run PROSPECT
# See ?system2
system2(PROSPECT4,args=c(N, Cab, Cw, Cm),wait=T)
# Get Output
# See ?read.table
LRT <- read.table(file='PROSPECT_4_leaf_spectrum.txt')

# Plot Results
par(mar=c(5,5,1,5)) #b, l, t, r
plot(LRT[,1],LRT[,2],type="l",lwd=5,ylim=c(0,1),xlab="Wavelength (nm)",ylab="Reflectance (%)",
     cex.axis=1.5, cex.lab=1.7)
par(new=TRUE)
lines(LRT[,1],1-LRT[,3],lwd=5,col="grey")
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)
box(lwd=2.2)
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Run PROSPECT-5B
N <- 2            #!! N Parameter (mesophyll thickness)
Cab <- 45         #!! Chlorophyll a+b concetration
Car <- 10         #!! Carotenoid concetration
Cbrown <- 0.0     #!! Brown pigment concetration
Cw <- 0.015       #!! Water content
Cm <- 0.009       #!! Dry matter content (LMA)

# Run PROSPECT
system2(PROSPECT5B,args=c(N, Cab, Car, Cbrown, Cw, Cm),wait=T)
# Get Output
LRT <- read.table(file='PROSPECT_5B_leaf_spectrum.txt')

# Plot Results
par(mar=c(5,5,1,5)) #b, l, t, r
plot(LRT[,1],LRT[,2],type="l",lwd=5,ylim=c(0,1),xlab="Wavelength (nm)",ylab="Reflectance (%)",
     cex.axis=1.5, cex.lab=1.7)
par(new=TRUE)
lines(LRT[,1],1-LRT[,3],lwd=5,col="grey")
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)
box(lwd=2.2)
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
# =================================================================================================#
### Run PROSPECT models over multiple parameter combinations
# =================================================================================================#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Run PROSPECT-4 over all possible combinations.
#!! See Feret et al., (2011)
N <- seq(1.0,3,0.5)                 #!! N Parameter (mesophyll thickness)
Cab <- seq(20,100,10)               #!! Chlorophyll a+b concetration
Cw <- seq(0.004,0.024,0.004)        #!! Water content
Cm <- seq(0.002,0.014,0.002)        #!! Dry matter content (LMA)

# Generate all combinations of input parameters
# See ?expand.grid
all.combs <- expand.grid(N,Cab,Cw,Cm)
names(all.combs) <- c("N","Cab","Cw","Cm")

# Build empty output array for PROSPECT run
p.output.refl <- array(data=NA,dim=c(dim(all.combs)[1], 2101))
p.output.tran <- array(data=NA,dim=c(dim(all.combs)[1], 2101))

# Set progress bar
# ?see txtProgressBar
pb <- txtProgressBar(min = 0, max = dim(all.combs)[1], width= 50,style = 3)

# Run PROSPECT iterations
system.time(for (i in 1:dim(all.combs)[1] ) {
  #for (i in 1:5 ) {
  system2(PROSPECT4,args=c(all.combs[i,1], all.combs[i,2], all.combs[i,3], all.combs[i,4]),wait=T)
  LRT <- read.table(file='PROSPECT_4_leaf_spectrum.txt')
  p.output.refl[i,] <- LRT[,2]
  p.output.tran[i,] <- LRT[,3]
  setTxtProgressBar(pb, i)
  # Remove temp objects/files
  rm(LRT)
  system2('rm',args=c("PROSPECT_4_leaf_spectrum.txt"))
})

### Plot results
waves <- seq(400,2500,1)
# Calculate spectra stats
mean.refl <- colMeans(p.output.refl,na.rm=T)
refl.spec.quant <- apply(p.output.refl,2,quantile,na.rm=T,probs=c(0,0.05,0.95,1))
mean.tran <- colMeans(p.output.tran,na.rm=T)
tran.spec.quant <- apply(p.output.tran,2,quantile,na.rm=T,probs=c(0,0.05,0.95,1))

# Create plot of simulated spectra
# See ?par ?mar
par(mfrow=c(2,1),mar=c(2,5,0.3,0.5)) #B,L,T,R
y.max <- max(refl.spec.quant[4,])
plot(waves,mean.refl,xlim=c(400,2500),ylim=c(0.01,y.max+0.1),cex=0.00001, col="white",xlab="",ylab="Reflectance (%)",
     cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(refl.spec.quant[4,], rev(refl.spec.quant[1,])),col="#99CC99",border=NA)
lines(waves,mean.refl,lwd=4)
lines(waves,refl.spec.quant[2,],lty=5,lwd=3)
lines(waves,refl.spec.quant[3,],lty=5,lwd=3)

# Transmittance
y.max <- max(tran.spec.quant[4,])
plot(waves,mean.tran,xlim=c(400,2500),ylim=c(0.01,y.max+0.1),cex=0.00001, col="white",xlab="",ylab="Transmittance (%)",
     cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(tran.spec.quant[4,], rev(tran.spec.quant[1,])),col="#99CC99",border=NA)
lines(waves,mean.tran,lwd=4)
lines(waves,tran.spec.quant[2,],lty=5,lwd=3)
lines(waves,tran.spec.quant[3,],lty=5,lwd=3)
box(lwd=2.2)

# Turn off graphic device
# See ?dev.off
dev.off()
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Calculate a few spectral vegetation indices (SVIs) across simulated spectral dataset
wave.names <- paste0("Wave_",seq(400,2500,1))
reflectance <- data.frame(p.output.refl)
names(reflectance) <- wave.names

dev.off()

# Calculate NDVI
NDVI <- (reflectance$Wave_850-reflectance$Wave_650)/(reflectance$Wave_850+reflectance$Wave_650)
hist(NDVI)

# Calculate PRI
PRI <- (reflectance$Wave_531-reflectance$Wave_570)/(reflectance$Wave_531+reflectance$Wave_570)
hist(PRI)

# Scaled PRI
sPRI <- (PRI+1)/2
hist(sPRI)

# Relationship between leaf chlorophyll and NDVI
plot(all.combs$Cab,NDVI,xlab="Cab (ug)",ylab="NDVI (-)",pch=21,col="black",bg="grey",cex=1)
box(lwd=2.2)

# Relationship between 970 nm and N
plot(all.combs$N,reflectance$Wave_970,xlab="N",ylab="970nm Reflectance (%)",pch=21,col="black",bg="grey",cex=1)
box(lwd=2.2)

# Relationship between leaf chlorophyll and sPRI
plot(all.combs$Cab,sPRI,xlab="Wavelength (nm)",ylab="sPRI (-)",pch=21,col="black",bg="grey",cex=1)
box(lwd=2.2)

#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
# =================================================================================================#
### Invert PROSPECT-4 model
# =================================================================================================#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Run PROSPECT-4
N <- 2            #!! N Parameter (mesophyll thickness)
Cab <- 50         #!! Chlorophyll a+b concetration
Cw <- 0.020       #!! Water content
Cm <- 0.01        #!! Dry matter content (LMA)

# Run PROSPECT
# See ?system2
system2(PROSPECT4,args=c(N, Cab, Cw, Cm),wait=T)
# Get Output
# See ?read.table
LRT <- read.table(file='PROSPECT_4_leaf_spectrum.txt')

### Invert PROSPECT

# Get RMSE values.
RMSE <- rmse(LRT[,2],p.output.refl,LRT[,3],p.output.tran)

# Find minimum RMSE between meas and modeled data
min.RMSE <- which.min(RMSE)
prospect.params <- all.combs[min.RMSE,]
prospect.params       #!! Should match exactly

### Plot results
waves <- seq(400,2500,1)
par(mar=c(5,5,1,5)) #b, l, t, r
plot(waves,LRT[,2],type="l",lwd=2.5,ylim=c(0,1),xlab="Wavelength (nm)",
     ylab="Reflectance (%)",cex.axis=1.5, cex.lab=1.7)
lines(waves,1-LRT[,3],lwd=2.5,col="dark grey")
lines(waves,p.output.refl[min.RMSE,],type="l",lty=2,lwd=1.8)
par(new=TRUE)
lines(waves,1-p.output.tran[min.RMSE,],type="l",
      lty=2,lwd=1.8,col="dark grey",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)
legend("right",legend=c("Observed","Modeled"),lty=c(1,2),lwd=2,bty="n",cex=1.5)
box(lwd=2.2)
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
# =================================================================================================#
### Run PROSPECT-4 model over a Latin Hypercube sampling strategy
# =================================================================================================#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Run PROSPECT-4 over latin hypercube

## Setup Hypercube
# See ?require
require(lhs)
# See ?randomLHS
N <- 2000                                       #!! number of LHS partitions (iterations)
LHS <- randomLHS(N, k=4)                        #!! Random LHS sampling over 4 parameters
param.combs <- LHS

#!! See Feret et al., (2011)
param.combs[,1] <- qunif(LHS[,1], 1.0, 2.3)     #!! N Parameter (mesophyll thickness)
param.combs[,2] <- qunif(LHS[,2], 10, 110)      #!! Chlorophyll a+b concetration (Cab)
param.combs[,3] <- qunif(LHS[,3], 0.004, 0.024) #!! Water content (Cw)
param.combs[,4] <- qunif(LHS[,4], 0.002, 0.014) #!! Dry matter content (LMA, Cm)
param.combs <- data.frame(param.combs)
names(param.combs) <- c("N","Cab","Cw","Cm")
rm(LHS,N)

# Build empty output array for PROSPECT run
p.output.refl <- array(data=NA,dim=c(dim(param.combs)[1], 2101))
p.output.tran <- array(data=NA,dim=c(dim(param.combs)[1], 2101))

# Set progress bar
# ?see txtProgressBar
pb <- txtProgressBar(min = 0, max = dim(param.combs)[1], width= 50,style = 3)

# Run PROSPECT iterations
system.time(for (i in 1:dim(param.combs)[1] ) {
  #for (i in 1:5 ) {
  system2(PROSPECT4,args=c(param.combs[i,1], param.combs[i,2], param.combs[i,3], param.combs[i,4]),wait=T)
  LRT <- read.table(file='PROSPECT_4_leaf_spectrum.txt')
  p.output.refl[i,] <- LRT[,2]
  p.output.tran[i,] <- LRT[,3]
  setTxtProgressBar(pb, i)
  # Remove temp objects/files
  rm(LRT)
  system2('rm',args=c("PROSPECT_4_leaf_spectrum.txt"))
})

### Plot results
waves <- seq(400,2500,1)
# Calculate spectra stats
mean.refl <- colMeans(p.output.refl,na.rm=T)
refl.spec.quant <- apply(p.output.refl,2,quantile,na.rm=T,probs=c(0,0.05,0.95,1))
mean.tran <- colMeans(p.output.tran,na.rm=T)
tran.spec.quant <- apply(p.output.tran,2,quantile,na.rm=T,probs=c(0,0.05,0.95,1))

# Create plot of simulated spectra
# See ?par ?mar
par(mfrow=c(2,1),mar=c(2,5,0.3,0.5)) #B,L,T,R
y.max <- max(refl.spec.quant[4,])
plot(waves,mean.refl,xlim=c(400,2500),ylim=c(0.01,y.max+0.1),cex=0.00001, col="white",xlab="",ylab="Reflectance (%)",
     cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(refl.spec.quant[4,], rev(refl.spec.quant[1,])),col="#99CC99",border=NA)
lines(waves,mean.refl,lwd=4)
lines(waves,refl.spec.quant[2,],lty=5,lwd=3)
lines(waves,refl.spec.quant[3,],lty=5,lwd=3)

# Transmittance
y.max <- max(tran.spec.quant[4,])
plot(waves,mean.tran,xlim=c(400,2500),ylim=c(0.01,y.max+0.1),cex=0.00001, col="white",xlab="",ylab="Transmittance (%)",
     cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(tran.spec.quant[4,], rev(tran.spec.quant[1,])),col="#99CC99",border=NA)
lines(waves,mean.tran,lwd=4)
lines(waves,tran.spec.quant[2,],lty=5,lwd=3)
lines(waves,tran.spec.quant[3,],lty=5,lwd=3)
box(lwd=2.2)

# Turn off graphic device
# See ?dev.off
dev.off()
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
# =================================================================================================#
### Invert PROSPECT-4 model on real data using Latin Hypercube sampling strategy
# =================================================================================================#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Read in real data. Update this for local settings
data.dir <- '~/Data/Dropbox/Meetings/MICMoR/RTM_Lab/PROSPECT_Lab/Data/'

# Availible datasets:
# acsa3_bottom_leaf.csv; acsa3_top_leaf.csv; clover_top_leaf.csv; corn_top_leaf.csv; 
# fagr_top_leaf.csv; potr_top_leaf.csv; quru_middle_leaf.csv
dataset <- 'acsa3_top_leaf.csv'
# See ?read.csv; ?paste0
meas.data <- read.csv(paste0(data.dir,dataset),header=T)

### Invert PROSPECT using LHS LUT
# Get RMSE values.
RMSE <- rmse(meas.data[,2],p.output.refl,meas.data[,3],p.output.tran)

# Find minimum RMSE between meas and modeled data
min(RMSE)
min.RMSE <- which.min(RMSE)
prospect.params <- param.combs[min.RMSE,]
prospect.params

### Plot results
dev.off()
waves <- seq(400,2500,1)
par(mar=c(5,5,1,5)) #b, l, t, r
plot(waves,meas.data[,2],type="l",lwd=2.5,ylim=c(0,1),xlab="Wavelength (nm)",
     ylab="Reflectance (%)",cex.axis=1.5, cex.lab=1.7)
lines(waves,1-meas.data[,3],lwd=2.5,col="dark grey")
lines(waves,p.output.refl[min.RMSE,],type="l",lty=2,lwd=1.8)
par(new=TRUE)
lines(waves,1-p.output.tran[min.RMSE,],type="l",
      lty=2,lwd=1.8,col="dark grey",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)
legend("right",legend=c("Observed","Modeled"),lty=c(1,2),lwd=2,bty="n",cex=1.5)
box(lwd=2.2)


##### Invert using a population of best fits. Top ten fits (i.e. lowest 10 RMSE parameter combinations)
# How many solutions to keep?
solutions <- 10

# Get info
RMSE.sort <- sort.int(RMSE,index.return=T)
best.param.combs <- data.frame(RMSE=RMSE.sort$x[1:solutions],param.combs[RMSE.sort$ix[1:solutions],])
best.param.combs

# Parameter means
colMeans(best.param.combs) 

# Get modeled spec stats
mod.refl <- p.output.refl[RMSE.sort$ix[1:solutions],]
mod.trans <- p.output.tran[RMSE.sort$ix[1:solutions],]
mod.refl.minmax <- apply(mod.refl,2,quantile,na.rm=T,probs=c(0,1))
mod.trans.minmax <- apply(mod.trans,2,quantile,na.rm=T,probs=c(0,1))

### Plot results
dev.off()
waves <- seq(400,2500,1)
par(mar=c(5,5,1,5)) #b, l, t, r
plot(waves,meas.data[,2],type="l",lwd=2.5,ylim=c(0,1),xlab="Wavelength (nm)",
     ylab="Reflectance (%)",cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(mod.refl.minmax[2,], rev(mod.refl.minmax[1,])),col="#99CC99",border=NA)
polygon(c(waves ,rev(waves)),c(1-mod.trans.minmax[2,], rev(1-mod.trans.minmax[1,])),col="#99CC99",border=NA)
lines(waves,meas.data[,2],lwd=2.5,col="black")
lines(waves,1-meas.data[,3],lwd=2.5,col="dark grey")
par(new=TRUE)
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)
legend("right",legend=c("Observed","Modeled"),lty=c(1,2),lwd=2,bty="n",cex=1.5)
box(lwd=2.2)
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
# =================================================================================================#
### Run PROSPECT-5B model over a Latin Hypercube sampling strategy
# =================================================================================================#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Run PROSPECT-5B over latin hypercube

## Setup Hypercube
# See ?require
require(lhs)
# See ?randomLHS
N <- 2000                                       #!! number of LHS partitions (iterations)
LHS <- randomLHS(N, k=6)                        #!! Random LHS sampling over 5 parameters
param.combs <- LHS

#!! See Feret et al., (2011). NOT USING BROWN PIGMENT PARAMETER
param.combs[,1] <- qunif(LHS[,1], 1.0, 2.3)     #!! N Parameter (mesophyll thickness)
param.combs[,2] <- qunif(LHS[,2], 10, 110)      #!! Chlorophyll a+b concetration (Cab)
param.combs[,3] <- qunif(LHS[,3], 2.5, 25)      #!! Carotenoid concetration (Car)
param.combs[,4] <- qunif(LHS[,4], 0, 0)      #!! Brown pigment concetration (Cbrown)
param.combs[,5] <- qunif(LHS[,5], 0.004, 0.024) #!! Water content (Cw)
param.combs[,6] <- qunif(LHS[,6], 0.002, 0.014) #!! Dry matter content (LMA, Cm)
param.combs <- data.frame(param.combs)
names(param.combs) <- c("N","Cab","Car","Cbrown","Cw","Cm")
rm(LHS,N)

# Build empty output array for PROSPECT run
# See ?array
p.output.refl <- array(data=NA,dim=c(dim(param.combs)[1], 2101))
p.output.tran <- array(data=NA,dim=c(dim(param.combs)[1], 2101))

# Set progress bar
# ?see txtProgressBar
pb <- txtProgressBar(min = 0, max = dim(param.combs)[1], width= 50,style = 3)

# Run PROSPECT iterations
system.time(for (i in 1:dim(param.combs)[1] ) {
  #for (i in 1:5 ) {
  # PROSPECT5B N Cab Car Cbrown Cw Cm
  system2(PROSPECT5B,args=c(param.combs[i,1], param.combs[i,2], param.combs[i,3], param.combs[i,4],
                            param.combs[i,5],param.combs[i,6]),wait=T)
  LRT <- read.table(file='PROSPECT_5B_leaf_spectrum.txt')
  p.output.refl[i,] <- LRT[,2]
  p.output.tran[i,] <- LRT[,3]
  setTxtProgressBar(pb, i)
  # Remove temp objects/files
  rm(LRT)
  system2('rm',args=c("PROSPECT_5B_leaf_spectrum.txt"))
})

### Plot results
waves <- seq(400,2500,1)
# Calculate spectra stats
mean.refl <- colMeans(p.output.refl,na.rm=T)
refl.spec.quant <- apply(p.output.refl,2,quantile,na.rm=T,probs=c(0,0.05,0.95,1))
mean.tran <- colMeans(p.output.tran,na.rm=T)
tran.spec.quant <- apply(p.output.tran,2,quantile,na.rm=T,probs=c(0,0.05,0.95,1))

# Create plot of simulated spectra
# See ?par ?mar
par(mfrow=c(2,1),mar=c(2,5,0.3,0.5)) #B,L,T,R
y.max <- max(refl.spec.quant[4,])
plot(waves,mean.refl,xlim=c(400,2500),ylim=c(0.01,y.max+0.1),cex=0.00001, col="white",xlab="",ylab="Reflectance (%)",
     cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(refl.spec.quant[4,], rev(refl.spec.quant[1,])),col="#99CC99",border=NA)
lines(waves,mean.refl,lwd=4)
lines(waves,refl.spec.quant[2,],lty=5,lwd=3)
lines(waves,refl.spec.quant[3,],lty=5,lwd=3)

# Transmittance
y.max <- max(tran.spec.quant[4,])
plot(waves,mean.tran,xlim=c(400,2500),ylim=c(0.01,y.max+0.1),cex=0.00001, col="white",xlab="",ylab="Transmittance (%)",
     cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(tran.spec.quant[4,], rev(tran.spec.quant[1,])),col="#99CC99",border=NA)
lines(waves,mean.tran,lwd=4)
lines(waves,tran.spec.quant[2,],lty=5,lwd=3)
lines(waves,tran.spec.quant[3,],lty=5,lwd=3)
box(lwd=2.2)

# Turn off graphic device
# See ?dev.off
dev.off()
#--------------------------------------------------------------------------------------------------#
#!!!
#!!!
# =================================================================================================#
### Invert PROSPECT-5B model on real data using Latin Hypercube sampling strategy
# =================================================================================================#
#!!!
#!!!
#--------------------------------------------------------------------------------------------------#
### Read in real data. Update this for local settings
data.dir <- '~/Data/Dropbox/Meetings/MICMoR/RTM_Lab/PROSPECT_Lab/Data/'

# Availible datasets:
# acsa3_bottom_leaf.csv; acsa3_top_leaf.csv; clover_top_leaf.csv; corn_top_leaf.csv; 
# fagr_top_leaf.csv; potr_top_leaf.csv; quru_middle_leaf.csv
dataset <- 'acsa3_top_leaf.csv'
# See ?read.csv; ?paste0
meas.data <- read.csv(paste0(data.dir,dataset),header=T)

# Plot dataset
dev.off()
par(mar=c(5,5,1,5)) #b, l, t, r
plot(meas.data[,1],meas.data[,2],type="l",lwd=5,ylim=c(0,1),xlab="Wavelength (nm)",ylab="Reflectance (%)",
     cex.axis=1.5, cex.lab=1.7)
par(new=TRUE)
lines(meas.data[,1],1-meas.data[,3],lwd=5,col="grey")
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)

### Invert PROSPECT using LHS LUT
# Get RMSE values.
RMSE <- rmse(meas.data[,2],p.output.refl,meas.data[,3],p.output.tran)

# Find minimum RMSE between meas and modeled data
min(RMSE)
min.RMSE <- which.min(RMSE)
prospect.params <- param.combs[min.RMSE,]
prospect.params

### Plot results
dev.off()
waves <- seq(400,2500,1)
par(mar=c(5,5,1,5)) #b, l, t, r
plot(waves,meas.data[,2],type="l",lwd=2.5,ylim=c(0,1),xlab="Wavelength (nm)",
     ylab="Reflectance (%)",cex.axis=1.5, cex.lab=1.7)
lines(waves,1-meas.data[,3],lwd=2.5,col="dark grey")
lines(waves,p.output.refl[min.RMSE,],type="l",lty=2,lwd=1.8)
par(new=TRUE)
lines(waves,1-p.output.tran[min.RMSE,],type="l",
      lty=2,lwd=1.8,col="dark grey",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)
legend("right",legend=c("Observed","Modeled"),lty=c(1,2),lwd=2,bty="n",cex=1.5)
box(lwd=2.2)

##### Invert using a population of best fits. Top ten fits (i.e. lowest 10 RMSE parameter combinations)
# How many solutions to keep?
solutions <- 10

# Get info
RMSE.sort <- sort.int(RMSE,index.return=T)
best.param.combs <- data.frame(RMSE=RMSE.sort$x[1:solutions],param.combs[RMSE.sort$ix[1:solutions],])
best.param.combs

# Parameter means
colMeans(best.param.combs) 


# Get modeled spec stats
mod.refl <- p.output.refl[RMSE.sort$ix[1:solutions],]
mod.trans <- p.output.tran[RMSE.sort$ix[1:solutions],]
mod.refl.minmax <- apply(mod.refl,2,quantile,na.rm=T,probs=c(0,1))
mod.trans.minmax <- apply(mod.trans,2,quantile,na.rm=T,probs=c(0,1))


### Plot results
dev.off()
waves <- seq(400,2500,1)
par(mar=c(5,5,1,5)) #b, l, t, r
plot(waves,meas.data[,2],type="l",lwd=2.5,ylim=c(0,1),xlab="Wavelength (nm)",
     ylab="Reflectance (%)",cex.axis=1.5, cex.lab=1.7)
polygon(c(waves ,rev(waves)),c(mod.refl.minmax[2,], rev(mod.refl.minmax[1,])),col="#99CC99",border=NA)
polygon(c(waves ,rev(waves)),c(1-mod.trans.minmax[2,], rev(1-mod.trans.minmax[1,])),col="#99CC99",border=NA)
lines(waves,meas.data[,2],lwd=2.5,col="black")
lines(waves,1-meas.data[,3],lwd=2.5,col="dark grey")
par(new=TRUE)
axis(4,labels = rev(seq(0.0, 1, 0.2)), at = seq(0.0, 1, 0.2),cex.axis=1.5, cex.lab=1.7)
mtext("Transmittance (%)",side=4,line=3,cex=1.7)
legend("right",legend=c("Observed","Modeled"),lty=c(1,2),lwd=2,bty="n",cex=1.5)
box(lwd=2.2)
#--------------------------------------------------------------------------------------------------#

### EOF