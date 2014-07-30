extracted_40m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_40m.csv",sep="\t", header=T)
file.info<-read.table(file="/Users/hardimanb/Desktop/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t") ##For Mac
wlef_abg<-read.csv("/Users/hardimanb/Desktop/data.remote/biometry/biometry_trimmed.csv", sep=",", header=T)


odds<-seq(1,ncol(extracted_40m),by=2)
evens<-seq(2,ncol(extracted_40m),by=2)
date.time<-as.vector(substr(file.info$scndate,1,8))
col_names<-c(rbind(paste(date.time, "HH",sep="_"),paste(date.time, "HV",sep="_")))
HHscn.dates<-as.Date(substr(col_names[odds],1,8),"%Y%m%d")
HVscn.dates<-as.Date(substr(col_names[evens],1,8),"%Y%m%d")

HH_wlef<-extracted_40m[,odds]
colnames(HH_wlef)<-date.time
HV_wlef<-extracted_40m[,evens]
colnames(HV_wlef)<-date.time

HH.calib <-HH_wlef[,1]
HV.calib <-HV_wlef[,1]
################################################################################################
y<-HV.calib
# y<-HH.calib
x<-(wlef_abg$ABG_biomass)^0.5
z<-HH.calib

biomass<-x
sel = which(x>0)
x = x[sel];y=y[sel]
################################################################################################
par(mfrow=c(1,2))
scatter.smooth(x,z,col="#CCCCCC",xlab="ABG_biomass",ylab="HH (gamma)",main=date.time[1])
scatter.smooth(x,y,col="#CCCCCC",xlab="ABG_biomass",ylab="HV (gamma)",main=date.time[1])

par(mfrow=c(1,1))
scatter.smooth(x,y,col="#CCCCCC",xlab="ABG_biomass",ylab="HV (gamma (dB))",main=date.time[1])
par(mfrow=c(1,1))
scatter.smooth(x,z,col="#CCCCCC",xlab="ABG_biomass",ylab="HH (gamma (dB))",main=date.time[1])

par(mfrow=c(1,1))
plot(x,y,col="#CCCCCC",xlab="ABG_biomass",ylab="HV (gamma (dB))",main=date.time[1])

###############################################################################################
##Funtion to estimate likelihood with monod2 (intercept)
###############################################################################################
k<-10 ##biomass (x) value at maximum HV (y) value
HVmax<-0.10
sd<-sd(HV.calib)

params<-c(k,HVmax,0.05,sd)

##Fits a monod function to ABG biomass and HV palsar returns
fit.monod2 = optim(par=params,ll.monod2,x=x,y=y)
fit.monod2
aic.monod2<- -2*fit.monod2$value + 2*length(params)

params = fit.monod2$par
xseq = seq(min(x),max(x),length=1000)
par(new=F)

par(mfrow=c(1,1))
plot(x,y,ylim=c(min(y),max(y)),xlab="(ABG Biomass)^0.5 (Mg/ha)",col="#CCCCCC",ylab="HV (gamma)",main="WLEF Calibration Plots")
lines(cbind(xseq,params[2]*(xseq/(xseq+params[1]))+params[3]),col="firebrick",lwd=3)

###############################################################################################
##Funtion to estimate likelihood for michaelis-menten
###############################################################################################
a<- 0.10
b<-10
sd<-sd(HV.calib)
params<-c(a,b,sd)

ll.micmen(params,x,y)

##Fits a michaelis-menten function to ABG biomass and HV palsar returns
fit.micmen = optim(par=params,ll.micmen,x=x,y=y)
aic.micmen<- -2*fit.micmen$value + 2*length(params)

params = fit.micmen$par
xseq = seq(min(x),max(x),length=1000)

lines(cbind(xseq,(params[1]*xseq)/(params[2]+xseq)),col=2,lwd=3)

###############################################################################################
##Funtion to estimate likelihood for Holling Type II
###############################################################################################
k<-10 ##biomass (x) value at maximum HV (y) value
HVmax<-0.10
sd<-sd(HV.calib)
params<-c(k,HVmax,0.05,sd)

par(new=F)
par(mfrow=c(1,1))
fit.holl2 = optim(params,ll.holling2,x=x,y=y)
aic.holl2<- -2*fit.holl2$value + 2*length(params)

params = fit.holl2$par
lines(xseq,params[1]*xseq/(1+xseq*params[2])+params[3],col=3,lwd=3)

###############################################################################################
##Funtion to estimate likelihood for Holling Type III
###############################################################################################
a<- 0.10
b<-10
sd<-sd(HV.calib)
params<-c(a,b,sd)

ll.holling3(params,x,y)

##Fits a Holling Type III function to ABG biomass and HV palsar returns
fit.holl3 = optim(par=params,ll.holling3,x=x,y=y)
aic.holl3<- -2*fit.holl3$value + 2*length(params)

params = fit.holl3$par
xseq = seq(min(x),max(x),length=1000)

lines(cbind(xseq,(params[1]*xseq^2)/(params[2]^2+xseq^2)),col=4,lwd=3)

###############################################################################################
##Funtion to estimate likelihood for Holling Type IV
###############################################################################################
a<- 1
b<-10
c<-1
sd<-sd(HV.calib)
params<-c(a,b,c,sd)

ll.holling4(params,x,y)

fit.holl4 = optim(par=params,ll.holling4,x=x,y=y)
aic.holl4<- -2*fit.holl4$value + 2*length(params)

params = fit.holl4$par
xseq = seq(min(x),max(x),length=1000)

par(mfrow=c(1,1))
plot(x,y,ylim=c(min(y),max(y)),xlab=expression(ABG_Biomass^0.5~ "(Mg/ha)"),col="#CCCCCC",ylab="HV (gamma)",main="WLEF Calibration Plots")
lines(cbind(xseq,(params[1]*xseq^2)/(params[2]+(params[3]*xseq)+xseq^2)),col=5,lwd=3)

###############################################################################################
##Funtion to estimate likelihood for Monomolecular
###############################################################################################
a<- 0.10
b<-10
int<-0.05
sd<-sd(HV.calib)
params<-c(a,b,int,sd)

fit.mono = optim(params,ll.mono,x=x,y=y)
aic.mono<- -2*fit.mono$value + 2*length(params)

params = fit.mono$par
xseq = seq(min(x),max(x),length=1000)

lines(cbind(xseq,params[1]*(1-exp(-xseq*params[2]))+params[3]),col=6,lwd=3)

###############################################################################################
##Funtion to estimate likelihood for NonRectangular Hyperbola
###############################################################################################
a<-0.1
b<-3
theta <- 0.8
int <- -15
sd<-1
params<-c(a,b,theta,int,sd)

fit.nrh = optim(params,ll.nrh,x=x,y=y)
aic.nrh<- -2*fit.nrh$value + 2*length(params)

params = fit.nrh$par
xseq = seq(min(x),max(x),length=1000)

###############################################################################################
###############################################################################################

rbind(c("monod2",aic.monod2),
      c("micmen",aic.micmen),
      c("holl2",aic.holl2),
      c("holl3",aic.holl3),
      c("holl4",aic.holl4),
      c("mono",aic.mono),
      c("nrh",aic.nrh))

###############################################################################################

params = fit.mono$par
eqn<-(params[1]*xseq^2)/(params[2]+(params[3]*xseq)+xseq^2))
nls(formula, data,