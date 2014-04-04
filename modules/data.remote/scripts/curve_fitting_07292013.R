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


par(mfrow=c(1,2))
scatter.smooth(wlef_abg$ABG_biomass,HH.calib,col="#CCCCCC",xlab="ABG_biomass",ylab="HH (gamma (dB))",main=date.time[1])
scatter.smooth(wlef_abg$ABG_biomass,HV.calib,col="#CCCCCC",xlab="ABG_biomass",ylab="HV (gamma (dB))",main=date.time[1])

par(mfrow=c(1,1))
scatter.smooth(wlef_abg$ABG_biomass,HV.calib,col="#CCCCCC",xlab="ABG_biomass",ylab="HV (gamma (dB))",main=date.time[1])
###############################################################################################
##Funtion to estimate likelihood
###############################################################################################
k<-10 ##biomass (x) value at maximum HV (y) value
HVmax<-0.14
sd<-sd(HV.calib)

params<-c(k,HVmax,0.05,sd)

y<-HV.calib
x<-(wlef_abg$ABG_biomass)^0.5
biomass<-x
sel = which(x>0)
x = x[sel];y=y[sel]

ll.monod2(params,x,y)

##Fits a monod function to ABG biomass and HV palsar returns
fit.monod2 = optim(par=params,ll.monod2,x=x,y=y)
fit.monod2
aic.monod2<- -2*fit.monod2$value + 2*(length(params)-1)

params = fit1$par
par(new=F)
par(mfrow=c(1,1))
plot(x,y,ylim=c(min(y),max(y)),xlab="ABG Biomass (Mg/ha)",ylab="HV (gamma (dB))",main="WLEF_HV Monod Function")
xseq = seq(min(x),max(x),length=1000)
lines(xseq,params[2]*xseq/(xseq+params[1])+params[3],col=2,lwd=3)
# lines(cbind(x,y),col=3,lwd=3)

par(new=F)
par(mfrow=c(1,1))
fit.holl2 = optim(params,ll.holling2,x=x,y=y)
aic.holl2<- -2*fit.holl2$value + 2*(length(params)-1)
params = fit2$par
lines(xseq,params[1]*xseq/(1+xseq*params[2])+params[3],col=3,lwd=3)

par(new=F)
par(mfrow=c(1,1))
fit.mono = optim(params,ll.mono,x=x,y=y)
aic.mono<- -2*fit.mono2$value + 2*(length(params)-1)
params = fit3$par
lines(xseq,params[1]*(1-exp(-xseq*params[2]))+params[3],col=4,lwd=3)

par(new=F)
par(mfrow=c(1,1))
fit.nrh = optim(c(0.1,3,0.8,-15,1),ll.nrh,x=x,y=y)
aic.nrh<- -2*fit.nrh$value + 2*(length(params)-1)
params = fit4$par
lines(xseq,1/(2*params[3])*(params[1]*xseq+params[2]-sqrt((params[1]*xseq+params[2])^2-4*params[3]*params[2]*params[1]*xseq))+params[4],col=5,lwd=3)
###############################################################################################
##Funtion to estimate likelihood for michaelis-menten
###############################################################################################
a<- -12
b<-100
sd<-sd(HV.calib)
params<-c(a,b,sd)

y<-HV.calib
x<-wlef_abg$ABG_biomass
biomass<-x
sel = which(x>0)
x = x[sel];y=y[sel]

ll.micmen(params,x,y)

##Fits a michaelis-menten function to ABG biomass and HV palsar returns
fit1 = optim(par=params,ll.micmen,x=x,y=y)
fit1
params = fit1$par
xseq = seq(min(x),max(x),length=1000)
par(new=F)

par(mfrow=c(1,1))
plot(x,y,ylim=c(min(y),max(y)),xlab="ABG Biomass (Mg/ha)",ylab="HV (gamma (dB))",main="WLEF_HV Michaelis-Menten Function")
lines(cbind(xseq,(params[1]*xseq)/(params[2]+xseq)),col=2,lwd=3)
# lines(cbind(x,y),col=3,lwd=3)

###############################################################################################
##Funtion to estimate likelihood for Holling Type III
###############################################################################################
a<- -12
b<-100
sd<-sd(HV.calib)
params<-c(a,b,sd)

y<-HV.calib
x<-wlef_abg$ABG_biomass
biomass<-x
sel = which(x>0)
x = x[sel];y=y[sel]

ll.holling3(params,x,y)

##Fits a Holling Type III function to ABG biomass and HV palsar returns
fit1 = optim(par=params,ll.micmen,x=x,y=y)
fit1
params = fit1$par
xseq = seq(min(x),max(x),length=1000)
par(new=F)

par(mfrow=c(1,1))
plot(x,y,ylim=c(min(y),max(y)),xlab="ABG Biomass (Mg/ha)",ylab="HV (gamma (dB))",main="WLEF_HV Holling Type III Function")
lines(cbind(xseq,(params[1]*xseq^2)/(params[2]^2+xseq^2)),col=2,lwd=3)
# lines(cbind(x,y),col=3,lwd=3)

###############################################################################################
##Funtion to estimate likelihood for Holling Type IV
###############################################################################################
a<- -12
b<-100
sd<-sd(HV.calib)
params<-c(a,b,sd)

y<-HV.calib
x<-wlef_abg$ABG_biomass
biomass<-x
sel = which(x>0)
x = x[sel];y=y[sel]

ll.holling4(params,x,y)

##Fits a Holling Type IV function to ABG biomass and HV palsar returns
fit1 = optim(par=params,ll.holling4,x=x,y=y)
fit1
params = fit1$par
xseq = seq(min(x),max(x),length=1000)
par(new=F)

par(mfrow=c(1,1))
plot(x,y,ylim=c(min(y),max(y)),xlab="ABG Biomass (Mg/ha)",ylab="HV (gamma (dB))",main="WLEF_HV Michaelis-Menten Function")
lines(cbind(xseq,(params[1]*xseq^2)/(params[2]^2+xseq^2)),col=2,lwd=3)
# lines(cbind(x,y),col=3,lwd=3)

##This section forces the monod function to fit the intercept. It does worse than the regular monod.
# params2 = c(50,0.7,0.2,1)
# fit2 = optim(par=params2,ll.monod2,x=x,y=y)
# fit2
# params2 = fit2$par
# lines(xseq,params2[2]*xseq/(xseq+params2[1])+params2[3],col=4,lwd=3)
# lines(lowess(x,y),col=5,lwd=3)

bin.size = 25
xbin = seq(0,450,bin.size)
bin = findInterval(x,xbin)
bin.mu = tapply(y,bin,mean,na.rm=TRUE)
bin.sd = tapply(y,bin,sd,na.rm=TRUE)
points(xbin[sort(as.numeric(names(bin.mu)))]+bin.size/2,bin.mu,col="orange",cex=3,pch=18)
points(xbin[sort(as.numeric(names(bin.mu)))]+bin.size/2,bin.mu+bin.sd,col="orange",cex=3,pch="_")
points(xbin[sort(as.numeric(names(bin.mu)))]+bin.size/2,bin.mu-bin.sd,col="orange",cex=3,pch="_")

biomass<-loess.smooth(wlef_abg$ABG_biomass,HV_signal[,1])$x
HVvals<-loess.smooth(wlef_abg$ABG_biomass,HV_signal[,1])$y
par(mfrow=c(1,1))
plot(cbind(biomass,HVvals))
plot(loess.smooth(wlef_abg$ABG_biomass,HV_signal[,1]))