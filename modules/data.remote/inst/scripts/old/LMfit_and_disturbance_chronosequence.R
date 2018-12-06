extracted_40m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_40m.csv",sep="\t", header=T)
file.info<-read.table(file="/Users/hardimanb/Desktop/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t") ##For Mac
wlef_abg<-read.csv("/Users/hardimanb/Desktop/data.remote/biometry/biometry_trimmed.csv", sep=",", header=T)
disturbance_extracted_40m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/disturbance_extracted_40m.csv",sep="\t", header=T)
disturbance_inpath <-"/Users/hardimanb/Desktop/data.remote/biometry" ##For Mac
# disturbance_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry"
disturbance_infile <-read.csv(file.path(disturbance_inpath,"Cheas_coordinates_disturbance_year.csv"), sep=",", header=T) #disturbance plots


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

x<-HV.calib
y<-(wlef_abg$ABG_biomass)^0.5
z<-HH.calib

par(mfrow=c(1,2))
scatter.smooth(y,z,col="#CCCCCC",xlab=expression(~ABG_biomass^2~ "(Mg/ha)"),ylab="HH (gamma)",main=date.time[1])
scatter.smooth(y,x,col="#CCCCCC",xlab=expression(~ABG_biomass^2~ "(Mg/ha)"),ylab="HV (gamma)",main=date.time[1])

plot(x,y)
summary(lm(y~x))
abline(reg=lm(y~x),col=2,lwd=3)

################################################################################################
odds<-seq(1,ncol(disturbance_extracted_40m),by=2)
evens<-seq(2,ncol(disturbance_extracted_40m),by=2)

biomass<-x
sel = which(x>0)
x = x[sel];y=y[sel]

################################################################################################
##Linear Regression
################################################################################################
HVfit <- lm(y ~ x) 
HHfit <- lm(y ~ z) 
HH.HVfit <- lm(y ~ x + z) 

AIC(HVfit,HHfit,HH.HVfit)

summary(HH.HVfit)
coef<-coefficients(HH.HVfit)

xseq = seq(min(x),max(x),length=length(wlef_abg$ABG_biomass[wlef_abg$ABG_biomass<100]))
zseq = seq(min(z),max(z),length=length(wlef_abg$ABG_biomass[wlef_abg$ABG_biomass<100]))

par(mfrow=c(1,1))
plot(HV.calib,(wlef_abg$ABG_biomass)^0.5,col="#CCCCCC",ylab="ABG_biomass",xlab="HV (gamma (dB))",main=date.time[1])
lines(cbind(xseq,coef[2]*xseq+ coef[3]*zseq + coef[1]),col=2,lwd=3)

par(mfrow=c(1,1))
plot(cbind(sqrt(wlef_abg$ABG_biomass[wlef_abg$ABG_biomass<100]),(coef[2]*xseq+ coef[3]*zseq + coef[1])),ylim=c(0,10),xlab="(Observed Biomass)^0.5 (Mg/ha)",ylab="(Predicted Biomass)^0.5 (Mg/ha)")
abline(a = 0, b = 1,col="red",lwd=3)
################################################################################################
disturbance_signal<- disturbance_extracted_40m
HH_disturb<-disturbance_signal[,odds]
HV_disturb<-disturbance_signal[,evens]
disturbance_extracted_40m

scn.dates<-as.Date(substr(colnames(disturbance_extracted_40m),2,9),"%Y%m%d")
scn.yr<-substr(colnames(disturbance_extracted_40m),2,5)
scn.yr<-as.numeric(scn.yr[odds])

colnames(HH_disturb)<-as.character(scn.dates[odds])
colnames(HV_disturb)<-as.character(scn.dates[evens])

disturbance_ages<-matrix(NA,nrow(HH_disturb),length(scn.yr))
colnames(disturbance_ages)<-as.character(scn.dates[evens])
for(i in 1:length(scn.yr)){
  disturbance_ages[,i]<-scn.yr[i]-disturbance_infile$distyr
}

initial_age<-sort(unique(disturbance_ages[,1]))

dist_biomass_est<-(coef[2]*disturbance_extracted_40m[,2]+ coef[3]*disturbance_extracted_40m[,1] + coef[1])
plot(disturbance_ages[,1],dist_biomass_est,ylab="sqrt(Estimated Biomass (Mg/ha))",xlab="Time since disturbance (years)")

mean_est_biomass<-vector(mode="numeric",length=0)
for(i in 1:length(initial_age)){
  mean_est_biomass<-c(mean_est_biomass,mean(dist_biomass_est[initial_age==initial_age[i]],na.rm = T))
}

plot(initial_age,mean_est_biomass,ylab="sqrt(Estimated Biomass (Mg/ha))",xlab="Time since disturbance (years)")

par(new=F)
par(mfrow=c(1,1))
for(i in 1:ncol(HH_disturb)){
  plot(disturbance_infile$distyr[HH_disturb[,i]>0],HH_disturb[HH_disturb[,i]>0,i], xlim=c(1985,2010),ylim=c(0,0.5),xlab="",ylab="",axes=F)
  par(new=T)
}
par(new=T)
plot(disturbance_infile$distyr[HH_disturb[,i]>0],HH_disturb[HH_disturb[,i]>0,i], xlim=c(1985,2010),ylim=c(0,0.5),xlab="Year of disturbance",ylab="Extracted HH returns",type="n")

par(new=F)
par(mfrow=c(1,1))
for(i in 1:ncol(HH_disturb)){
  plot(disturbance_infile$distyr[HV_disturb[,i]>0],HV_disturb[HV_disturb[,i]>0,i], xlim=c(1985,2010),ylim=c(0,0.15),xlab="",ylab="",axes=F)
  par(new=T)
}
par(new=T)
plot(disturbance_infile$distyr[HV_disturb[,i]>0],HV_disturb[HV_disturb[,i]>0,i], xlim=c(1985,2010),ylim=c(0,0.15),xlab="Year of disturbance",ylab="Extracted HV returns",type="n")

HV_dist_chrono<-na.omit(
rbind(cbind(disturbance_ages[HV_disturb[,12]>0,12],HV_disturb[HV_disturb[,12]>0,12]),
      cbind(disturbance_ages[HV_disturb[,13]>0,13],HV_disturb[HV_disturb[,13]>0,13]),
      cbind(disturbance_ages[HV_disturb[,14]>0,14],HV_disturb[HV_disturb[,14]>0,14]),
      cbind(disturbance_ages[HV_disturb[,15]>0,15],HV_disturb[HV_disturb[,15]>0,15]),
      cbind(disturbance_ages[HV_disturb[,16]>0,16],HV_disturb[HV_disturb[,16]>0,16]),
      cbind(disturbance_ages[HV_disturb[,17]>0,17],HV_disturb[HV_disturb[,17]>0,17]),
      cbind(disturbance_ages[HV_disturb[,18]>0,18],HV_disturb[HV_disturb[,18]>0,18]),
      cbind(disturbance_ages[HV_disturb[,19]>0,19],HV_disturb[HV_disturb[,19]>0,19]),
      cbind(disturbance_ages[HV_disturb[,20]>0,20],HV_disturb[HV_disturb[,20]>0,20]),
      cbind(disturbance_ages[HV_disturb[,21]>0,21],HV_disturb[HV_disturb[,21]>0,21])))
colnames(HV_dist_chrono)<-c("age","HV")


chrono<-sort(unique(HV_dist_chrono[,1]))
distchron<-matrix(NA,length(chrono),2)

for(i in 1:length(chrono)){
  distchron[i,1]<-chrono[i]
  distchron[i,2]<-mean(HV_dist_chrono[HV_dist_chrono[,1]==chrono[i],2],na.rm=T)
}

plot(distchron[,1],distchron[,2])
age_vs_HV <- lm(distchron[,2] ~ distchron[,1])
summary(age_vs_HV)
abline(reg=age_vs_HV,col=2,lwd=3)


plot(HV_dist_chrono,xlab="Forest Age (years)",ylab="HV (gamma)")
age_vs_HV <- lm(HV_dist_chrono[,2] ~ HV_dist_chrono[,1])
summary(age_vs_HV)
abline(reg=age_vs_HV,col=2,lwd=3)
text(x=15,y=0.11,expression(~R^2~"= 0.06, p<0.005"))

par(mfrow=c(1,1))
iseq<-seq(12,21, by=1)
cols<-rainbow(length(iseq))
cols<-c("black","red","blue", "green","cyan","magenta","yellow","orange")
for(i in 13:18){
  plot(disturbance_ages[HV_disturb[,i]>0,i],HV_disturb[HV_disturb[,i]>0,i],xlim=c(min(HV_dist_chrono[,1]),max(HV_dist_chrono[,1])),ylim=c(min(HV_dist_chrono[,2]),max(HV_dist_chrono[,2])),xlab="",ylab="",axes=F,col=cols[i-11])
  age_vs_HV <- lm(HV_disturb[HV_disturb[,i]>0,i] ~ disturbance_ages[HV_disturb[,i]>0,i])
  summary(age_vs_HV)
  abline(reg=age_vs_HV,col=cols[1],lwd=3)
  par(new=T)
}
par(new=T)
plot(disturbance_ages[HV_disturb[,i]>0,i],HV_disturb[HV_disturb[,i]>0,i],xlim=c(min(HV_dist_chrono[,1]),max(HV_dist_chrono[,1])),ylim=c(min(HV_dist_chrono[,2]),max(HV_dist_chrono[,2])),xlab="Forest Age (years)",ylab="HV (gamma)",main="2010 PALSAR Scenes")
text(x=22,y=0.11,expression(~R^2~"= 0.18, p<0.05"))
#################################################################
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
#################################################################

HV_dist_chrono<-cbind(HV_dist_chrono[,1],HV_dist_chrono[,2])
colnames(HV_dist_chrono)<-c("age","HV")
boxplot(data=HV_dist_chrono, HV ~ age,xlab="Forest Age (years)",ylab="HV (gamma)")

HH_dist_chrono<-na.omit(
  rbind(cbind(disturbance_ages[HH_disturb[,12]>0,12],HH_disturb[HH_disturb[,12]>0,12]),
        cbind(disturbance_ages[HH_disturb[,13]>0,13],HH_disturb[HH_disturb[,13]>0,13]),
        cbind(disturbance_ages[HH_disturb[,14]>0,14],HH_disturb[HH_disturb[,14]>0,14]),
        cbind(disturbance_ages[HH_disturb[,15]>0,15],HH_disturb[HH_disturb[,15]>0,15]),
        cbind(disturbance_ages[HH_disturb[,16]>0,16],HH_disturb[HH_disturb[,16]>0,16]),
        cbind(disturbance_ages[HH_disturb[,17]>0,17],HH_disturb[HH_disturb[,17]>0,17]),
        cbind(disturbance_ages[HH_disturb[,18]>0,18],HH_disturb[HH_disturb[,18]>0,18]),
        cbind(disturbance_ages[HH_disturb[,19]>0,19],HH_disturb[HH_disturb[,19]>0,19]),
        cbind(disturbance_ages[HH_disturb[,20]>0,20],HH_disturb[HH_disturb[,20]>0,20]),
        cbind(disturbance_ages[HH_disturb[,21]>0,21],HH_disturb[HH_disturb[,21]>0,21])))
plot(HH_dist_chrono)

HH_dist_chrono<-cbind(HH_dist_chrono[,1],HH_dist_chrono[,2])
colnames(HH_dist_chrono)<-c("age","HH")
boxplot(data=HH_dist_chrono, HH ~ age,xlab="Age of disturbance (years)",ylab="HH (gamma)")



sort(unique(c(unique(disturbance_ages[,1]),
  unique(disturbance_ages[,2]),
unique(disturbance_ages[,3]),
unique(disturbance_ages[,4]),
unique(disturbance_ages[,5]),
unique(disturbance_ages[,6]),
unique(disturbance_ages[,7]),
unique(disturbance_ages[,8]),
unique(disturbance_ages[,9]),
unique(disturbance_ages[,10]),
unique(disturbance_ages[,11]),
unique(disturbance_ages[,12]),
unique(disturbance_ages[,13]),
unique(disturbance_ages[,14]),
unique(disturbance_ages[,15]),
unique(disturbance_ages[,16]),
unique(disturbance_ages[,17]),
unique(disturbance_ages[,18]),
unique(disturbance_ages[,19]),
unique(disturbance_ages[,20]),
unique(disturbance_ages[,21]))))


par(new=F)
par(mfrow=c(1,1))
plot(scn.dates[odds],HV_disturb[3,],type="n")
lines(scn.dates[odds],HV_disturb[3,],type="b")
lines(scn.dates[odds],HV_disturb[4,],type="b", col=2)
lines(scn.dates[odds],HV_disturb[5,],type="b", col=3)
lines(scn.dates[odds],HV_disturb[6,],type="b", col=4)
lines(scn.dates[odds],HV_disturb[7,],type="b", col=5)
lines(scn.dates[odds],HV_disturb[8,],type="b", col=6)
lines(scn.dates[odds],HV_disturb[9,],type="b", col=7)


HV.calib <-HV_wlef[,1]
(params[1]*xseq^2)/(params[2]+(params[3]*xseq)+xseq^2)
