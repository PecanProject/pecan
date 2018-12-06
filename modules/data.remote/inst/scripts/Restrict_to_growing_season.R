#Restrict to growing season scndates only (Jun-Aug)
dat48.gs<- dat48[as.numeric(format(dat48$scndate,"%m"))>5 & as.numeric(format(dat48$scndate,"%m"))<9,]
dat48.gs$year<-as.numeric(format(dat48.gs$scndate,"%Y"))
dat48.gs$month<-as.numeric(format(dat48.gs$scndate,"%m"))

plot(dat48.gs$biomass,dat48.gs$HH.sigma.48)
plot(dat48.gs$biomass,dat48.gs$HV.sigma.48)

crap<-aggregate(dat48.gs,list(dat48.gs$plot,dat48.gs$year,dat48.gs$month),mean)
plot(crap$biomass[crap$year==2007], crap$HH.sigma.48[crap$year==2007])

HH.gs<-tapply(dat48.gs$HH.sigma.48,list(dat48.gs$plot,dat48.gs$month,dat48.gs$year),mean)
HV.gs<-tapply(dat48.gs$HV.sigma.48,list(dat48.gs$plot,dat48.gs$month,dat48.gs$year),mean)

HH.gs.2007<-cbind(melt(HH.gs[,,1],id=HH.gs[,1,1])[1],rep.int(2007,nrow(HH.gs[,,1])),melt(HH.gs[,,1],id=HH.gs[,1,1])[2:3])
HH.gs.2008<-cbind(melt(HH.gs[,,2],id=HH.gs[,1,2])[1],rep.int(2008,nrow(HH.gs[,,1])),melt(HH.gs[,,2],id=HH.gs[,1,2])[2:3])
HH.gs.2009<-cbind(melt(HH.gs[,,3],id=HH.gs[,1,3])[1],rep.int(2009,nrow(HH.gs[,,1])),melt(HH.gs[,,3],id=HH.gs[,1,3])[2:3])
HH.gs.2010<-cbind(melt(HH.gs[,,4],id=HH.gs[,1,4])[1],rep.int(2010,nrow(HH.gs[,,1])),melt(HH.gs[,,4],id=HH.gs[,1,4])[2:3])

HV.gs.2007<-cbind(melt(HV.gs[,,1],id=HV.gs[,1,1])[1],rep.int(2007,nrow(HV.gs[,,1])),melt(HV.gs[,,1],id=HV.gs[,1,1])[2:3])
HV.gs.2008<-cbind(melt(HV.gs[,,2],id=HV.gs[,1,2])[1],rep.int(2008,nrow(HV.gs[,,1])),melt(HV.gs[,,2],id=HV.gs[,1,2])[2:3])
HV.gs.2009<-cbind(melt(HV.gs[,,3],id=HV.gs[,1,3])[1],rep.int(2009,nrow(HV.gs[,,1])),melt(HV.gs[,,3],id=HV.gs[,1,3])[2:3])
HV.gs.2010<-cbind(melt(HV.gs[,,4],id=HV.gs[,1,4])[1],rep.int(2010,nrow(HV.gs[,,1])),melt(HV.gs[,,4],id=HV.gs[,1,4])[2:3])

colnames(HH.gs.2007)<-c("plot","year","month","HH")
colnames(HH.gs.2008)<-c("plot","year","month","HH")
colnames(HH.gs.2009)<-c("plot","year","month","HH")
colnames(HH.gs.2010)<-c("plot","year","month","HH")
colnames(HV.gs.2007)<-c("plot","year","month","HV")
colnames(HV.gs.2008)<-c("plot","year","month","HV")
colnames(HV.gs.2009)<-c("plot","year","month","HV")
colnames(HV.gs.2010)<-c("plot","year","month","HV")

HH.gs<-rbind(HH.gs.2007,HH.gs.2008,HH.gs.2009,HH.gs.2010)
HV.gs<-rbind(HV.gs.2007,HV.gs.2008,HV.gs.2009,HV.gs.2010)

dat48.gs<-cbind(HH.gs,HV.gs$HV)