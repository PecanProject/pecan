palsar.plotter<-function(outpath,coord.set,fia){
  
  dat48<-read.csv(paste(outpath,"/",coord.set[fia+1],"_dat48.csv",sep=""),header = TRUE)
  
  dat48$scnid<-as.character(dat48$scnid)
  dat48$scndate<-as.Date(dat48$scndate,"%Y-%m-%d")
  dat48$plot<-as.numeric(dat48$plot)
  dat48$UTM.lat<- as.numeric(as.character(dat48$UTM.lat))
  dat48$UTM.lon<- as.numeric(as.character(dat48$UTM.lon))
  dat48$biomass<- as.numeric(as.character(dat48$biomass))
  dat48$HH.sigma.48<- as.numeric(as.character(dat48$HH.sigma.48))
  dat48$HV.sigma.48<- as.numeric(as.character(dat48$HV.sigma.48))
  dat48$year<-as.numeric(format(dat48$scndate,"%Y"))
  dat48$month<-as.numeric(format(dat48$scndate,"%m"))
  dat48$HHse_data_48m<- as.numeric(as.character(dat48$HHse_data_48m))
  dat48$HVse_data_48m<- as.numeric(as.character(dat48$HVse_data_48m))
  
  #Generate PDF of raw data exploration
  #NOTE: Some of these figures will not be relevant for the FIA dataset
  pdf(paste(outpath,"/",coord.set[fia+1], "_ExtractionQCplots.pdf",sep=""),width = 6, height = 6, paper='special')
  
  #Plot boxplots of each scndate by year (HH)
  par(mfrow=c(2,2))
  boxplot(dat48$HH.sigma.48[dat48$year==2007] ~ dat48$month[dat48$year==2007],xlab="month",main="2007 HH")
  boxplot(dat48$HH.sigma.48[dat48$year==2008] ~ dat48$month[dat48$year==2008],xlab="month",main="2008 HH")
  boxplot(dat48$HH.sigma.48[dat48$year==2009] ~ dat48$month[dat48$year==2009],xlab="month",main="2009 HH")
  boxplot(dat48$HH.sigma.48[dat48$year==2010] ~ dat48$month[dat48$year==2010],xlab="month",main="2010 HH")
  
  #Plot boxplots of each scndate by year (HV)
  par(mfrow=c(2,2))
  boxplot(dat48$HV.sigma.48[dat48$year==2007] ~ dat48$month[dat48$year==2007],xlab="month",main="2007 HV")
  boxplot(dat48$HV.sigma.48[dat48$year==2008] ~ dat48$month[dat48$year==2008],xlab="month",main="2008 HV")
  boxplot(dat48$HV.sigma.48[dat48$year==2009] ~ dat48$month[dat48$year==2009],xlab="month",main="2009 HV")
  boxplot(dat48$HV.sigma.48[dat48$year==2010] ~ dat48$month[dat48$year==2010],xlab="month",main="2010 HV")
  
  #Plot comparing HH values of May, June, August 2007
  par(mfrow=c(1,3))
  plot(dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HH.sigma.48[dat48$month==6 & dat48$year==2007],
       xlab="05 HH",ylab='06 HH',main="may 2007 vs jun 2007")
  fit1<-lm(dat48$HH.sigma.48[dat48$month==6 & dat48$year==2007] ~ dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007])
  abline(0,1,lwd=2,lty=2,col="grey")
  abline(fit1,lwd=2,lty=1,col="red")
  plot(dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007],
       xlab="05 HH",ylab='08 HH',main="may 2007 vs aug 2007")
  fit2<-lm(dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007] ~ dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007])
  abline(0,1,lwd=2,lty=2,col="grey")
  abline(fit1,lwd=2,lty=1,col="red")
  plot(dat48$HH.sigma.48[dat48$month==6  & dat48$year==2007],dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007],
       xlab="06 HH",ylab='08 HH',main="jun 2007 vs aug 2007")
  fit3<-lm(dat48$HH.sigma.48[dat48$month==6  & dat48$year==2007] ~ dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007])
  abline(0,1,lwd=2,lty=2,col="grey")
  abline(fit1,lwd=2,lty=1,col="red")
  
  #Plot comparing HV values of May, June, August 2007
  par(mfrow=c(1,3))
  plot(dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HV.sigma.48[dat48$month==6 & dat48$year==2007],
       xlab="05 HV",ylab='06 HV',main="may 2007 vs jun 2007")
  fit1<-lm(dat48$HV.sigma.48[dat48$month==6 & dat48$year==2007] ~ dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007])
  abline(0,1,lwd=2,lty=2,col="grey")
  abline(fit1,lwd=2,lty=1,col="red")
  plot(dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007],
       xlab="05 HV",ylab='08 HV',main="may 2007 vs aug 2007")
  fit2<-lm(dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007] ~ dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007])
  abline(0,1,lwd=2,lty=2,col="grey")
  abline(fit1,lwd=2,lty=1,col="red")
  plot(dat48$HV.sigma.48[dat48$month==6  & dat48$year==2007],dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007],
       xlab="06 HV",ylab='08 HV',main="jun 2007 vs aug 2007")
  fit3<-lm(dat48$HV.sigma.48[dat48$month==6  & dat48$year==2007] ~ dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007])
  abline(0,1,lwd=2,lty=2,col="grey")
  abline(fit1,lwd=2,lty=1,col="red")
  
  #######################################
  ####                                ### 
  #######################################
  
  
  #Plot scene frequency by year, month
  par(mfrow=c(1,2))
  hist(dat48$year,freq=TRUE,main="By year")
  hist(dat48$month,freq=TRUE,main="By month")
  
  # par(mfrow=c(1,1))
  # hist(dat48$scndate,freq=T,100,xaxt="n")
  # axis(1, dat48$scndate, format(dat48$scndate, "%b %Y"), cex.axis = .7)
  
  par(mfrow=c(1,3))
  hist(dat48$biomass,main=paste(coord.set[fia+1],"biomass",sep=" "))
  hist(dat48$HH.sigma.48,main=paste(coord.set[fia+1],"HH",sep=" "))
  hist(dat48$HV.sigma.48,main=paste(coord.set[fia+1],"HV",sep=" "))
  
  #Figure showing color-coded density plots of the data
  Lab.palette <- colorRampPalette(c("white","violet","blue","green","yellow","orange", "red"), space = "Lab")
  par(mfrow=c(1,3))
  smoothScatter(dat48$HV.sigma.48,dat48$HH.sigma.48,nbin=256,colramp = Lab.palette,xlab="HV",ylab="HH")
  smoothScatter(dat48$biomass,dat48$HH.sigma.48,nbin=256,colramp = Lab.palette,xlab="biomass",ylab="HH",main="Density")
  smoothScatter(dat48$biomass,dat48$HV.sigma.48,nbin=256,colramp = Lab.palette,ylim=c(0,max(dat48$HH.sigma.48)),xlab="biomass",ylab="HV")
  
  #Figure of biomass vs backscatter color-coded by year of scndate
  par(mfrow=c(1,2))
  scatter.smooth(dat48$biomass,dat48$HH.sigma.48,cex=0,xlab="biomass",ylab="HH",main="48m",col="grey")
  points(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2007],col=1,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2008],col=2,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2009],col=3,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col=4,cex=0.5)
  legend("topright",pch=1,legend=c(2007,2008,2009,2010), cex=0.7,pt.cex=0.5,col=1:4,bty="n",xjust=1)
  scatter.smooth(dat48$biomass,dat48$HV.sigma.48,cex=0,xlab="biomass",ylab="HV",main="48m",col="grey")
  points(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2007],col=1,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2008],col=2,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2009],col=3,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010],col=4,cex=0.5)
  legend("topright",pch=1,legend=c(2007,2008,2009,2010), cex=0.7,pt.cex=0.5,col=1:4,bty="n",xjust=1)
  # scatter.smooth(dat60$biomass,dat60$HV.sigma.60,xlab="biomass",ylab="HV",main="60m",col="grey")
  # scatter.smooth(dat60$biomass,dat60$HV.sigma.60,xlab="biomass",ylab="HV",main="60m",col="grey")
  
  #Figure showing ratio and product of backscatter bands vs biomass
  par(mfrow=c(1,2))
  scatter.smooth(dat48$biomass,dat48$HV.sigma.48/dat48$HH.sigma.48,xlab="biomass",ylab="HV/HH",main="48m",col="grey")
  # scatter.smooth(dat60$biomass,dat60$HV.sigma.60/dat60$HV.sigma.60,xlab="biomass",ylab="HV/HV",main="60m",col="grey")
  scatter.smooth(dat48$biomass,dat48$HH.sigma.48*dat48$HV.sigma.48,xlab="biomass",ylab="HHxHV",main="48m",col="grey")
  # scatter.smooth(dat60$biomass,dat60$HV.sigma.60*dat60$HV.sigma.60,xlab="biomass",ylab="HVxHV",main="60m",col="grey")
  
  #Plot NDVI-style ratio of the two backscatter bands
  par(mfrow=c(1,1))
  scatter.smooth(dat48$biomass,(dat48$HH.sigma.48-dat48$HV.sigma.48)/(dat48$HH.sigma.48+dat48$HV.sigma.48),xlab="biomass",ylab="(HH-HV)/(HH+HV)",main="48m", col="gray")
  # scatter.smooth(dat60$biomass,(dat60$HV.sigma.60-dat60$HV.sigma.60)/(dat60$HV.sigma.60+dat60$HV.sigma.60),xlab="biomass",ylab="(HV-HV)/(HV+HV)",main="60m", col="gray")
  
  #Figure illustrating the effect of seasonality on backscatter values (non-growing season values tend to be much higher)
  #     NOTE: Due to significant effect of season, and poor replication in non-growing season scndates, we restrict analysis to growing season only
  # par(mfrow=c(4,2),mar=c(4,4,2,2))
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2007],col="grey",xlab="biomass",ylab="HH",main="2007")
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2007],col="grey",xlab="biomass",ylab="HV",main="2007")
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2008],col="grey",xlab="biomass",ylab="HH",main="2008")
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2008],col="grey",xlab="biomass",ylab="HV",main="2008")
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2009],col="grey",xlab="biomass",ylab="HH",main="2009")
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2009],col="grey",xlab="biomass",ylab="HV",main="2009")
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HH",main="2010")
  #   points(dat48$biomass[format(dat48$scndate,"%m")>10],dat48$HH.sigma.48[format(dat48$scndate,"%m")>10],col="red",xlab="biomass",ylab="HV",main="2010")
  #   legend("topright",pch=1,legend=c("!Dec","Dec"), cex=0.7,pt.cex=0.5,col=c("grey","red"),bty="n",xjust=1)
  # scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HV",main="2010")
  #   points(dat48$biomass[format(dat48$scndate,"%m")>10],dat48$HV.sigma.48[format(dat48$scndate,"%m")>10],col="red",xlab="biomass",ylab="HV",main="2010")
  #   legend("topright",pch=1,legend=c("!Dec","Dec"), cex=0.7,pt.cex=0.5,col=c("grey","red"),bty="n",xjust=1)
  
  par(mfrow=c(1,2))
  plot(dat48$scndate,dat48$HH.sigma.48,ylim=c(0,max(dat48$HH.sigma.48)),xlab="Date",ylab="HH")
  plot(dat48$scndate,dat48$HV.sigma.48,ylim=c(0,max(dat48$HH.sigma.48)),xlab="Date",ylab="HV")
  mtext("On same scale", side=3, line=-2, outer=TRUE, cex=1, font=2)
  
  par(mfrow=c(1,2))
  plot(dat48$scndate,dat48$HH.sigma.48,ylim=c(0,max(dat48$HH.sigma.48)),xlab="Date",ylab="HH")
  plot(dat48$scndate,dat48$HV.sigma.48,ylim=c(0,max(dat48$HV.sigma.48)),xlab="Date",ylab="HV")
  mtext("By Date", side=3, line=-2, outer=TRUE, cex=1, font=2)
  
  par(mfrow=c(1,2))
  plot(dat48$scndate[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],xlab="2010",ylab="HH")
  plot(dat48$scndate[format(dat48$scndate,"%Y")==2010],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010],xlab="2010",ylab="HV")
  mtext("2010 only", side=3, line=-3, outer=TRUE, cex=1, font=2)
  
  #Plots demonstrating the effects of including DEC palsar scndate
  if(leaf.off==1){ #Only plots if leaf off period is included
    par(mfrow=c(2,2))
    scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HH",main="2010 only")
    points(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10])
    scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HH",main="2010 only")
    points(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10])
    scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],col="grey",xlab="biomass",ylab="HH",main="2010 only,Dec. removed")
    scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],col="grey",xlab="biomass",ylab="HV",main="2010 only,Dec. removed")
  }
  
  # #Plot individual time series of HH for each coordinate set
  # par(new=FALSE, mfrow=c(1,1))
  # plot(format(dat48$scndate,"%Y"),dat48$HH.sigma.48,col="grey",cex=0.5,
  #      xlab="Year",ylab="HH",main="Average backscatter/plot/year")
  # date.plot.HHmean<-tapply(dat48$HH.sigma.48,list(dat48$plot,format(dat48$scndate,"%Y")),mean,na.rm=TRUE) #mean HH for each plot in each year
  # for(i in dat48$plot){
  #   lines(cbind(c(2007,2008,2009,2010),date.plot.HHmean[dat48$plot[i],]),col=i)
  #   par(new=T)
  # }
  # #Plot individual time series of HV for each coordinate set
  # par(new=FALSE, mfrow=c(1,1))
  # plot(format(dat48$scndate,"%Y"),dat48$HV.sigma.48,col="grey",cex=0.5,
  #      xlab="Year",ylab="HV",main="Average backscatter/plot/year")
  # date.plot.HVmean<-tapply(dat48$HV.sigma.48,list(dat48$plot,format(dat48$scndate,"%Y")),mean,na.rm=TRUE) #mean HV for each plot in each year
  # date.plot.HVmean<-na.exclude(date.plot.HVmean)
  # for(i in dat48$plot){
  #   lines(cbind(c(2007,2008,2009,2010),date.plot.HVmean[i,]),col=i)
  #   par(new=T)
  # }
  
  #breaks biomass data into quantiles each containing ~5% of the data
  bind.bio<-tapply(dat48$biomass,   cut(dat48$biomass,    breaks=                  round(quantile(dat48$biomass,probs = seq(0, 1, 0.05))) ),mean)
  # bind.HH<-tapply(dat48$HH.sigma.48,cut(dat48$HH.sigma.48,breaks=dat48$HH.sigma.48[round(quantile(dat48$biomass,probs = seq(0, 1, 0.05)))] ),mean)
  # bind.HV<-tapply(dat48$HV.sigma.48,cut(dat48$HV.sigma.48,breaks=dat48$HV.sigma.48[round(quantile(dat48$biomass,probs = seq(0, 1, 0.05)))] ),mean)
  
  cuts<-matrix(unlist(strsplit(names(bind.bio),",")),ncol=2,byrow=T)
  for(i in 1:nrow(cuts)){
    for(j in 1:ncol(cuts)){
      cuts[i,j]<-strsplit(gsub("[^[:alnum:] ]", "", cuts[i,j]), " +")[[1]]
    }
  }
  cuts<-cbind(as.numeric(cuts[,1]),as.numeric(cuts[,2]))
  par(mfrow=c(1,2))
  scatter.smooth(dat48$biomass,dat48$HH.sigma.48,col="grey",pch=".",xlab="Biomass",ylab="HH",main="")
  for(i in 1:nrow(cuts)){
    points(mean(dat48$biomass[dat48$biomass>=cuts[i,1] & dat48$biomass<cuts[i,2]]), 
           mean(dat48$HH.sigma.48[dat48$biomass>=cuts[i,1] & dat48$biomass<cuts[i,2]]))
  }
  legend("bottomleft",lty=c(1,NA),pch=c(NA,1),legend=c("Loess Curve","Bin Mean"),bty="n")
  
  scatter.smooth(dat48$biomass,dat48$HV.sigma.48,col="grey",pch=".",xlab="Biomass",ylab="HV",main="")
  for(i in 1:nrow(cuts)){
    points(mean(dat48$biomass[dat48$biomass>=cuts[i,1] & dat48$biomass<cuts[i,2]]), 
           mean(dat48$HV.sigma.48[dat48$biomass>=cuts[i,1] & dat48$biomass<cuts[i,2]]))
  }
  legend("bottomleft",lty=c(1,NA),pch=c(NA,1),legend=c("Loess Curve","Bin Mean"),bty="n")
  mtext("Bins each contain 5% of the data", side=3, line=-3, outer=TRUE, cex=1, font=2)
  
  #Figures showing example of variation in backscatter on a single scndate (1st scndate)
  par(mfrow=c(2,2))
  scatter.smooth(dat48$biomass[dat48$scndate==unique(dat48$scndate)[1]], dat48$HH.sigma.48[dat48$scndate==unique(dat48$scndate)[1]],col="grey",pch=19,cex=0.5,xlab="Biomass (Mg/ha)",ylab="HH (sigma naught)")
  bplot.xy(dat48$biomass[dat48$scndate==unique(dat48$scndate)[1]], dat48$HH.sigma.48[dat48$scndate==unique(dat48$scndate)[1]],N=15,xlab="Biomass (Mg/ha)",ylab="HH (sigma naught)")
    
  scatter.smooth(dat48$biomass[dat48$scndate==unique(dat48$scndate)[1]], dat48$HV.sigma.48[dat48$scndate==unique(dat48$scndate)[1]],col="grey",pch=19,cex=0.5,xlab="Biomass (Mg/ha)",ylab="HV (sigma naught)")
  bplot.xy(dat48$biomass[dat48$scndate==unique(dat48$scndate)[1]], dat48$HV.sigma.48[dat48$scndate==unique(dat48$scndate)[1]],N=15,xlab="Biomass (Mg/ha)",ylab="HV (sigma naught)")
  mtext(unique(dat48$scndate)[1], side=3, line=-3, outer=TRUE, cex=1, font=2)
  
  #Figures showing example of variation in backscatter for a each plot 
  par(new=T, mfrow=c(1,1))
  plot(dat48$scndate, dat48$HH.sigma.48,ylim=c(min(dat48$HH.sigma.48),max(dat48$HH.sigma.48)),xaxt="n",type="n",col=i,ylab="HH (sigma naught)",xlab="")
  for(i in unique(dat48$plot)){
  lines(dat48$scndate[dat48$plot==unique(dat48$plot)[i]], dat48$HH.sigma.48[dat48$plot==unique(dat48$plot)[i]],ylim=c(min(dat48$HH.sigma.48),max(dat48$HH.sigma.48)),xaxt="n",type="b",col=i,ylab="HH (sigma naught)",xlab="")

  par(new=F)
  }    
  lines(tapply(dat48$HH.sigma.48,dat48$scndate,mean),col="black",lwd=3,type="b")
  axis.Date(side = 1, dat48$scndate[dat48$plot==unique(dat48$plot)[i]], format = "%Y-%m",las=2)

  par(mfrow=c(1,1))
  plot(dat48$scndate, dat48$HV.sigma.48,ylim=c(min(dat48$HV.sigma.48),max(dat48$HV.sigma.48)),xaxt="n",type="n",col=i,ylab="HV (sigma naught)",xlab="")
  for(i in unique(dat48$plot)){
    lines(dat48$scndate[dat48$plot==unique(dat48$plot)[i]], dat48$HV.sigma.48[dat48$plot==unique(dat48$plot)[i]],ylim=c(min(dat48$HV.sigma.48),max(dat48$HV.sigma.48)),xaxt="n",type="b",col=i,ylab="HV (sigma naught)",xlab="")
    par(new=F)
  }
  axis.Date(side = 1, dat48$scndate[dat48$plot==unique(dat48$plot)[i]], format = "%Y-%m",las=2)
#   mtext(paste("Plot",unique(dat48$plot)[1],sep=" "), side=3, line=-3, outer=TRUE, cex=1, font=2)

  
  #Figure showing within-plot variation in backscatter values for each scn date
  colors=rainbow(length(unique(dat48$scndate)))
  par(mfrow=c(1,3))
  plot(dat48$biomass,dat48$HH.sigma.48,type="n",xlab="Biomass",ylab="HH")
  for(d in as.character(unique(dat48$scndate))){
    for(p in unique(dat48$plot)){
      #   lines(dat48$biomass[dat48$plot==p],dat48$HH.sigma.48[dat48$plot==p],col="grey")
      x<-dat48$biomass[dat48$plot==p & dat48$scndate==d]
      y<-dat48$HH.sigma.48[dat48$plot==p & dat48$scndate==d]
      points(x,y,pch=19,cex=0.5,col=colors[as.character(unique(dat48$scndate))==d])
      se<-dat48$HHse_data_48m[dat48$plot==p & dat48$scndate==d]
      arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3,col=colors[as.character(unique(dat48$scndate))==d])
    }
  }
  
  plot(dat48$biomass,dat48$HV.sigma.48,type="n",xlab="Biomass",ylab="HV")
  for(d in as.character(unique(dat48$scndate))){
    for(p in unique(dat48$plot)){
      #   lines(dat48$biomass[dat48$plot==p],dat48$HV.sigma.48[dat48$plot==p],col="grey")
      x<-dat48$biomass[dat48$plot==p & dat48$scndate==d]
      y<-dat48$HV.sigma.48[dat48$plot==p & dat48$scndate==d]
      points(x,y,pch=19,cex=0.5,col=colors[as.character(unique(dat48$scndate))==d])
      se<-dat48$HVse_data_48m[dat48$plot==p & dat48$scndate==d]
      arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3,col=colors[as.character(unique(dat48$scndate))==d])
    }
  }
  plot(dat48$biomass,dat48$HV.sigma.48,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  legend("center",pch=19,col=colors,legend=unique(dat48$scndate),bty="n")
  mtext("Between-scene, within-plot variation", side=3, line=-2, outer=TRUE, cex=1, font=2)
  
  #Figure showing temporal variability for each plot in biomass-vs-backscatter space
  par(mfrow=c(1,1))
  plot(dat48$biomass,dat48$HH.sigma.48,pch="",xlab="Biomass",ylab="HH")
  for(p in unique(dat48$plot)){
    lines(dat48$biomass[dat48$plot==p],dat48$HH.sigma.48[dat48$plot==p],col="grey")
    x<-mean(dat48$biomass[dat48$plot==p])
    y<-mean(dat48$HH.sigma.48[dat48$plot==p])
    se<-sd(dat48$HH.sigma.48[dat48$plot==p])/sqrt(length(dat48$HH.sigma.48[dat48$plot==p]))
    arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3)
    points(x,y)
  }
  legend("topright",lty=c(1,1,NA),pch=c(NA,NA,1),col=c("grey","black","black"),legend=c("Range","SE","Mean"),bty="n")
  
  plot(dat48$biomass,dat48$HV.sigma.48,pch="",xlab="Biomass",ylab="HV")
  for(p in unique(dat48$plot)){
    lines(dat48$biomass[dat48$plot==p],dat48$HV.sigma.48[dat48$plot==p],col="grey")
    x<-mean(dat48$biomass[dat48$plot==p])
    y<-mean(dat48$HV.sigma.48[dat48$plot==p])
    se<-sd(dat48$HV.sigma.48[dat48$plot==p])/sqrt(length(dat48$HV.sigma.48[dat48$plot==p]))
    arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3)
    points(x,y)
  }
  legend("topright",lty=c(1,1,NA),pch=c(NA,NA,1),col=c("grey","black","black"),legend=c("Range","SE","Mean"),bty="n")
  mtext("Between-scene variation", side=3, line=-2, outer=TRUE, cex=1, font=2)
  
  #Figure comparing within-plot variation (averaged over all scndates) to between-scene variation of plot means
  par(mfrow=c(1,2))
  plot(dat48$HHse_data_48m,dat48$HHse_data_48m,type="n",ylab="Btwn scene SE of within-plot mean",xlab="Btwn scene mean of within-plot SE",main="HH")
  abline(0,1,lwd=2,lty=2,col="grey")
  for(p in unique(dat48$plot)){
    y<-mean(dat48$HHse_data_48m[dat48$plot==p])
    se<-sd(dat48$HHse_data_48m[dat48$plot==p])/sqrt(length(dat48$HHse_data_48m[dat48$plot==p]))
    
    x<-sd(dat48$HH.sigma.48[dat48$plot==p])/sqrt(length(dat48$HH.sigma.48[dat48$plot==p]))
    #   yse<-sd(dat48$HH.sigma.48[dat48$plot==p])/sqrt(length(dat48$HH.sigma.48[dat48$plot==p]))
    
    points(x,y,pch=19,cex=0.5)
    arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3)
  }
  plot(dat48$HVse_data_48m,dat48$HVse_data_48m,type="n",ylab="Btwn scene SE of within-plot mean",xlab="Btwn scene mean of within-plot SE",main="HV")
  abline(0,1,lwd=2,lty=2,col="grey")
  for(p in unique(dat48$plot)){
    y<-mean(dat48$HVse_data_48m[dat48$plot==p])
    se<-sd(dat48$HVse_data_48m[dat48$plot==p])/sqrt(length(dat48$HVse_data_48m[dat48$plot==p]))
    
    x<-sd(dat48$HV.sigma.48[dat48$plot==p])/sqrt(length(dat48$HV.sigma.48[dat48$plot==p]))
    #   yse<-sd(dat48$HV.sigma.48[dat48$plot==p])/sqrt(length(dat48$HV.sigma.48[dat48$plot==p]))
    
    points(x,y,pch=19,cex=0.5)
    arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3)
  }
  
  #Figure showing across-date plot means with across-plot scndate means 
  colors=rainbow(length(unique(dat48$scndate)))
  par(mfrow=c(1,2))
  plot(tapply(dat48$HH.sigma.48,dat48$plot,mean),xlab="Plot",ylab="Across-scene Mean HH")
  for(d in as.character(unique(dat48$scndate))){
    #   points(tapply(dat48$HH.sigma.48[dat48$scndate==d],dat48$plot[dat48$scndate==d],mean),col=colors[as.character(unique(dat48$scndate))==d])
    abline(h=mean(dat48$HH.sigma.48[dat48$scndate==d]),col=colors[as.character(unique(dat48$scndate))==d])
  }
  plot(tapply(dat48$HV.sigma.48,dat48$plot,mean),xlab="Plot",ylab="Across-scene Mean HV")
  for(d in as.character(unique(dat48$scndate))){
    #   points(tapply(dat48$HV.sigma.48[dat48$scndate==d],dat48$plot[dat48$scndate==d],mean),col=colors[as.character(unique(dat48$scndate))==d])
    abline(h=mean(dat48$HV.sigma.48[dat48$scndate==d]),col=colors[as.character(unique(dat48$scndate))==d])
  }
  
  std.err<-function(x){ sd(x)/sqrt(length(x)) }
  
  #Figure showing between scene and between plot variation
  x<-unique(dat48$scndate)
  y<-tapply(dat48$HH.sigma.48,dat48$scndate,mean)
  se<-tapply(dat48$HH.sigma.48,dat48$scndate,std.err)
  par(mfrow=c(1,2))
  plot(x,y,ylim=c(min(y-se),max(y+se)),xlab="Scndate",ylab="HH") #scndate vs among-plot mean return
  arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3) #std.err bars are among-plot variation for each scndate
  lines(unique(dat48$scndate),tapply(dat48$HH.sigma.48,dat48$scndate,mean))
  abline(h=mean(dat48$HH.sigma.48),col="grey",lwd=2) #mean return value for all scenes on all dates
  mtext("Between-scndate variation", side=3, line=-3, outer=TRUE, cex=1, font=2)
  
  x<-unique(dat48$scndate)
  y<-tapply(dat48$HV.sigma.48,dat48$scndate,mean)
  se<-tapply(dat48$HV.sigma.48,dat48$scndate,std.err)
  plot(x,y,ylim=c(min(y-se),max(y+se)),xlab="Scndate",ylab="HV")
  arrows(x, y-se, x, y+se, length=0.05, angle=90, code=3)
  lines(unique(dat48$scndate),tapply(dat48$HV.sigma.48,dat48$scndate,mean))
  abline(h=mean(dat48$HV.sigma.48),col="grey",lwd=2)
  
  #Figure 
  x<-unique(dat48$plot)
  y<-tapply(dat48$HH.sigma.48,list(dat48$scndate,dat48$plot),mean)
  se<-tapply(dat48$HH.sigma.48,list(dat48$scndate,dat48$plot),std.err)
  plot(dat48$plot,dat48$HH.sigma.48,type="n")
  for(p in unique(dat48$plot)){
    points(p,mean(dat48$HH.sigma.48[dat48$plot==p]))
  }
  abline(h=mean(dat48$HH.sigma.48),col="grey",lwd=2)
  
  
  
  # par(new=FALSE, mfrow=c(1,2))
  # plot(dat48$biomass,dat48$HH.sigma.48,col="grey",pch=".",xlab="Binned Biomass",ylab="Binned HH")
  #   points(bind.bio,bind.HH)
  # plot(dat48$biomass,dat48$HV.sigma.48,col="grey",,pch=".",xlab="Binned Biomass",ylab="Binned HV")
  #   points(bind.bio,bind.HV)
  # points(dat48$biomass[dat48$biomass>=1 & dat48$biomass<=48],dat48$HV.sigma.48[dat48$biomass>=1 &dat48$biomass<=48],pch=".",col="red")
  # mtext("Bins each contain 5% of the data points", side=3, line=-3, outer=TRUE, cex=1, font=2)
  
  # #breaks data into even-length bins
  # bind.bio<-tapply(dat48$biomass,   cut(dat48$biomass,    breaks=seq(0, max(dat48$biomass),     0.05*max(dat48$biomass))),mean)
  # bind.HH<-tapply(dat48$HH.sigma.48,cut(dat48$HH.sigma.48,breaks=seq(0, max(dat48$HH.sigma.48), 0.05*max(dat48$HH.sigma.48))),mean)
  # bind.HV<-tapply(dat48$HV.sigma.48,cut(dat48$HV.sigma.48,breaks=seq(0, max(dat48$HV.sigma.48), 0.05*max(dat48$HV.sigma.48))),mean)
  # par(mfrow=c(1,2))
  # plot(dat48$biomass,dat48$HH.sigma.48,col="grey",pch=".",xlab="Binned Biomass",ylab="Binned HH")
  #   points(bind.bio,bind.HH)
  # plot(dat48$biomass,dat48$HV.sigma.48,col="grey",,pch=".",xlab="Binned Biomass",ylab="Binned HV")
  #   points(bind.bio,bind.HV)
  # mtext("Bins each contain 5% of data range", side=3, line=-3, outer=TRUE, cex=1, font=2)
  
  par(mfrow=c(1,2))
  bplot.xy(dat48$biomass,dat48$HH.sigma.48,N=15,xlab="biomass",ylab="HH (sigma naught)")
  bplot.xy(dat48$biomass,dat48$HV.sigma.48,N=15,xlab="biomass",ylab="HV (sigma naught)")
  
  dev.off()
  
}#end function