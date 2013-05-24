## Code to post-process (summarize and graph) the spectral
## signal of model error between models and fluxtowers
##
## Analysis part of the NACP site-level intercomparison
##
## Michael Dietze, University of Illinois
##

## libraries and functions
library(dplR,lib.loc="~/lib/R") ## Andy Bunn's Dendrochronology package

## directory of spectra
spec.dir <- "/scratch/NACP/spectral/"

## directory to find model files
model.dir <- "NEEm"

## listing of available site files
site.files <- dir(model.dir,"txt")

## Top ten sites
sites <- c(1, ##CA-Ca1
           5, ##CA-Let
           7, ##CA-Mer
           8, ##CA-Oas
           9, ##CA-Obs
           25,##US-Ha1
           26,##US-Ho1
           30,##US-Me2
           38,##US-Ne3
           45 ##US-UMB
           )
site.name <- c("CA-Ca1",
               "CA-Let",
               "CA-Mer",
               "CA-Oas",
               "CA-Obs",
               "US-Ha1",
               "US-Ho1",
               "US-Me2",
               "US-Ne3",
               "US-UMB"
               )

model.set <- c(sort(c("BEPS","CNCLASS","ISOLSM","TECO","ecosys","SiBCASA","SiB","DLEM","ED2","LoTEC_DA","AgroIBIS","DNDC","SiBcrop","can.ibis","EDCM","ORCHIDEE","LPJ","BIOME_BGC","SSiB2","TRIPLEX","EPIC")),"MEAN")
Nmodel <- length(model.set)  ## number of models

#subdaily
#daily
#lotec
#mean
model.col <- rep(1,Nmodel)
model.col[model.set == "MEAN"] <- 'blue'
model.col[model.set == "LoTEC_DA"] <- 'red'
model.col[model.set %in% c("BEPS","BIOME_BGC","DLEM","LPJ")] <- 'green' #daily
model.col[model.set %in% c("AgroIBIS","SiBcrop","SiBCASA")] <- "pink" #low

cross.model <- list()
cross.site  <- list()

for(s in sites){

  if(s == 30) next
  
#  cross.site[[s]] <- list()
  cross.model[[s]] <- list()

  avg.spec <- norm.spec <- geom.spec <- NULL
  mcount <- 0

  load(paste(sub("-","",site.name[which(sites==s)]),".specCI.Rdata",sep=""))
  
#  for(m in 1:Nmodel){
 m = 22 
    print(c(s,m))

    if(s %in% c(1,7,8,26)){
      load(paste(spec.dir,"/NACPspecNORMpre2v2.",s,".",m,".Rdata",sep=""))
    }
    ##if(s %in% c(9,38,45)){
    if(s %in% c(9)){
        load(paste(spec.dir,"/NACPspecNORMpre2v2_17.",s,".",m,".Rdata",sep=""))
      }
    if(s %in% c(5,25,38,45) | m == 22){
      load(paste(spec.dir,"/NACPspecNORMpre2.clip.",s,".",m,".Rdata",sep=""))
    }

    period <- wv$period/day

    if(length(period) != length(Period)) {
      print(c("PERIOD MISSMATCH",length(period),length(Period)))
      next
    }
    
## to calculate:
##  - global spectra for each model, graph multi-site and median
##  - composite spectra for each site, avg across models
##  - relative power in different bands:
##      - overall summary
##      - stat model of site & model effects


    if(sum(Power,na.rm=TRUE)>0){  ### if model was run for site
    

      mcount <- mcount + 1
      
      ## divide up spectra
      Pglobe <- apply(Power,2,sum,na.rm=TRUE)
      Pnorm  <- Pglobe/pCI[3,]

      if(FALSE){
        plot(period,Pglobe,log="xy",main='US-Ho1  ED2',ylim=c(1,max(Pglobe)),ylab="Power",type='l',lwd=2,cex.lab=1.5,cex.main=1.5,cex.axis=1.2)
        lines(Period,pCI[3,],col=2,lty=2)
        lines(Period,pCI[2,],col=2)
        lines(Period,pCI[1,],col=2,lty=2)
        
##        plot(period,Pglobe/pCI[3,],log='x')
        plot(period,Pglobe/pCI[3,],log='xy',ylab="Power/(Null 95%)",type='l',lwd=2,cex.lab=1.5,cex.main=1.5,cex.axis=1.2,main='US-Ho1  ED2')
        abline(v=c(1,365.25),lty=3,col=6,lwd=2)
        abline(h=1,lty=2,lwd=2)
      }
      
      day.mid <- findInterval(1,period)
      day.bin <- day.mid + c(-4:4)
      ##    abline(v=period[day.bin])
      
      year.mid <- findInterval(365.25,period)
      year.bin <- year.mid + c(-4:4)
      ##    abline(v=period[year.bin])
    
      synop.bin <- (max(day.bin)+1):(min(year.bin)-1)
      subday.bin <- 1:(min(day.bin)-1)
      inter.bin <- (max(year.bin)+1):length(period)
      if(length(period) <= max(year.bin)) inter.bin <- NA
      
      pow.bin <- c(sum(Pglobe[subday.bin]),sum(Pglobe[day.bin]),sum(Pglobe[synop.bin]),sum(Pglobe[year.bin]),sum(Pglobe[inter.bin]))
      pow.bin <- pow.bin/sum(pow.bin)

      pow.binN <- c(sum(Pnorm[subday.bin]),sum(Pnorm[day.bin]),sum(Pnorm[synop.bin]),sum(Pnorm[year.bin]),sum(Pnorm[inter.bin]))
##      pow.binN <- pow.binN/sum(pow.binN)

      
      if(is.null(avg.spec)){
        avg.spec <- Power
        norm.spec <- Power/sum(as.vector(Power),na.rm=TRUE)
        geom.spec <- log(Power)
      } else {
        avg.spec <- avg.spec + Power
        norm.spec <- norm.spec + Power/sum(as.vector(Power),na.rm=TRUE)
        geom.spec <- geom.spec + log(Power)
      }

    
      cross.model[[s]][[m]] <- list(Pglobe=Pglobe,pow.bin=pow.bin,Pnorm=Pnorm,pow.binN=pow.binN,period=period,day=day)
      
    } else { ## end if model run
      cross.model[[s]][[m]] <- NULL
    }
  } ## end loop over models

  
  
  cross.site[[s]] <- list(avg=avg.spec/mcount,norm=norm.spec/mcount,geom=exp(geom.spec/mcount),period=period,day=day)

  save(cross.site,cross.model,file="CrossSpec.v6.Rdata")
}

load("CrossSpec.v6.Rdata")

##### view global spectra by model:
###########################################################################
sitesel <- 1:10
sitesel <- c(1:7,9,10)
pdf("byModel.v5.pdf",width=6.5,height=11)
par(mfrow=c(2,1))
ModelMean <- list()
maxP   <- 0
maxPN   <- 0
for(m in 1:Nmodel){
  for(i in sitesel){
    s <- sites[i]
    if(m < length(cross.model[[s]]) && !is.null(cross.model[[s]][[m]])){
      if(max(cross.model[[s]][[m]]$Pglobe) > maxP) maxP <- max(cross.model[[s]][[m]]$Pglobe)
      if(max(cross.model[[s]][[m]]$Pnorm) > maxPN) maxPN <- max(cross.model[[s]][[m]]$Pnorm)
    }
  }
}
  
for(m in 1:Nmodel){
  
  ## calc ranges
  maxlen <- 0
  nsite <- 0
  periods <- matrix(NA,length(sites),100)
  for(i in sitesel){
    s <- sites[i]
    if(m <= length(cross.model[[s]]) && !is.null(cross.model[[s]][[m]])){
      nsite <- nsite + 1
      periods[i,1:length(cross.site[[s]]$period)] <- cross.site[[s]]$period
    }
  }
 
  
  Period <- sort(unique(as.vector(periods)))
  sel <- c(1,1+which(diff(log(Period))>0.001))
  Period <- Period[sel]
  maxlen <- length(Period)
  if(nsite > 0){
    ## fill in overall power matrix for each site
    ## need to align power to period for each site
    ## to account for different meas. frequency
    ## and interpolate missing values  
    Power <- matrix(NA,length(sites),maxlen)
    PowerNorm <- matrix(NA,length(sites),maxlen)
    for(i in sitesel){
      s <- sites[i]
      first <- NA
      ## fill in values
      if(m <= length(cross.model[[s]]) && !is.null(cross.model[[s]][[m]])){
        ##      print(s)
        for(j in 1:length(cross.model[[s]][[m]]$period)){
          pmatch <- which.min(abs(log(Period) - log(cross.model[[s]][[m]]$period[j])))
          Power[i,pmatch] <- cross.model[[s]][[m]]$Pglobe[j]
          PowerNorm[i,pmatch] <- cross.model[[s]][[m]]$Pnorm[j]
          if(is.na(first)) first <- pmatch
        }
        ## interpolate
        for(j in first:pmatch){
          if(is.na(Power[i,j])) Power[i,j] <- exp((log(Power[i,j-1])+log(Power[i,j+1]))/2)
          if(is.na(PowerNorm[i,j])) PowerNorm[i,j] <- exp((log(PowerNorm[i,j-1])+log(PowerNorm[i,j+1]))/2)
        }
      }
    }
    
    ## calc means
    P.cnt <- apply(!is.na(Power),2,sum)
    P.bar <- apply(Power,2,mean,na.rm=TRUE)
    P.geom <- exp(apply(log(Power),2,mean,na.rm=TRUE))
    Psum <- apply(Power,1,sum,na.rm=TRUE)
    P.norm <- apply(Power/Psum,2,mean,na.rm=TRUE)
    P.gnorm <- exp(apply(log(Power/Psum),2,mean,na.rm=TRUE))
    P.med  <- apply(Power,2,median,na.rm=TRUE)
    P.norm.med  <- apply(Power/Psum,2,median,na.rm=TRUE)

    PN.cnt <- apply(!is.na(PowerNorm),2,sum)
    PN.bar <- apply(PowerNorm,2,mean,na.rm=TRUE)
    PN.geom <- exp(apply(log(PowerNorm),2,mean,na.rm=TRUE))
    PNsum <- apply(PowerNorm,1,sum,na.rm=TRUE)
    PN.norm <- apply(PowerNorm/Psum,2,mean,na.rm=TRUE)
    PN.gnorm <- exp(apply(log(PowerNorm/Psum),2,mean,na.rm=TRUE))
    PN.med  <- apply(PowerNorm,2,median,na.rm=TRUE)
    PN.norm.med  <- apply(PowerNorm/Psum,2,median,na.rm=TRUE)

    
    
    day.mid <- findInterval(1,Period)
    day.bin <- day.mid + c(-4:4)  
    year.mid <- findInterval(365.25,Period)
    year.bin <- year.mid + c(-4:4)
    breaks <- c(range(day.bin),range(year.bin))  

    ModelMean[[m]] <- list(Period=Period,P.norm.med=P.norm.med,PN.med=PN.med)
    
if(FALSE){
    
### Raw power - arithmetic mean, geometirc mean, and median
    plot(Period,P.bar,ylim=c(1,maxP),log='xy',type='n',ylab="Power",main=paste("RAW",model.set[m]),xlab="period (days)")
    for(i in 1:length(sites)){
                                        #    lines(cross.model[[s]][[m]]$Pglobe)
      lines(Period,Power[i,])
    }
    lines(Period,P.bar,ylim=c(1,maxP),type='l',lwd=3,col=2)
    lines(Period,P.geom,ylim=c(1,maxP),type='l',lwd=3,col=3)
    lines(Period,P.med,ylim=c(1,maxP),type='l',lwd=3,col=4)
    abline(v=Period[breaks])
}

    colseq = c(8,5,7,6,8,6,8,1,5,6)
    ltyseq = c(4,3,2,2,2,3,3,1,2,2)
### Normalized Spectra
    plot(Period,P.norm,ylim=range(1e-7,1),log='xy',type='n',ylab="Power",main=paste("NORMALISED",model.set[m]),xlab="period (days)")
    for(i in 1:length(sites)){
                                        #    lines(cross.model[[s]][[m]]$Pglobe)
      lines(Period,Power[i,]/Psum[i],col=colseq[i],lty=ltyseq[i])
    }
    lines(Period,P.norm,ylim=c(1,maxP),type='l',lwd=3,col=2)
    lines(Period,P.gnorm,ylim=c(1,maxP),type='l',lwd=3,col=3)
    lines(Period,P.norm.med,ylim=c(1,maxP),type='l',lwd=3,col=4)
    abline(v=Period[breaks])

    ### CI Raw power - arithmetic mean, geometirc mean, and median
    plot(Period,PN.bar,ylim=c(.1,maxPN),log='xy',type='n',ylab="Power/(95% NULL)",main=paste("TEST RAW",model.set[m]),xlab="period (days)")
    for(i in 1:length(sites)){
                                        #    lines(cross.model[[s]][[m]]$Pglobe)
      lines(Period,PowerNorm[i,],col=colseq[i],lty=ltyseq[i])
    }
    lines(Period,PN.bar,ylim=c(1,maxP),type='l',lwd=3,col=2)
    lines(Period,PN.geom,ylim=c(1,maxP),type='l',lwd=3,col=3)
    lines(Period,PN.med,ylim=c(1,maxP),type='l',lwd=3,col=4)
    abline(v=Period[breaks],lty=3,col=6)
    abline(h=1,lty=2)

    if(FALSE){  ## Nothing new learned here and loose signif. threshold
### CI Normalized Spectra
      plot(Period,PN.norm,ylim=range(1e-7,1),log='xy',type='n',ylab="Power",main=paste("NORMALISED",model.set[m]),xlab="period (days)")
      for(i in 1:length(sites)){
                                        #    lines(cross.model[[s]][[m]]$Pglobe)
        lines(Period,PowerNorm[i,]/PNsum[i])
      }
      lines(Period,PN.norm,ylim=c(1,maxP),type='l',lwd=3,col=2)
      lines(Period,PN.gnorm,ylim=c(1,maxP),type='l',lwd=3,col=3)
      lines(Period,PN.norm.med,ylim=c(1,maxP),type='l',lwd=3,col=4)
      abline(v=Period[breaks])
    }


    
  }
}  ### end BY MODEL

##pdf("AllModelSpec.pdf",width=8.5,height=7)
#par(mfrow=c(2,1))
par(lwd=2,cex=1.5)
### Normalized Spectra
i <- 9
plot(ModelMean[[i]]$Period,ModelMean[[i]]$P.norm.med,ylim=range(1e-4,1),log='xy',type='n',ylab="Power",main=paste("ALL MODELS - NORMALISED"),xlab="Time-scale (days)")
for(i in 1:length(ModelMean)){
  lines(ModelMean[[i]]$Period,ModelMean[[i]]$P.norm.med,col=model.col[i],lwd=2*(2-(model.col[i]==1)))
}
#abline(v=c(1,365.25),lty=3,col=6)
abline(v=Period[breaks],col=6,lty=3,lwd=2)
### CI Raw power
i<-9
plot(ModelMean[[i]]$Period,ModelMean[[i]]$PN.med,ylim=c(.3,1200),log='xy',type='n',ylab="Power/(95% NULL)",main=paste("ALL MODELS - SIGNIFICANCE"),xlab="Time-scale (days)")#,cex.axis=0.8)
for(i in 1:length(ModelMean)){
  lines(ModelMean[[i]]$Period,ModelMean[[i]]$PN.med,col=model.col[i],lwd=2*(2-(model.col[i]==1)))
}
abline(h=1,lwd=2,lty=2)
#abline(v=c(1,365.25),col=6,lty=3)
abline(v=Period[breaks],col=6,lty=3,lwd=2)
legend("topleft",legend=c("Ensemble Mean","LoTEC-DA","Daily timestep","Low Syn. Error"),col=c("blue","red","green","pink"),lwd=5,bg='white',cex=0.8)
dev.off()  


#### Composite spectra by site
############################################################################
.libPaths("~/lib/R")
library(fields)

sitesel = c(4,6,10,1,7,5,9,2,3)
ystart = c(1998,1999,1999,1997,2000,1992,1996,NA,2002,1999) 
jpeg("bySite.v5b.NORM.jpg",width=19.5,height=12,units="in",res=300)
par(mfrow=c(3,3))
par(lwd=2,mar=c(4,4,4,1))

lev <- list()

for(i in sitesel){
  print(i)
  s <- sites[i]
  nbreak <- 10

if(FALSE){
  ## load site data table
  dat <- read.table(paste(model.dir,site.files[s],sep="/"),
                    header=TRUE,
                    na.string="-999.000"
                    )
  ## calc daily mean NEE
  NEEfd  = tapply(dat$NEE_FILLED,floor(dat$X.YEAR*1000+dat$FDOY),mean,na.rm=TRUE)
  YEARfd = tapply(dat$X.YEAR,floor(dat$X.YEAR*1000+dat$FDOY),mean,na.rm=TRUE)
  FDOYfd = tapply(dat$FDOY,floor(dat$X.YEAR*1000+dat$FDOY),mean,na.rm=TRUE)
  TIMEfd = YEARfd + FDOYfd/365.25
  years  = sort(unique(dat$X.YEAR))
  
  ## ID growing season
  win = 10
  NEEfdf = filter(NEEfd,rep(1/win,win))  ## smooth
  phenol = matrix(NA,length(years),2)
  cnt = 0
  phenVec = NA
  for(y in 1:length(years)){
    sel = which(YEARfd == years[y])

    phenol[y,1] <- which.min(sign(NEEfdf[sel]))
    cnt = cnt + 1
    phenVec[cnt] <- years[y] + phenol[y,1]/365.25
    
    phenol[y,2] <- length(sel) - which.min(sign(NEEfdf[rev(sel)]))
    cnt = cnt + 1
    phenVec[cnt] <- years[y] + phenol[y,2]/365.25

  }
}

if(FALSE){
  plot(TIMEfd,NEEfd,pch=".")
  lines(TIMEfd,NEEfdf)
  abline(v=phenVec,col=2)
  abline(h=0,col=3)
}

## alt calc based on GPP
sname = site.name[which(sites==s)]
sname = paste(substr(sname,1,2),substr(sname,4,6),sep="")
prefix <- paste("MDSNEE_",sname,"-",sep="")
load(paste(prefix,".field.Rdata",sep="")) ##fdat,fylen,yrs,ysel
GPP  = fdat[,8]
GPP[GPP < -1000] <- NA
GPP[GPP < 0] = 0
GPPd  = tapply(GPP,fdat[,1]*1000+fdat[,2],mean,na.rm=TRUE)
YEARd = tapply(fdat[,1],fdat[,1]*1000+fdat[,2],mean,na.rm=TRUE)
DOYd  = tapply(fdat[,2],fdat[,1]*1000+fdat[,2],mean,na.rm=TRUE)
TIMEd = YEARd + DOYd/365.25
years  = sort(unique(YEARd))
  
  ## ID growing season
  win = 10
  GPPdf = filter(GPPd,rep(1/win,win))  ## smooth
  phenol = matrix(NA,length(years),2)
  cnt = 0
  phenVec2 = NA
  for(y in 1:length(years)){
    sel = which(YEARd == years[y])

    phenol[y,1] <- which.max(sign(GPPdf[sel]-max(GPPdf[sel],na.rm=TRUE)*.2))
    cnt = cnt + 1
    phenVec2[cnt] <- years[y] + phenol[y,1]/365.25
    
    phenol[y,2] <- length(sel) - which.max(sign(GPPdf[rev(sel)]-max(GPPdf[sel],na.rm=TRUE)*.2))
    cnt = cnt + 1
    phenVec2[cnt] <- years[y] + phenol[y,2]/365.25

  }

if(FALSE){
#  plot(TIMEd,GPPd,pch=".")
#  lines(TIMEd,GPPdf)
  plot(TIMEfd,-NEEfd,pch=".")
  lines(TIMEfd,-NEEfdf)
  abline(v=phenVec,col=2)
  abline(v=phenVec2,col=3)
  abline(h=0,col=3)
}


  if(FALSE){
  jpeg(paste("bySite.v2.",s,".AVG.jpg",sep=""),width=6.5,height=4,units="in",res=300)
#  pdf(paste("bySite.v2.",s,".pdf",sep=""),width=6.5,height=11)
#  par(mfrow=c(3,1))
      

  ##  spec.bar <- spec.gmean <- spec.norm <- spec.gnorm <- matrix(0,
  ##    cross.site[[s]] <- list(avg=avg.spec,norm=norm.spec,geom=exp(geom.spec),period=period,day=day)
  mylevels <- quantile(cross.site[[s]]$avg,seq(0,1,length=(nbreak +1)),na.rm=TRUE)
  image.plot((1:nrow(cross.site[[s]]$avg))/cross.site[[s]]$day,cross.site[[s]]$period,
             cross.site[[s]]$avg,
             ylab="Period (days)",log='y',
             main=paste(site.name[i],"AVG"),
             breaks=mylevels,#lab.breaks=formatC(mylevels,digits = 2),
             axis.args=list(at=log(mylevels),labels=formatC(mylevels,digits = 2)),
             nlevel=nbreak,
             #breaks=exp(seq(min(log(cross.site[[s]]$avg),na.rm=TRUE),
             #  max(log(cross.site[[s]]$avg),na.rm=TRUE),length=65))
             xlab="Time (days)"
             )
  dev.off()
  jpeg(paste("bySite.v2.",s,".GEOM.jpg",sep=""),width=6.5,height=4,units="in",res=300)
  mylevels <- quantile(cross.site[[s]]$geom,seq(0,1,length=(nbreak +1)),na.rm=TRUE)
  image.plot((1:nrow(cross.site[[s]]$geom))/cross.site[[s]]$day,cross.site[[s]]$period,
             cross.site[[s]]$geom,
             ylab="Period (days)",log='y',
             main=paste(site.name[i],"GEOM"),
             breaks=mylevels,#lab.breaks=formatC(mylevels,digits = 2),
             axis.args=list(at=log(mylevels),labels=formatC(mylevels,digits = 2)),
             nlevel=nbreak,xlab="Time (days)"
             #breaks=exp(seq(min(log(cross.site[[s]]$geom),na.rm=TRUE),
             #  max(log(cross.site[[s]]$geom),na.rm=TRUE),length=65))
             )

  dev.off()
  jpeg(paste("bySite.v2.",s,".NORM.jpg",sep=""),width=6.5,height=4,units="in",res=300)
}
#  mylevels <- quantile(cross.site[[s]]$norm,seq(0,1,length=(nbreak +1)),na.rm=TRUE)
  mylevels <- lev3
#  lev[[i]] = mylevels
  time = (1:nrow(cross.site[[s]]$norm))/cross.site[[s]]$day/365.25 + ystart[i]
  image.plot(time,cross.site[[s]]$period,
             cross.site[[s]]$norm,
             ylab="Time-scale (days)",log='y',
             main=site.name[i],
             breaks=mylevels,#lab.breaks=formatC(mylevels,digits = 2),
             axis.args=list(at=log(mylevels),labels=formatC(log10(mylevels),digits = 2)),
             nlevel=length(mylevels)-1,
             #breaks=exp(seq(min(log(cross.site[[s]]$norm),na.rm=TRUE),
             #  max(log(cross.site[[s]]$norm),na.rm=TRUE),length=65))
             xlab="Time (year)",
             cex.lab=1.4,
             cex.axis=1.2,
             cex.main=1.75
             )
  abline(h=Period[breaks],col=1,lty=1,lwd=2)
  abline(v=phenVec2,col=1,lty=1,lwd=2)
#  dev.off()
}  ## died on #5, #9 & #10
dev.off()

lev2 = matrix(NA,10,11)
for(i in sitesel){lev2[i,] = lev[[i]]}
lev = exp(apply(log(lev2),2,mean,na.rm=TRUE))
lev3 = 10^rev(c(-4,seq(-6,-9,length=7),-24))
lcol = tim.colors(length(lev3)-1)
plot(0,0,type='n',xlim=c(0,3),ylim=c(0,13))
for(i in 1:(length(lev3)-1)){
  rect(1,i,5/4,i+1,col=lcol[i])
}
text(1.3,1:length(lev3),formatC(log10(lev3),digits=2),pos=4,cex=1.2,font=2)
lev3[length(lev3)] = 10^-3.5

#########################################################################

pdf("bars.v4.pdf",width=10,height=7.5)
### ANOVA style analysis of power
ptable <- matrix(NA,Nmodel*9,7)
mrun   <- matrix(NA,10,Nmodel)
i <- 0
for(s in c(1,5,7,8,9,25,26,38,45)){
  for(m in 1:length(cross.model[[s]])){
    if(!is.null(cross.model[[s]][[m]])){
      mrun[which(sites == s),m] = 1
      i <- i+1
      ptable[i,1] <- s
      ptable[i,2] <- m
      tmp <- cross.model[[s]][[m]]$pow.binN
      tmp[is.na(tmp)] = 0
      ptable[i,3:7] <- tmp

    }
  }
}
ptable <- ptable[1:(i+1),]
colnames(ptable) <- c("site","model","sub","day","int","year","inter")

ssub <- tapply(ptable[,3],ptable[,1],mean)
sday <- tapply(ptable[,4],ptable[,1],mean)
smid <- tapply(ptable[,5],ptable[,1],mean)
syear <- tapply(ptable[,6],ptable[,1],mean)
sinter <- tapply(ptable[,7],ptable[,1],mean)
sbar = rbind(ssub,sday,smid,syear,sinter)

barplot(sbar,
        col=1:5,names.arg=site.name[sitesel])#,
##        legend.text=c("subdaily","diurnal",
##          "intermediate","annual","interannual")
##        )

par(las=3,mar=c(7,4,4,2))
barplot(sbar,
        col=1:5,names.arg=site.name[sitesel],
        legend.text=c("subdaily","diurnal",
          "intermediate","annual","interannual"),cex.names=1.5,cex.axis=1.2
)

sbarhat = apply(sbar,2,sum)
sbar2 = sbar
for(i in 1:ncol(sbar)){
  sbar2[,i] = sbar2[,i]/sbarhat[i]
}
par(las=3,mar=c(7,4,4,2))
#barplot(sbar2,
ss2 = c(4,6,9,1,7,5,8,2,3)
barplot(sbar2[,ss2],
        col=1:5,names.arg=site.name[sitesel],
#        legend.text=c("subdaily","diurnal",
#          "intermediate","annual","interannual")
        ,cex.names=1.5,cex.axis=1.2
)


msub <- tapply(ptable[,3],ptable[,2],mean)
mday <- tapply(ptable[,4],ptable[,2],mean)
mmid <- tapply(ptable[,5],ptable[,2],mean)
myear <- tapply(ptable[,6],ptable[,2],mean)
minter <- tapply(ptable[,7],ptable[,2],mean)
mbar <- rbind(msub,mday,mmid,myear,minter)
colnames(mbar) <- model.set[as.numeric(colnames(mbar))]

par(las=3,mar=c(7,4,4,2))
bp <- barplot(mbar,
        col=1:5,space=0.1,)#,
##        legend.text=c("subdaily","diurnal",
##          "intermediate","annual","interannual")
##        )
##text(bp,-2,model.set,pos=2,srt=45)

par(las=3,mar=c(7,4,4,2))
bp <- barplot(mbar,
              col=1:5,space=0.1,
              legend.text=c("subdaily","diurnal",
                "intermediate","annual","interannual"),
              cex.axis=1.2,cex.names=1.2
        )
##text(bp,-2,model.set,pos=2,srt=45)

mbarhat = apply(mbar,2,sum)
mbar2 = mbar
for(i in 1:ncol(mbar)){
  mbar2[,i] = mbar2[,i]/mbarhat[i]
}

par(las=3,mar=c(7,4,4,2))
bp <- barplot(mbar2,
              col=1:5,space=0.1,
#              legend.text=c("subdaily","diurnal",
#                "intermediate","annual","interannual"),
              cex.axis=1.2,cex.names=1.2
        )
##text(bp,-2,model.set,pos=2,srt=45)
dev.off()

#########  ANOVA
ptable <- as.data.frame(ptable)
summary(aov(sub ~ site + model + site*model,ptable))
summary(aov(day ~ site + model + site*model,ptable))
summary(aov(int ~ site + model + site*model,ptable))
summary(aov(year ~ site + model + site*model,ptable))
summary(aov(inter ~ site + model + site*model,ptable))

##summary(manova(ptable[,3:7] ~ site + model + site*model,ptable))

######  Normalize so each row sums to 1
ptable2 <- ptable[,3:7]
ptablenorm <- apply(ptable2,1,sum,na.rm=TRUE)
for(i in 1:5){
  ptable2[,i] = ptable2[,i]/ptablenorm
}

pvec = cbind(ptable[,1],ptable[,2],rep(1,nrow(ptable)),ptable[,3])
for(j in 2:5){
	pvec = rbind(pvec,cbind(ptable[,1],ptable[,2],rep(j,nrow(ptable)),ptable[,j+2]))
}
colnames(pvec) <- c("site","model","band","val")
pvec = as.data.frame(pvec)

pvec2 = cbind(ptable[,1],ptable[,2],rep(1,nrow(ptable)),ptable2[,1])
for(j in 2:5){
	pvec2 = rbind(pvec2,cbind(ptable[,1],ptable[,2],rep(j,nrow(ptable)),ptable2[,j]))
}
colnames(pvec2) <- c("site","model","band","val")
pvec2 = as.data.frame(pvec2)

if(FALSE){
## is large fraction of IAV at CA1 due to hi IAV or low annual
iav = which(pvec$band == 5)
iavBySite <- tapply(pvec$val[iav],pvec$site[iav],mean,na.rm=TRUE)
iavBySite[4]/mean(iavBySite[-4])

ann = which(pvec$band == 4)
annBySite <- tapply(pvec$val[ann],pvec$site[ann],mean,na.rm=TRUE)
annBySite[4]/mean(annBySite[-4])

rBySite <- tapply(pvec$val[iav]/pvec$val[ann],pvec$site[ann],mean,na.rm=TRUE)

}


summary(aov(val ~ site + model + band + site*model,pvec))
summary(aov(val ~ site + model + band + site*model + site*band + model*band,pvec))
summary(aov(val ~ site + model + band + site*model + site*band + model*band + site*model*band,pvec))

summary(aov(val ~ site + model + band + site*model,pvec2))
summary(aov(val ~ site + model + band + site*model + site*band + model*band,pvec2))
summary(aov(val ~ as.factor(site) + as.factor(model) + as.factor(band) + as.factor(site)*as.factor(model) + as.factor(site)*as.factor(band) + as.factor(model)*as.factor(band) + as.factor(site)*as.factor(model)*as.factor(band),pvec2))

a2 = aov(val ~ as.factor(site) + as.factor(model) + as.factor(band) + as.factor(site)*as.factor(model) + as.factor(site)*as.factor(band) + as.factor(model)*as.factor(band),pvec2)

pvec3 <- pvec2[pvec2$band < 5,]
a3 <- aov(val ~ as.factor(site) + as.factor(model) + as.factor(band) + as.factor(site)*as.factor(model) + as.factor(site)*as.factor(band) + as.factor(model)*as.factor(band),pvec3)

summary(a2)
summary(a3)

plot(residuals(a3))
hist(residuals(a3))


### ANOVA for just complete forest sites:
forest.sites = c(1,8,9,25,26,45)
forest.models = which(apply(!is.na(mrun[which(sites %in% forest.sites),]),2,sum) == 6)
forest.models = forest.models[-which(model.set[forest.models] == "MEAN")]
ptable3 = ptable[ptable$site %in% forest.sites,]
ptable3 = ptable3[ptable3$model %in% forest.models,]
ptable4 <- ptable3[,3:7]
ptablenorm4 <- apply(ptable4,1,sum,na.rm=TRUE)
for(i in 1:5){
  ptable4[,i] = ptable4[,i]/ptablenorm4
}
pvec4 = cbind(ptable3[,1],ptable3[,2],rep(1,nrow(ptable3)),ptable4[,1])
for(j in 2:5){
	pvec4 = rbind(pvec4,cbind(ptable3[,1],ptable3[,2],rep(j,nrow(ptable3)),ptable4[,j]))
}
colnames(pvec4) <- c("site","model","band","val")
pvec4 = as.data.frame(pvec4)
pvec5 <- pvec4[pvec4$band < 5,]
a5 <- aov(val ~ as.factor(site) + as.factor(model) + as.factor(band) + as.factor(site)*as.factor(model) + as.factor(site)*as.factor(band) + as.factor(model)*as.factor(band),pvec5)
summary(a5)

biome <- pvec5$site
biome[biome %in% c(1,8,26)]  <- 'E'
biome[biome %in% c(9,25,45)] <- 'D'

a6 <- aov(val ~  + as.factor(biome) + as.factor(model) + as.factor(band) + as.factor(biome)*as.factor(model) + as.factor(biome)*as.factor(band) + as.factor(model)*as.factor(band),pvec5)
summary(a6)


## total count table
pres = matrix(NA,21,9)
for(j in 1:9){
  for(i in 1:length(cross.model[[sites[sitesel[j]]]])){
    pres[i,j] = length(cross.model[[sites[sitesel[j]]]][[i]])
  }
}
rownames(pres) <- model.set
colnames(pres) <- site.name[sitesel]
t(pres[,ord])

### Model covariates
mcov <- read.csv("ModelCov.csv")
sub.sel <- which(mcov$step == 'sub')
int.bin <- ptable[ptable[,2] %in% sub.sel,c(1,2,5)]
int.bin <- as.data.frame(int.bin)
int.bin <- cbind(int.bin,mcov[int.bin[,2],3],mcov[int.bin[,2],4],mcov[int.bin[,2],6],mcov[int.bin[,2],7],mcov[int.bin[,2],8])
int.bin <- int.bin[-which(int.bin$model == 13),]
colnames(int.bin)[4:8] <- c("Phen","GPP","Nit","Spool","Slayer")
spool = int.bin$Spool; spool[spool > 1] = 2
#a7 <- aov(int ~   as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+Spool + Slayer,data=int.bin)
a7 <- aov(int ~   as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(Spool>1) + I(Slayer>1),data=int.bin)
a7 <- aov(int ~   I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ as.factor(spool) ,data=int.bin)
a7 <- aov(int ~   I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(spool>1) ,data=int.bin)
summary(a7)
int.bin.f <- int.bin[int.bin$site %in% forest.sites,]
int.bin.f <- int.bin.f[int.bin.f$model %in% forest.models,]
spoolf = int.bin.f$Spool; spoolf[spoolf > 1] = 2
a8 <- aov(int ~  as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(Spool>1) + I(Slayer>1),data=int.bin.f)
a8 <- aov(int ~  I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ as.factor(spoolf),data=int.bin.f)
a8 <- aov(int ~  I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(spoolf>1),data=int.bin.f)
summary(a8)
tapply(int.bin$int,int.bin$Nit,mean)
tapply(int.bin$int,int.bin$Slayer>1,mean)
tapply(int.bin.f$int,int.bin.f$Slayer>1,mean)
tapply(int.bin$int,int.bin$Spool>1,mean)
tapply(int.bin$int,int.bin$Spool,mean)
tapply(int.bin$int,spool,mean)

## repeat for annual time scale
year.bin <- ptable[ptable[,2] %in% sub.sel,c(1,2,6)]
year.bin <- as.data.frame(year.bin)
year.bin <- cbind(year.bin,mcov[year.bin[,2],3],mcov[year.bin[,2],4],mcov[year.bin[,2],6],mcov[year.bin[,2],7],mcov[year.bin[,2],8])
year.bin <- year.bin[-which(year.bin$model == 13),]
colnames(year.bin)[4:8] <- c("Phen","GPP","Nit","Spool","Slayer")
spool = year.bin$Spool; spool[spool > 1] = 2
#a7 <- aov(int ~   as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+Spool + Slayer,data=year.bin)
a9 <- aov(int ~   as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(Spool>1) + I(Slayer>1),data=year.bin)
a9 <- aov(year ~   I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ as.factor(spool) ,data=year.bin)
a9 <- aov(year ~   I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(spool>1) ,data=year.bin)
summary(a9)
year.bin.f <- year.bin[year.bin$site %in% forest.sites,]
year.bin.f <- year.bin.f[year.bin.f$model %in% forest.models,]
spoolf = year.bin.f$Spool; spoolf[spoolf > 1] = 2
a10 <- aov(int ~  as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(Spool>1) + I(Slayer>1),data=year.bin.f)
a10 <- aov(year ~  I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ as.factor(spoolf),data=year.bin.f)
a10 <- aov(year ~  I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(spoolf>1),data=year.bin.f)
summary(a10)
tapply(year.bin$year,year.bin$Nit,mean)
tapply(year.bin$year,year.bin$Slayer>1,mean)
tapply(year.bin.f$year,year.bin.f$Slayer>1,mean)
tapply(year.bin$year,year.bin$Spool>1,mean)
tapply(year.bin$year,year.bin$Spool,mean)
tapply(year.bin$year,spool,mean)
tapply(year.bin$year,year.bin$GPP,mean,na.rm=TRUE)
tapply(year.bin$year,year.bin$Phen,mean,na.rm=TRUE)

## repeat for daily time scale
day.bin <- ptable[ptable[,2] %in% sub.sel,c(1,2,4)]
day.bin <- as.data.frame(day.bin)
day.bin <- cbind(day.bin,mcov[day.bin[,2],3],mcov[day.bin[,2],4],mcov[day.bin[,2],6],mcov[day.bin[,2],7],mcov[day.bin[,2],8])
day.bin <- day.bin[-which(day.bin$model == 13),]
colnames(day.bin)[4:8] <- c("Phen","GPP","Nit","Spool","Slayer")
spool = day.bin$Spool; spool[spool > 1] = 2
#a7 <- aov(int ~   as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+Spool + Slayer,data=day.bin)
#a9 <- aov(int ~   as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(Spool>1) + I(Slayer>1),data=day.bin)
a11 <- aov(day ~   I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ as.factor(spool) ,data=day.bin)
a11 <- aov(day ~   I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(spool>1) ,data=day.bin)
summary(a11)
day.bin.f <- day.bin[day.bin$site %in% forest.sites,]
day.bin.f <- day.bin.f[day.bin.f$model %in% forest.models,]
spoolf = day.bin.f$Spool; spoolf[spoolf > 1] = 2
#a10 <- aov(int ~  as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(Spool>1) + I(Slayer>1),data=day.bin.f)
a12 <- aov(day ~  I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ as.factor(spoolf),data=day.bin.f)
a12 <- aov(day ~  I(Slayer>1) + as.factor(site)+ as.factor(Phen) + as.factor(GPP) + as.factor(Nit)+ I(spoolf>1),data=day.bin.f)
summary(a12)
tapply(day.bin$day,day.bin$Nit,mean)
tapply(day.bin$day,day.bin$Slayer>1,mean)
tapply(day.bin.f$day,day.bin.f$Slayer>1,mean)
tapply(day.bin$day,day.bin$Spool>1,mean)
tapply(day.bin.f$day,day.bin.f$Spool>1,mean)
tapply(day.bin$day,day.bin$Spool,mean)
tapply(day.bin$day,spool>1,mean)
tapply(day.bin.f$day,spoolf>1,mean)
tapply(day.bin$day,day.bin$GPP,mean,na.rm=TRUE)
tapply(day.bin$day,day.bin$Phen,mean,na.rm=TRUE)

sclass = rep(0,length(spool))
sclass[day.bin$model %in% forest.models] = 2
sclass[day.bin$site %in% forest.sites] = sclass[day.bin$site %in% forest.sites] + 1
tapply(day.bin$day,sclass,mean,na.rm=TRUE)



###### SINGLE SITE X MODEL Examples #######

.libPaths("~/lib/R")
library(fields)
nbreak <- 12

load("HowlandTowerRawWave.Rdata")
Power.tower = Power

jpeg("HowlandTowerRawWave2.jpg",width=5,height=3.75,units="in",res=300)
#mylevels <- quantile(Power.tower,seq(0,1,length=(nbreak +1)),na.rm=TRUE)
time = (1:nrow(Power.tower))/48/365.25 + ystart[i]
image.plot(time,Period,
           Power.tower,
           ylab="Period (days)",log='y',
           main=paste(site.name[i],"WAVE"),
           breaks=mylevels,#lab.breaks=formatC(mylevels,digits = 2),
           axis.args=list(at=log(mylevels),labels=formatC(mylevels,digits = 2)),
           nlevel=nbreak,xlab="Time (year)"
             )
dev.off()

## scale bar
lcol = tim.colors(nbreak)
plot(0,0,type='n',xlim=c(0,3),ylim=c(0,nbreak+2))
for(i in 1:nbreak){
  rect(1,i,5/4,i+1,col=lcol[i])
}
text(1.3,1:(nbreak+1),formatC(mylevels,digits=2,format="e"),pos=4,cex=1.2,font=2)

load("HowlandEDRawWave.Rdata")
Power.model = Power

jpeg("HowlandEDRawWave2.jpg",width=5,height=3.75,units="in",res=300)
#mylevels <- quantile(Power.model,seq(0,1,length=(nbreak +1)),na.rm=TRUE)
time <- (1:nrow(Power.model))/day/365.25 + 1996
image.plot(time,Period,
           Power.model,
           ylab="Period (days)",log='y',
           main=paste(site.name[i],"WAVE"),
           breaks=mylevels,#lab.breaks=formatC(mylevels,digits = 2),
           axis.args=list(at=log(mylevels),labels=formatC(mylevels,digits = 2)),
           nlevel=nbreak,xlab="Time (year)"
             )
dev.off()

## scale bar
lcol = tim.colors(nbreak)
plot(0,0,type='n',xlim=c(0,3),ylim=c(0,nbreak+2))
for(i in 1:nbreak){
  rect(1,i,5/4,i+1,col=lcol[i])
}
text(1.3,1:(nbreak+1),formatC(mylevels,digits=2,format="e"),pos=4,cex=1.2,font=2)
## single site, single model full half plane
m = 9
s = 26
i = 7
  
load(paste(spec.dir,"/NACPspecNORMpre2v2.",s,".",m,".Rdata",sep=""))
Power.error = Power

jpeg("HowlandEDIndexWave2.jpg",width=5,height=3.75,units="in",res=300)
#mylevels <- quantile(Power.error,seq(0,1,length=(nbreak +1)),na.rm=TRUE)
time <- (1:nrow(Power.error))/day/365.25 + 1996
sel <- time < 2004
time = time[sel]
Power.error = Power.error[sel,]
image.plot(time,Period,
           Power.error,
           ylab="Period (days)",log='y',
           main=paste(site.name[i],"WAVE"),
           breaks=mylevels,#lab.breaks=formatC(mylevels,digits = 2),
           axis.args=list(at=log(mylevels),labels=formatC(mylevels,digits = 2)),
           nlevel=nbreak,xlab="Time (year)"
             )
dev.off()

## scale bar
lcol = tim.colors(nbreak)
plot(0,0,type='n',xlim=c(0,3),ylim=c(0,nbreak+2))
for(i in 1:nbreak){
  rect(1,i,5/4,i+1,col=lcol[i])
}
text(1.3,1:(nbreak+1),formatC(mylevels,digits=2,format="e"),pos=4,cex=1.2,font=2)

## unified Scale Bar

rtot = range(c(range(Power.tower,na.rm=TRUE),
  range(Power.model,na.rm=TRUE),
  range(Power.error,na.rm=TRUE)))

hist(log10(Power.tower),xlim=log10(rtot),probability=TRUE)
lines(density(log10(Power.model),na.rm=TRUE),col=2)
lines(density(log10(Power.error),na.rm=TRUE),col=3)

mylevels = 10^c(-22,-6:1,3)
nbreak = length(mylevels)-1
