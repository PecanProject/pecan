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
spec.dir <- "Rdata"

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

model.set <- sort(c("BEPS","CNCLASS","ISOLSM","TECO","ecosys","SiBCASA","SiB","DLEM","ED2","LoTEC_DA","AgroIBIS","DNDC","SiBcrop","can.ibis","EDCM","ORCHIDEE","LPJ","BIOME_BGC","SSiB2","TRIPLEX","EPIC"))
Nmodel <- length(model.set)  ## number of models

cross.model <- list()
cross.site  <- list()

for(s in sites){

  if(s == 30) next
  
#  cross.site[[s]] <- list()
  cross.model[[s]] <- list()

  avg.spec <- norm.spec <- geom.spec <- NULL
  mcount <- 0

  load(paste(sub("-","",site.name[which(sites==s)]),".specCI.Rdata",sep=""))
  
  for(m in 1:Nmodel){
  
    print(c(s,m))
    
    load(paste(spec.dir,"/NACPspecNORMpre2.",s,".",m,".Rdata",sep=""))
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
        plot(period,Pglobe,log="xy",main='US-Ho1  ED2',ylim=c(1e4,max(Pglobe)),ylab="Power",type='l',lwd=2,cex.lab=1.5,cex.main=1.5,cex.axis=1.2)
        lines(Period,pCI[3,],col=2,lty=2)
        
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

  save(cross.site,cross.model,file="CrossSpec.v3.Rdata")
}

load("CrossSpec.v2.Rdata")

##### view global spectra by model:
####################################
sitesel <- 1:10
sitesel <- c(1:7,9,10)
pdf("byModel.v2.pdf",width=6.5,height=11)
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
    if(m < length(cross.model[[s]]) && !is.null(cross.model[[s]][[m]])){
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
      if(m < length(cross.model[[s]]) && !is.null(cross.model[[s]][[m]])){
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
    
### Normalized Spectra
    plot(Period,P.norm,ylim=range(1e-7,1),log='xy',type='n',ylab="Power",main=paste("NORMALISED",model.set[m]),xlab="period (days)")
    for(i in 1:length(sites)){
                                        #    lines(cross.model[[s]][[m]]$Pglobe)
      lines(Period,Power[i,]/Psum[i])
    }
    lines(Period,P.norm,ylim=c(1,maxP),type='l',lwd=3,col=2)
    lines(Period,P.gnorm,ylim=c(1,maxP),type='l',lwd=3,col=3)
    lines(Period,P.norm.med,ylim=c(1,maxP),type='l',lwd=3,col=4)
    abline(v=Period[breaks])

    ### CI Raw power - arithmetic mean, geometirc mean, and median
    plot(Period,PN.bar,ylim=c(.1,maxPN),log='xy',type='n',ylab="Power/(95% NULL)",main=paste("TEST RAW",model.set[m]),xlab="period (days)")
    for(i in 1:length(sites)){
                                        #    lines(cross.model[[s]][[m]]$Pglobe)
      lines(Period,PowerNorm[i,])
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

### Normalized Spectra
i <- 9
plot(ModelMean[[i]]$Period,ModelMean[[i]]$P.norm.med,ylim=range(1e-7,1),log='xy',type='n',ylab="Power",main=paste("ALL MODELS - NORMALISED"),xlab="period (days)")
for(i in 1:length(ModelMean)){
  lines(ModelMean[[i]]$Period,ModelMean[[i]]$P.norm.med)
}
abline(v=c(1,365.25),lty=3,col=6)
### CI Raw power
i<-9
plot(ModelMean[[i]]$Period,ModelMean[[i]]$PN.med,ylim=c(.1,maxPN),log='xy',type='n',ylab="Power/(95% NULL)",main=paste("ALL MODELS - SIGNIFICANCE"),xlab="period (days)")
for(i in 1:length(ModelMean)){
  lines(ModelMean[[i]]$Period,ModelMean[[i]]$PN.med)
}
abline(h=1,lwd=2,lty=2)
abline(v=c(1,365.25),col=6,lty=3)
dev.off()  


#### Composite spectra by site
.libPaths("~/lib/R")
library(fields)

for(i in sitesel){
  print(i)
  s <- sites[i]
  
  pdf(paste("bySite.",s,".pdf",sep=""),width=6.5,height=11)
  par(mfrow=c(3,1))
      

  ##  spec.bar <- spec.gmean <- spec.norm <- spec.gnorm <- matrix(0,
  ##    cross.site[[s]] <- list(avg=avg.spec,norm=norm.spec,geom=exp(geom.spec),period=period,day=day)
  nbreak <- 10
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

  mylevels <- quantile(cross.site[[s]]$norm,seq(0,1,length=(nbreak +1)),na.rm=TRUE)
  image.plot((1:nrow(cross.site[[s]]$norm))/cross.site[[s]]$day,cross.site[[s]]$period,
             cross.site[[s]]$norm,
             ylab="Period (days)",log='y',
             main=paste(site.name[i],"NORM"),
             breaks=mylevels,#lab.breaks=formatC(mylevels,digits = 2),
             axis.args=list(at=log(mylevels),labels=formatC(mylevels,digits = 2)),
             nlevel=nbreak,
             #breaks=exp(seq(min(log(cross.site[[s]]$norm),na.rm=TRUE),
             #  max(log(cross.site[[s]]$norm),na.rm=TRUE),length=65))
             xlab="Time (days)"
             )
  dev.off()
}


### ANOVA style analysis of power
ptable <- matrix(NA,Nmodel*9,7)
i <- 0
for(s in c(1,5,7,8,9,25,26,38,45)){
  for(m in 1:length(cross.model[[s]])){
    if(!is.null(cross.model[[s]][[m]])){
      i <- i+1
      ptable[i,1] <- s
      ptable[i,2] <- m
      ptable[i,3:7] <- cross.model[[s]][[m]]$pow.binN
    }
  }
}
ptable <- ptable[1:(i+1),]

ssub <- tapply(ptable[,3],ptable[,1],mean)
sday <- tapply(ptable[,4],ptable[,1],mean)
smid <- tapply(ptable[,5],ptable[,1],mean)
syear <- tapply(ptable[,6],ptable[,1],mean)
sinter <- tapply(ptable[,7],ptable[,1],mean)

barplot(rbind(ssub,sday,smid,syear,sinter),
        col=1:5,names.arg=site.name[sitesel])#,
##        legend.text=c("subdaily","diurnal",
##          "intermediate","annual","interannual")
##        )

msub <- tapply(ptable[,3],ptable[,2],mean)
mday <- tapply(ptable[,4],ptable[,2],mean)
mmid <- tapply(ptable[,5],ptable[,2],mean)
myear <- tapply(ptable[,6],ptable[,2],mean)
minter <- tapply(ptable[,7],ptable[,2],mean)
mbar <- rbind(msub,mday,mmid,myear,minter)
colnames(mbar) <- model.set

par(las=3,mar=c(7,4,4,2))
bp <- barplot(mbar,
        col=1:5,space=0.1,)#,
##        legend.text=c("subdaily","diurnal",
##          "intermediate","annual","interannual")
##        )
##text(bp,-2,model.set,pos=2,srt=45)
