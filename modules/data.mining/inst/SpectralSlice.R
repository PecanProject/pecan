## Code to evaluate one specific Intermediate Time slice for one model at one site

site  = 26 ## Howland
model = 9  ## ED

site  = 8  ## CA-Oas
model = 10 ## LOTEC

site  = 25 ## Harvard
model = 5  ## ecosys

site  = 25 ## Harvard
model = 5  ## ecosys

site  = 5  ## Lethbridge
model = 15 ## orchidee

site  = 7  ## Mer Bleue
model = 12 ## isolsm   ## first instance where model more variable than the data

site  = 9  ## obs
model = 4  ## canibis  ## model more variable than data

tp    = c(10,70) ## time period (10 days)
outdir = "/scratch/NACP/spectral/"

## directory to find model files
model.dir <- "NEEm"
site.files <- dir(model.dir,"txt")

model.set <- sort(c("BEPS","CNCLASS","ISOLSM","TECO","ecosys","SiBCASA","SiB","DLEM","ED2","LoTEC_DA","AgroIBIS","DNDC","SiBcrop","can.ibis","EDCM","ORCHIDEE","LPJ","BIOME_BGC","SSiB2","TRIPLEX","EPIC"))

load(paste(outdir,"NACPspecNORMpre2.",site,".",model,".Rdata",sep=""))
period <- wv$period/day  ## wavelet periods

dat <- read.table(paste(model.dir,site.files[site],sep="/"),
                  header=TRUE,
                  na.string="-999.000"
                  )
m2c <- match(model.set,names(dat))  ## match model names to data table columns
date = dat$X.YEAR + dat$FDOY/366


NEEm = dat[,m2c[model]]
NEEt = dat$NEE_FILLED
    ## normalize tower
    NEEt.bar <- mean(NEEt,na.rm=TRUE)
    NEEt.sd  <- NA
    if(is.nan(NEEt.bar)){
      NEEt.bar <- NA
    } else {
      NEEt.sd <- sqrt(var(NEEt,na.rm=TRUE))
    }    
    NEEt.norm <- (NEEt - NEEt.bar)/NEEt.sd
    ## normalize model
    NEEm.bar <- mean(NEEm,na.rm=TRUE)
    NEEm.sd  <- NA
    if(is.nan(NEEm.bar)){
      NEEm.bar <- NA
    } else {
      NEEm.sd <- sqrt(var(NEEm,na.rm=TRUE))
    }    
    NEEm.norm <- (NEEm - NEEm.bar)/NEEm.sd
    y <- NEEm.norm-NEEt.norm  ## calc residuals of normalized
NEEt[is.na(NEEt)] = 0.0
NEEm.s = NEEt.s = list()
for(i in 1:length(tp)){
#filt = c(1:(day*tp/2),(day*tp/2):1)
  filt = rep(1,day*tp[i])
  filt = filt/sum(filt)
  NEEm.s[[i]] = filter(NEEm,filt)
  NEEt.s[[i]] = filter(NEEt,filt)
  y.s = filter(y,filt)
}
## find the spectral band for the desired period
band <- list()
for(i in 1:length(tp)){
  k = which.min((period - tp[i])^2)
  ##band = apply(Power[,k+(-2:2)],1,mean)
  band[[i]] = apply(Power[,k+(-1:1)],1,mean)
  ##band = Power[,k]
  coi  = wv$coi/day
  j = which.min((coi-tp[i])^2)
  sel = j:(length(band[[i]])-j)
  band[[i]][1:j] <- NA
  band[[i]][length(band[[i]]) - (0:j)] <- NA
}

if(FALSE){
## find the null spectra
stepsize <- 50
site.name <- site.files[site]
site.name <- sub("_NEE.txt","",site.name)
site.name <- sub("-","",site.name)
PspecG <- NULL
for(nstart in seq(1,951,by=50)){
  print(c(site,nstart))
#  load(paste(outdir,site.name,".",nstart,".specCIclip.Rdata",sep=""))
  load(paste(outdir,site.name,".",nstart,".specCI.Rdata",sep=""))
  if(is.null(PspecG)){
    PspecG <- matrix(NA,length(Period),1000)
  }
  PspecG[,seq(nstart,length=stepsize,by=1)] <- Pspec[1:length(Period),seq(nstart,length=stepsize,by=1)]  
}
bandN <- NULL
for(i in 1:1000){
  print(i)
#  save(wv,Power,day,file=paste("NACPspecNORM4clip.pseudo.",sitenum,".",i,".Rdata",sep=""))
  load(file=paste(outdir,"NACPspecNORM4.pseudo.",site,".",i,".Rdata",sep=""))

  if(is.null(bandN)){
    bandN <- matrix(NA,1000,nrow(Power))
  }
  period <- wv$period/day  ## wavelet periods
  kg = which.min((period - tp)^2)
  bandN[i,] = apply(Power[,kg+(-1:1)],1,mean)
  coi  = wv$coi/day
  j = which.min((coi-tp)^2)
  sel = j:(length(band)-j)
  bandN[i,1:j] <- NA
  bandN[i,length(band) - (0:j)] <- NA
}
bandNci <- apply(bandN,2,quantile,0.95,na.rm=TRUE)
}

#jpeg("ED_Howland_10day.jpg",width=960)
#pdf("ED_Howland_10_70_day.pdf",width=11)
#pdf("LOTEC_OAS_10_70_day.pdf",width=11)
#pdf("ecosys_harvard_10_70_day.pdf",width=11)
#pdf("Orchidee_Leth_10_70_day.pdf",width=11)
#pdf("isolsm_mer_10_70_day.pdf",width=11)
#pdf("canibis_obs_10_70_day.pdf",width=11)


thresh = 10
sel <- which(date < 2004)
par(mfrow=c(3,1))
par(cex=1.2,lwd=3)
par(mar=c(2,4,0.5,0.1))
plot(date[sel],band[[1]][sel],type='l',log='y',ylim=c(0.05,max(sapply(band,max,na.rm=TRUE))),
     xlab="time",ylab="Power")
abline(h=thresh,col="grey")
lines(date[sel],band[[2]][sel],col=2)
#lines(date[sel],band[[3]][sel],col=3)
abline(v=1990:2010,lty=2,col="grey")
legend("bottomleft",legend=tp,col=c(1,2),lty=1,horiz=TRUE,bg='white')
##lines(bandNci,col=2,type='l')
#lines(abs(4*y.s),col=3,type='l')
for(i in 1:2){
  plot(date[sel],NEEm.s[[i]][sel],col=2,type='l',ylim=range(c(NEEt.s[[i]],NEEm.s[[i]]),na.rm=TRUE),
##  plot(date[sel],NEEm.s[[i]][sel],col=2,type='l',ylim=c(-4,2),
       xlab="time",ylab="NEE (umol/m2/s)")
  abline(h=0,col="grey")
  lines(date[sel],NEEt.s[[i]][sel],col=4,type='l')

  peaks = NEEt.s[[i]]
  peaks[which(band[[i]] < thresh)] <- NA
  peaks[is.na(band[[i]])] <- NA
  lines(date[sel],peaks[sel],col=5)
  abline(v=1990:2010,lty=2,col="grey")
  if(i == 2) legend("bottomleft",legend=c("Tower","Model"),col=c(4,2),lty=1,horiz=TRUE,bg='white')

}
  
lines(y.s,col=3,type='l')

plot(abs(NEEm.s - NEEt.s),col=6)
lines(abs(NEEm.s - NEEt.s)*(band > 10),col=2)
lines(abs(y.s),col=3,type='l')
