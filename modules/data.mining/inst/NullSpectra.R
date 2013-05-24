### generate wavelet spectra of eddy flux data that incorporates uncertainty 
###
### This code is computationally intensive so it is set up to be submitted to a cluster queue
### assumes that it is passed an ID for which site is being analyzed and a number range of
### Monte Carlo iterations to process
###
### Michael Dietze
###
### inputs: annual NEE matrices of [time x ensemble member] 
### outputs: mean and quantile spectra

source("ResidSpectra.R")

## Get site sel'n from cmd line
sitenum <- as.numeric(system("echo $SITENUM",intern=TRUE)) ## site number
nstart <- as.numeric(system("echo $NSTART",intern=TRUE))  ## first Monte Carlo to process
n2proc <- 50   ## number of spectra to process

## Paths and prefixes
path <- "/home/mdietze/stats/spectral/"
model.dir <- "NEEm"
site.files <- dir(model.dir,"txt")

#####################################
##   load up the model data        ##
#####################################
site.name <- site.files[sitenum]
site.name <- sub("_NEE.txt","",site.name)
site.name <- sub("-","",site.name)
prefix <- paste("NEE_",site.name,"-",sep="")
field <- paste("NEEf/",site.name,"/FilledNEE/",sep="")
dat <- read.table(paste(model.dir,site.files[sitenum],sep="/"),
                  header=TRUE,
                  na.string="-999.000"
                  )
day <-  1/diff(dat$FDOY[1:2])       ## number of observations per day

#####################################
##   load up the "pseudo" data     ##
#####################################
files <- dir(path,prefix)
files <- files[grep("f32",files)]

yrs <- sub(prefix,"",files)
yrs <- as.numeric(sub(".f32","",yrs))
print(length(yrs))

dat <- NULL
ylen <- rep(NA,length(yrs))
for(i in 1:length(yrs)){
  print(yrs[i])

  db <- readBin(paste(path,files[i],sep=""),"double",size=4,n=17569000)
  dbm <- matrix(db,ncol=1000)
  ylen[i] <- nrow(dbm)

  if(is.null(dat)){
    dat <- dbm
  } else {
    dat <- rbind(dat,dbm)
  }  
}
save(dat,ylen,yrs,file=paste(prefix,"pseudo.Rdata",sep=""))

#####################################
##     load up the "true" data     ##
#####################################
ffiles <- dir(paste(path,field,sep=""),"Moving")
fdat <- NULL
fylen <- rep(NA,length(yrs))
for(i in 1:length(yrs)){
  print(yrs[i])

  fd <- read.table(paste(path,field,ffiles[i],sep=""),skip=2,header=FALSE)
  fylen[i] <- nrow(fd)

  if(is.null(dat)){
    fdat <- fd
  } else {
    fdat <- rbind(fdat,fd)
  }  
}
save(fdat,fylen,yrs,file=paste(prefix,".field.Rdata",sep=""))

fdat[fdat == -9999] <- NA

############  LOOP OVER REPLICATES #############
################################################
pspec <- matrix(NA,nrow(dat),ncol(dat))
Pspec <- matrix(NA,100,1000)
for (i in seq(nstart,length=n2proc,by=1)){
  
  print(i)
  
  ### Calculate the error
  
  
  ## option 3 - normalized residuals (pre)
  ## subscripts: t = tower, p = pseudodata
  
  ## normalize tower
  NEEt.bar <- mean(fdat[,5],na.rm=TRUE)
  NEEt.sd  <- NA
  if(is.nan(NEEt.bar)){
    NEEt.bar <- NA
  } else {
    NEEt.sd <- sqrt(var(fdat[,5],na.rm=TRUE))
  }    
  NEEt.norm <- (fdat[,5] - NEEt.bar)/NEEt.sd
  
  ## normalize model
  NEEp.bar <- mean(dat[,i],na.rm=TRUE)
  NEEp.sd  <- NA
  if(is.nan(NEEp.bar)){
    NEEp.bar <- NA
  } else {
    NEEp.sd <- sqrt(var(dat[,i],na.rm=TRUE))
  }    
  NEEp.norm <- (dat[,i] - NEEp.bar)/NEEp.sd  ###########
  y <- NEEp.norm-NEEt.norm  ## calc residuals of normalized
  
  y[is.na(y)] <- 0 ## need to fill in missing values
  
    
  ### Do the wavelet power spectrum
  wv <- ResidSpecta(data=fdat[,5],model=dat[,i],obsPerDay=day,case=3)
  ##  wv <- WAVE(y,p2=17)  ## Calculate wavelet spectrum *************************
  Pglobe <- apply(wv$Power,2,sum,na.rm=TRUE)
  Pspec[1:length(Pglobe),i] <- Pglobe
  
  ### Also, do Fourier power spectra
  s <- spectrum(wv$y,plot=FALSE)  
  pspec[1:length(s$spec),i] <- s$spec
  period <- 1/s$freq/day
  
  save(wv,Power,day,file=paste("pseudo.",sitenum,".",i,".Rdata",sep=""))
  save(i,Pspec,pspec,Period,period,file=paste(site.name,".",nstart,".specCI.Rdata",sep=""))
}



if(FALSE){
  ## some diagnostics
period <- 1/s$freq/48
pspec <- pspec[1:length(period),]

pbar <- apply(pspec,1,mean,na.rm=TRUE)
pCI <- apply(pspec,1,quantile,c(0.05,0.5,0.95),na.rm=TRUE)
plot(period,pbar,log="xy",
     ylim=range(pCI),type='l',ylab="Power",xlab="Period (days)")
lines(period,pCI[1,],col=3)
#lines(period,pCI[2,],col=2)
lines(period,pCI[3,],col=4)
abline(v=c(0.5,1,365.25/2,365.25),col=2,lty=2)

sel <- which(period > 0.8 & period < 1.3)

plot(period[sel],pbar[sel],log="xy",
     ylim=range(pCI),type='l',ylab="Power",xlab="Period (days)")
lines(period[sel],pCI[1,sel],col=3)
#lines(period,pCI[2,],col=2)
lines(period[sel],pCI[3,sel],col=4)
abline(v=c(0.5,1,365.25/2,365.25),col=2,lty=2)

save.image("USHo1.specCI.Rdata")
}
