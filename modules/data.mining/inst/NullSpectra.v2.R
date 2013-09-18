
### generate wavelet spectra of the flux data that incorporates
### random and gap-filling uncertainty based on the Barr et al
### NACP data product

### inputs: annual NEE matrices of [time x ensemble member] 
### outputs: mean and quantile spectra

## Get site sel'n from cmd line
sitenum <- as.numeric(system("echo $SITENUM",intern=TRUE))
nstart <- as.numeric(system("echo $NSTART",intern=TRUE))
n2proc <- 50

## Paths and prefixes
.libPaths("/home/mdietze/lib/R")
#library(R.matlab)
library(dplR) ## Andy Bunn's Dendrochronology package
WAVE <- function (crn.vec, yr.vec, p2=NULL, dj = 0.25, siglvl = 0.99, ...) 
{
  ## simple function based on Bunn's wavelet.plot fcn that returns wavelet info
  if(is.null(p2)) p2 <- floor(log2(length(crn.vec)))
    n <- length(crn.vec)
    Dt <- 1
    s0 <- 1
    j1 <- p2/dj
    mother <- "morlet"
    crn.vec.ac <- acf(crn.vec, lag.max = 2, plot = FALSE)
    lag1 <- (crn.vec.ac$acf[2] + sqrt(crn.vec.ac$acf[3]))/2
    return(wavelet(y1 = crn.vec, Dt = Dt, s0 = s0, dj = dj, 
        J = j1, mother = mother, siglvl = siglvl))
}

#path <- "/home/mcd/Desktop/NACP/Spectral/NEEbarr"
path <- "/home/mdietze/stats/spectral/"
ppath <- "/home/mdietze/stats/spectral/MDS/"
model.dir <- "NEEm"
site.files <- dir(model.dir,"txt")

site.name <- site.files[sitenum]
site.name <- sub("_NEE.txt","",site.name)
site.name <- sub("-","",site.name)
prefix <- paste("MDSNEE_",site.name,"-",sep="")
field <- paste("NEEf/",site.name,"/FilledNEE/",sep="")
rdat <- read.table(paste(model.dir,site.files[sitenum],sep="/"),
                  header=TRUE,
                  na.string="-999.000"
                  )
day <-  1/diff(rdat$FDOY[1:2])       ## number of observations per day

#day <- 48
#sitenum <- 26

yset = 1990:2010
if(sitenum == 5)  yset = 1999:2007
if(sitenum == 25) yset = 1992:2004
if(sitenum == 38) yset = 2002:2004
if(sitenum == 45) yset = 1999:2003 


#####################################
##   load up the "pseudo" data     ##
#####################################
files <- dir(ppath,prefix)
files <- files[grep("f32",files)]

yrs <- sub(prefix,"",files)
yrs <- as.numeric(sub(".f32","",yrs))
print(length(yrs))

ysel = which(yrs %in% yset)

dat <- NULL
ylen <- rep(NA,length(ysel))
for(i in ysel){
  print(yrs[i])

  db <- readBin(paste(ppath,files[i],sep=""),"double",size=4,n=17569000)
  dbm <- matrix(db,ncol=1000)
  ylen[i] <- nrow(dbm)

  if(is.null(dat)){
    dat <- dbm
  } else {
    dat <- rbind(dat,dbm)
  }  
}

save(dat,ylen,yrs,ysel,file=paste(prefix,"pseudo.Rdata",sep=""))

#dat[is.na(dat)] <- 0

#####################################
##     load up the "true" data     ##
#####################################
ffiles <- dir(paste(path,field,sep=""),"Moving")

fdat <- NULL
fylen <- rep(NA,length(ysel))
for(i in ysel){
  print(yrs[i])

  fd <- read.table(paste(path,field,ffiles[i],sep=""),skip=2,header=FALSE)
  fylen[i] <- nrow(fd)

  if(is.null(dat)){
    fdat <- fd
  } else {
    fdat <- rbind(fdat,fd)
  }  
}
save(fdat,fylen,yrs,ysel,file=paste(prefix,".field.Rdata",sep=""))

fdat[fdat == -9999] <- NA

############  LOOP OVER REPLICATES #############
################################################
pspec <- matrix(NA,nrow(dat),ncol(dat))
Pspec <- matrix(NA,100,1000)
for (i in seq(nstart,length=n2proc,by=1)){
  
  print(i)
  
### Calculate the error

  ##  err <- dat[,i] - fdat[,5]
  
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
  mydat = dat[,i]
  if(day < 30) { ## if dealing with 60 min day, average pseudodata
    grp = rep(1:(nrow(dat)/2),each=2)
    mydat = tapply(mydat,grp,mean)  
  }
  NEEp.bar <- mean(mydat,na.rm=TRUE)
  NEEp.sd  <- NA
  if(is.nan(NEEp.bar)){
    NEEp.bar <- NA
  } else {
    NEEp.sd <- sqrt(var(mydat,na.rm=TRUE))
  }    
  NEEp.norm <- (mydat - NEEp.bar)/NEEp.sd  ###########
  y <- NEEp.norm-NEEt.norm  ## calc residuals of normalized
  
  y[is.na(y)] <- 0 ## need to fill in missing values
  
  
### first do overall power spectra
  
  s <- spectrum(y,plot=FALSE)
  
  pspec[1:length(s$spec),i] <- s$spec
  ##    plot(1/s$freq,s$spec,log='xy')
  period <- 1/s$freq/day
  
### Do the wavelet power spectrum (implement later)
  
  wv <- WAVE(y)#,p2=17)  ## Calculate wavelet spectrum *************************
  Period <- wv$period/day  ## wavelet periods
  Power <- (abs(wv$wave))^2 ## wavelet power
  ## power correction, Liu et al 2007
  for(t in 1:length(wv$Scale)){
      Power[,t] = Power[,t]/wv$Scale[t]
    }
  ## Crop out cone of influence
  coi <- wv$coi  ## cone of influence (valid if below value)
  for(t in 1:length(coi)){
    sel <- which(Period>coi[t])
    Power[t,sel] <- NA 
  }

  Pglobe <- apply(Power,2,sum,na.rm=TRUE)
  Pspec[1:length(Pglobe),i] <- Pglobe
  
  save(wv,Power,day,file=paste("NACPspecNORM4clip.pseudo.",sitenum,".",i,".Rdata",sep=""))


  save(i,Pspec,pspec,Period,period,file=paste(site.name,".",nstart,".specCIclip.Rdata",sep=""))
}

print(c("mySITE",sitenum,nstart,"DONE"))


if(FALSE){
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
