library(XML,lib.loc="~/lib/R")
library(ed21,lib.loc="~/lib/R")

### Load data
datadir <- "/home/mdietze/stats/lapse/"

stationfile <- dir(datadir,"dat.txt",full.names=TRUE)
dat <- read.csv(stationfile)
met <- ExtractNOAAstationMonthly(dat)

### load station locations
namefile <- dir(datadir,"stn.txt",full.names=TRUE)
stn <- read.fwf(namefile,c(7,8,31,33,7,32,18,10,11,10),header=FALSE,skip=2,na.strings=" ",as.is=TRUE)

keep <- which(!(1:nrow(stn) %in% c(15))) ## exclude stations

stnID <- as.numeric(as.character(stn[keep,1]))
elev <- as.numeric(as.character(stn[keep,10]))
#mch <- match(stnID,met$ID)


Temp <- apply(met$MNTM,2,mean,na.rm=TRUE)[keep]
ppt  <- apply(met$TPCP,2,mean,na.rm=TRUE)[keep]
snow <- apply(met$TSNW,2,mean,na.rm=TRUE)[keep]

par(mfrow=c(3,1))

plot(elev,Temp)
tm <- lm(Temp ~ elev)
abline(a=coef(tm)[1],b=coef(tm)[2])

plot(elev,ppt)
pm <- lm(ppt ~ elev)
abline(a=coef(pm)[1],b=coef(pm)[2])

plot(elev,snow)
sm <- lm(snow ~ elev)
abline(a=coef(sm)[1],b=coef(sm)[2])

print(c("Temp",coef(tm)))
print(c("ppt",coef(pm)))
print(c("snow",coef(sm)))

## need to double check units!!

par(mfrow=c(1,2))
plot(elev,Temp,xlab="Elevation (m)",ylab="Temperature (C)")
tm <- lm(Temp ~ elev)
abline(a=coef(tm)[1],b=coef(tm)[2])

plot(elev,ppt*12,xlab="Elevation (m)",ylab="Precipitation (mm/year)")
pm <- lm(ppt*12 ~ elev)
abline(a=coef(pm)[1],b=coef(pm)[2])
