
library(hdf5,lib.loc="~/lib/R/Rhdf")

### FUNCTIONS
dm <- c(0,32,60,91,121,152,182,213,244,274,305,335,366)
dl <- c(0,32,61,92,122,153,183,214,245,275,306,336,367)
month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
mon_num <- c("01","02","03","04","05","06","07","08","09","10","11","12")
day2mo <- function(year,day){
  leap <- (year %% 4 == 0)
  mo <- rep(NA,length(day))
  mo[leap] <- findInterval(day[leap],dl)
  mo[!leap] <- findInterval(day[!leap],dm)
  return(mo)
}
ndays <- c(31,28,31,30,31,30, 31,31,30,31,30,31)
ndayl <- c(31,29,31,30,31,30, 31,31,30,31,30,31)
nday <- function(mo,year){
  leap <- (year %% 4 == 0)
  if(leap) return(ndayl[mo])
  ndays[mo]
}
num <- function(d){as.numeric(as.character(d))}
checkNA <- function(val,master){
  if(length(val) == 0) return(rep(master,each=6))
  sNA <- which(is.na(val))
  if(length(sNA) == 0) return(val)
  ## translate NA's
  mNA <- round(sNA*length(master)/length(val))
  mNA[mNA < 1] <- 1
  mNA[mNA > length(master)] <- length(master)
  val[sNA] <- master[mNA]
  return(val)
}


lat <- read.table("../../veg/northing",header=FALSE,skip=1)
minN <- num(substr(lat[,4],1,8))
lon <- read.table("../../veg/easting",header=FALSE,skip=0)
minE <- num(substr(lon[,4],1,8))
lat <- 35+58/60+mean(minN)/3600
lon <- 79+05/50+mean(minE)/3600

#### for duke, files divided by VARIABLE not by TIME

## Air Temperature
Tair1 <- read.csv("AT97-03gap.csv",header=FALSE,skip=2)
Tair1 <- cbind(Tair1[,1:3],apply(Tair1[,4:11],1,mean,na.rm=TRUE))
colnames(Tair1) <- c("JDT","DOY","Time","TempC")
Tair2 <- read.csv("AT04-08gap.csv",header=FALSE,skip=2) 
Tair2 <- cbind(Tair2[,1:3],apply(Tair2[,c(4,8,10,12,16,18,20,21)],1,mean,na.rm=TRUE))
colnames(Tair2) <- c("JDT","DOY","Time","TempC")
Tair <- rbind(Tair1,Tair2,deparse.level=0)
breaks <- c(1,which(Tair$DOY == 1 & Tair$Time == 30),nrow(Tair)+1)
yrs <- 1997:2008
yr <- rep(NA,nrow(Tair))
for(y in 1:length(yrs)){
  yr[breaks[y]:(breaks[y+1]-1)] <- yrs[y]
}
mo <- day2mo(yr,Tair$DOY)
TempK <- Tair$TempC+273.15
Tair <- cbind(Tair,yr,mo)
## set partial year to NA's so it will be gapfilled
dfull <- rep(1:365,each=48)
mfull <- day2mo(1997,dfull)
sel <- 1:(length(mfull)-length(which(yr == 1997)))
Tadd <- cbind(rep(NA,length(sel)),dfull[sel],matrix(NA,length(sel),2),rep(1997,length(sel)),mfull[sel])
colnames(Tadd) <- colnames(Tair)
Tair <- rbind(Tadd,Tair)


## RH
RH1 <- read.csv("RH97-03gap.csv",header=FALSE,skip=2)
RH1 <- cbind(RH1[,1:3],apply(RH1[,4:11],1,mean,na.rm=TRUE))
colnames(RH1) <- c("JDT","DOY","Time","RH")
RH2 <- read.csv("RH04-08gap.csv",header=FALSE,skip=2) 
RH2 <- cbind(RH2[,1:3],apply(RH2[,c(4,8,10,12,16,18,20,21)],1,mean,na.rm=TRUE))
colnames(RH2) <- c("JDT","DOY","Time","RH")
RH <- rbind(RH1,RH2,deparse.level=0)
breaks <- c(1,which(RH$DOY == 1 & RH$Time == 30),nrow(RH)+1)
yrs <- 1997:2008
yr <- rep(NA,nrow(RH))
for(y in 1:length(yrs)){
  yr[breaks[y]:(breaks[y+1]-1)] <- yrs[y]
}
mo <- day2mo(yr,RH$DOY)
SH <-  (RH$RH/100.)*2.541e6*exp(-5415./(TempK))*(18./29.)
RH <- cbind(RH,SH,yr,mo)
## set partial year to NA's so it will be gapfilled
dfull <- rep(1:365,each=48)
mfull <- day2mo(1997,dfull)
sel <- 1:(length(mfull)-length(which(yr == 1997)))
Radd <- cbind(rep(NA,length(sel)),dfull[sel],matrix(NA,length(sel),3),rep(1997,length(sel)),mfull[sel])
colnames(Radd) <- colnames(RH)
RH <- rbind(Radd,RH)

### PRETREATMENT 
newdat <- read.csv("DUKE_TRH93-95.csv",header=FALSE,skip=2)

  
### Precip
PPT <- read.csv("PRECIPgap.csv",header=FALSE,skip=2) 
colnames(PPT) <- c("JDT","DOY","Time","PPT")
breaks <- c(1,which(PPT$DOY == 1 & PPT$Time == 30),nrow(PPT)+1)
yrs <- 1997:2007
yr <- rep(NA,nrow(PPT))
for(y in 1:length(yrs)){
  yr[breaks[y]:(breaks[y+1]-1)] <- yrs[y]
}
mo <- day2mo(yr,PPT$DOY)
PPT$PPT <- PPT$PPT/1800.
PPT <- cbind(PPT,yr,mo)
sel <- which(PPT$JDT > 1000)
PPT <- PPT[sel,]
## set partial year to NA's so it will be gapfilled
dfull <- rep(1:365,each=48)
mfull <- day2mo(1997,dfull)
sel <- 1:(length(mfull)-length(which(yr == 1997))+1)
Padd <- cbind(rep(NA,length(sel)),dfull[sel],matrix(NA,length(sel),2),rep(1997,length(sel)),mfull[sel])
colnames(Padd) <- colnames(PPT)
PPT <- rbind(Padd,PPT)


## RAD/WIND
TOW <- read.csv("RadWindGap.csv",header=FALSE,skip=2) 
colnames(TOW) <- c("JDT","DOY","Time","Z","ind","WS","WD","PAR","Rn","SWup","SWdn","LWup","LWdn")
breaks <- c(which(TOW$DOY == 1 & TOW$Time == 30),nrow(TOW)+1)
yrs <- 1998:2007
yr <- rep(NA,nrow(TOW))
for(y in 1:length(yrs)){
  yr[breaks[y]:(breaks[y+1]-1)] <- yrs[y]
}
mo <- day2mo(yr,TOW$DOY)
SW <- TOW$PAR/(0.45*4.6) # umol/m2/s -> w/m2
sel <- (TOW$SWup == 0 & TOW$Z < 90)
TOW$SWup[sel] <- SW[sel]
TOW$SWup[TOW$SWup<0] <- 0
TOW <- cbind(TOW,yr,mo)
SW <- TOW$SWup

  ### RADIATION CALCULATION
  ##build time variables (year, month, day of year)
  dt <- 1800
  yr <- TOW$yr
  doy <- TOW$DOY
  hr <- TOW$Time/100
  hr[hr %% 1 > 0] <- floor(hr[hr %% 1 > 0]) + 0.5
  mo <- day2mo(yr,doy)
  
  ## calculate potential radiation
  ## in order to estimate diffuse/direct
  f <- pi/180*(279.5+0.9856*doy)
  et <- (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600  #equation of time -> eccentricity and obliquity
  merid <- floor(lon/15)*15
  if(merid<0) merid <- merid+15
  lc <- (lon-merid)*-4/60  ## longitude correction
  tz <- merid/360*24 ## time zone
  midbin <- 0.5*dt/86400*24 ## shift calc to middle of bin
##  t0 <- 12+lc-et-tz-midbin   ## solar time
t0 <- lc-et-tz-midbin   ## solar time
  h <- pi/12*(hr-t0)  ## solar hour
  dec <- -23.45*pi/180*cos(2*pi*(doy+10)/365)  ## declination

  cosz <- sin(lat*pi/180)*sin(dec)+cos(lat*pi/180)*cos(dec)*cos(h)
  cosz[cosz<0] <- 0
  
  rpot <- 1366*cosz
  rpot <- rpot[1:length(SW)]
  rpotL <-(rpot[c(13:length(SW),1:12)])#[1:length(SW)]
##rpotL <-(rpot[c(12:1,1:length(SW))])[1:length(SW)]
  

  SW[rpotL < SW] <- rpotL[rpotL<SW] ## ensure radiation < max
      ### this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
  frac <- SW/rpot
  frac[frac>0.9] <- 0.9  ## ensure some diffuse
  frac[frac < 0.0] <- 0.0
  frac[is.na(frac)] <- 0.0
  frac[is.nan(frac)] <- 0.0
  SWd <- SW*(1-frac)  ## Diffuse portion of total short wave rad
  
### convert to ED2.1 hdf met variables
  nbdsfA <- (SW - SWd) * 0.57 # near IR beam downward solar radiation [W/m2]
  nddsfA <- SWd * 0.48        # near IR diffuse downward solar radiation [W/m2]
  vbdsfA <- (SW - SWd) * 0.43 # visible beam downward solar radiation [W/m2]
  vddsfA <- SWd * 0.52        # visible diffuse downward solar radiation [W/m2]

###LOAD CO2

CO2 <- read.csv("CO2gap.csv",header=FALSE,skip=2) 
colnames(CO2) <- c("JDT","DOY","Time","R1","R2","R3","R4","R5","R6","R7","R8","RA")
breaks <- c(1,which(CO2$DOY == 1 & CO2$Time == 30),nrow(CO2)+1)
yrs <- 1996:2007
yr <- rep(NA,nrow(CO2))
for(y in 1:length(yrs)){
  yr[breaks[y]:(breaks[y+1]-1)] <- yrs[y]
}
mo <- day2mo(yr,CO2$DOY)
AMB <- (CO2$R1+CO2$R5+CO2$R6+CO2$R8)/4
ELEV <- (CO2$R2+CO2$R3+CO2$R4+CO2$R7)/4
CO2 <- cbind(CO2,AMB,ELEV,yr,mo)
## gap-fill 96 CO2 w/ 1997
C96full <- CO2[CO2$yr == 1997,]
C96full <- rbind(C96full[1:48,],C96full) ## rep day 1 to account for leap year
C96full$mo[1:48] <- 2 ## set mo to feb (doesn't have to be in order)
C96full$yr <- 1996
CO2 <- rbind(C96full[1:(nrow(C96full)-sum(CO2$yr == 1996)),],CO2)

### loop over years to produce output
yrs <- 1996:2007
for(y in 1:length(yrs)){
  for(mo in 1:12){

    ## select month
    Tsel <- which(Tair$yr == yrs[y] & Tair$mo == mo) - 12#also for RH
    Psel <- which(PPT$yr == yrs[y] & PPT$mo == mo) - 12
    Wsel <- which(TOW$yr == yrs[y] & TOW$mo == mo) - 12
    Csel <- which(CO2$yr == yrs[y] & CO2$mo == mo) - 12
    Tsel[Tsel < 1] <- 1
    Psel[Psel < 1] <- 1
    Wsel[Wsel < 1] <- 1
    Csel[Csel < 1] <- 1
    
    ## set dims
    Tdims <- c(1,1,length(Tsel))
    if(length(Tsel) == 0) Tdims <- c(1,1,48*nday(mo,yrs[y]))
    Pdims <- c(1,1,length(Psel))
    if(length(Psel) == 0) Pdims <- c(1,1,48*nday(mo,yrs[y]))
    Wdims <- c(1,1,length(Wsel))
    if(length(Wsel) == 0) Wdims <- c(1,1,48*nday(mo,yrs[y]))
    Cdims <- c(1,1,length(Csel))
    if(length(Csel) == 0) Cdims <- c(1,1,48*nday(mo,yrs[y]))

#    pres  <- array(presA[selm],dim=dims)
    
    ## define variables
    nbdsf <- array(nbdsfA[Wsel],dim=Wdims)
    nddsf <- array(nddsfA[Wsel],dim=Wdims)
    vbdsf <- array(vbdsfA[Wsel],dim=Wdims)
    vddsf <- array(vddsfA[Wsel],dim=Wdims)
    prate <- array(PPT$PPT[Psel],dim=Pdims)
    dlwrf <- array(TOW$LWup[Wsel],dim=Wdims)

    hgt   <- array(50,dim=Wdims)
    ugrd  <- array(TOW$WS[Wsel],dim=Wdims)
    vgrd  <- array(0,dim=Wdims)
    sh    <- array(RH$SH[Tsel],dim=Tdims)
    tmp   <- array(TempK[Tsel],dim=Tdims)

                   
    ## grab & fill in other vars
    narr <- hdf5load(paste("NARR/duke_",yrs[y],month[mo],".h5",sep=""),load=FALSE)
    ##pres  <- 1e5*(rep(ncep[,6],each=6)/1004.)^(1004./287.)  ## exner -> pres
    pres  <- narr$pres

    dlwrf[dlwrf < 50] <- NA

    ## fill missing
    nbdsf <- checkNA(nbdsf,narr$nbdsf)
    nddsf <- checkNA(nddsf,narr$nddsf)
    vbdsf <- checkNA(vbdsf,narr$vbdsf)
    vddsf <- checkNA(vddsf,narr$vddsf)
    dlwrf <- checkNA(dlwrf,narr$dlwrf)
    prate <- checkNA(prate,narr$prate)
    hgt   <- checkNA(hgt,narr$hgt)
    ugrd  <- checkNA(ugrd,sqrt(narr$ugrd^2+narr$vgrd^2))
    sh    <- checkNA(sh,narr$sh)
    tmp   <- checkNA(tmp,narr$tmp)
    
#### OUTPUT ####
    
    ##ambient
    co2 <- array(CO2$AMB[Csel],dim=Cdims)
    mout <- paste("AMB_",yrs[y],month[mo],".h5",sep="")    
    hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt"
             ,"ugrd","vgrd","sh","tmp","co2")
    ## elevated
    co2 <- array(CO2$ELEV[Csel],dim=Cdims)
    mout <- paste("ELEV_",yrs[y],month[mo],".h5",sep="")    
    hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt"
             ,"ugrd","vgrd","sh","tmp","co2")
    
  }
}
