
### recode of flux2lsm to run on new HDF5 files as reference input and output
### version for Ameriflux L3 files

### SETTINGS
site <- "nc_pita"
siteID <- "USNC2"
lat  =  35.8031
lon  = -76.66791
tlag <- ceiling(lon/15)/24
tlag <- -5.0/24

#site <- "mize"
#siteID <- "USSP2"
#lat <-  29.7648
#lon <- -82.2448166667
#tlag <- -5.0/24

site <- "donaldson"
siteID <- "USSP3"
lat <-  29.7547666667
lon <- -82.1632833333
tlag <- -5.0/24

site <- "oak_openings"
siteID <- "USOho"
lat <- 41.55454
lon <- -83.84376
tlag <- -5.0/24



#yr0 <- 1987
yr0 <- 1998
yrf <- 2008

tstep = 1800
rad_step = 900
build.date <- Sys.Date()
ismissing <- rep(TRUE,12)
inpath <- paste("/home/mdietze/inputs/fluxnet/",site,"/",sep="")
#refpath <- "/home/scratch/dietze_lab/NARR/bartlett/bartlett_"
refpath <- "/home/scratch/dietze_lab/NARR/lower48/lower48_"
LAT <- c(26,49)
LON <- 360+c(-124.5,-67)
YRES <- 0.50
XRES <- 0.50
XSEQ <- seq(LON[1]+XRES/2,LON[2],by=XRES)
YSEQ <- seq(LAT[1]+YRES/2,LAT[2],by=YRES)
NX <- length(XSEQ)
NY <- length(YSEQ)
cellX <- findInterval(360+lon,XSEQ)
cellY <- findInterval(lat,YSEQ)

outpath <- paste("/home/mdietze/inputs/fluxnet/",site,"/met",build.date,"/",site,"_",sep="")
system(paste("mkdir /home/mdietze/inputs/fluxnet/",site,"/met",build.date,"/",sep=""))

## VARIABLES
# 1 - hgt
# 2 - prate
# 3 - pres
# 4 - sh
# 5 - tmp
# 6 - ugrd
# 7 - vgrd
# 8 - co2
# 9 - dlwrf
# 10 - nbdsf
# 11 - nddsf
# 12 - vbdsf
# 13 - vddsf
# 14 - date
                 
### LIBRARIES
library(XML,lib.loc="~/lib/R")
library(ed21,lib.loc="~/lib/R")
library(hdf5,lib.loc="~/lib/R/Rhdf")
setwd(paste("~/inputs/fluxnet/",site,sep=""))

### FUNCTIONS
T_st = 373.15 #steam temperature (K)
e_st = 1013.25 #saturation vapor pressure at steam temp (hPa)
radians = 3.14159/180.0
Kelvin = 273.15  #Celsius->Kelvin
C_PARCONV = 1.0/(0.45*4.6)
WATT = 1/4.6

rh2rv <- function(rh, T){
  #/converts relative humidity to specific humidity
  #/input: rh = relative humidity (proportion, not %)
  #/input: T  = absolute temperature
  rh*2.541e6*exp(-5415.0/T)*18/29
}

SatVapPres <- function(T){
  #/estimates saturation vapor pressure (kPa)  Goff-Gratch 1946
  #/input: T = absolute temperature
  0.1*exp( -7.90298*(T_st/T-1) + 5.02808*log(T_st/T) - 1.3816e-7*(10^(11.344*(1-T/T_st))-1) + 8.1328e-3*(10^(-3.49149*(T_st/T-1))-1) + log(e_st))  
}

exner <- function(pres){
  #/ estimated exner function
  #/ input: pres = air pressure (Bar)
  1004.0*pres^(287.0/1004.0)
}

AirDens <- function(pres, T, rv){
  #/ estimate air density from pressure, temperature, and humidity
  #/ input: pres = air pressure (pascals)
  #/ input: T    = air temperature (Kelvin)
  #/ input: rv   = humidity
  pres/(287.0*T*(1.0+0.61*rv))
}

firstday <- function(mo,yr){
  ldoy = c(0,31,60,91,121,152,182,213,244,274,305,335,366)  
  doy = c(0,31,59,90,120,151,181,212,243,273,304,334,365)  
  if(yr%%4 == 0){return(ldoy[mo])}
  return(doy[mo])
}

nday <- function(mo,yr){
  lnday = c(31,29,31,30,31,30,31,31,30,31,30,31)  
  nday = c(31,28,31,30,31,30,31,31,30,31,30,31)  
  if((yr-2000)%%4 == 0){return(lnday[mo])}
  return(nday[mo])
}


smoo <- function(a,b,c){  ## smoother
  comb <- c(a[-7:0 + length(a)],b,c[1:8])
  comb <- rep(comb,each=6)
  comb <- filter(comb,c(0.1,0.2,0.4,0.2,0.1),"con",circular = TRUE)
  comb <- comb[49:(length(comb)-48)]
  comb
}

smoR <- function(a,mo,yr,tstep = 900,torig = 10800){  ## radiation
  dat <- rep(a,each=torig/tstep)
  lab <- rep(1:length(a),each=torig/tstep)
  d0 <- firstday(mo,yr)
  df <- firstday(mo+1,yr)-1
  rin <- rpot(rep(d0:df,each=86400/tstep),rep(seq(tstep,86400,tstep),nday(mo,yr)),lat,tlag*24)
  rbar <- rep(tapply(rin,lab,mean),each=torig/tstep)
  r <-apply(cbind(dat*rin/rbar,rep(0,length(dat))),1,max)  
}

month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

# calcDecDay( day,  month,  year){
#  day-1 + firstday(month,year)
#}

dirfrac <- function(jday,time,rshort,lat){
  
  dayangle=2.0*3.14159*(jday)/365.25
  declination=0.006918-0.399912*cos(dayangle)+0.070257*sin(dayangle)-0.006758*cos(2.0*dayangle)+0.000907*sin(2.0*dayangle)-0.002697*cos(3.0*dayangle)+0.00148*sin(3.0*dayangle)  
  eccen=1.00011+0.034221*cos(dayangle)+0.00128*sin(dayangle)+0.000719*cos(2.0*dayangle)+0.000077*sin(2.0*dayangle)
  solartime=time/3600.0-12.0-1.0
  ket=1367.0*eccen*(cos(declination)*cos(lat/360.0*2.0*3.14159)*cos(15.0/360.0*2.0*3.14159*(solartime))+sin(declination)*sin((lat)/360.0*2.0*3.14159))
  frac=rshort/ket
  if (frac > 0.9) frac=0.9
  if (frac < 0.0) frac=0.0
  frac
}

dirfrac2 <- function(rpotn,rshort){
  frac=rshort/rpotn
  frac[frac > 0.9]=0.9
  frac[frac < 0.0]=0.0
  frac
}

rpot <- function(jday,time,lat,tlag){
  
  dayangle=2.0*3.14159*(jday)/365.25
  declination=0.006918-0.399912*cos(dayangle)+0.070257*sin(dayangle)-0.006758*cos(2.0*dayangle)+0.000907*sin(2.0*dayangle)-0.002697*cos(3.0*dayangle)+0.00148*sin(3.0*dayangle)  
  eccen=1.00011+0.034221*cos(dayangle)+0.00128*sin(dayangle)+0.000719*cos(2.0*dayangle)+0.000077*sin(2.0*dayangle)
  solartime=time/3600.0-12.0+tlag
  ket=1367.0*eccen*(cos(declination)*cos(lat/360.0*2.0*3.14159)*cos(15.0/360.0*2.0*3.14159*(solartime))+sin(declination)*sin((lat)/360.0*2.0*3.14159))
  ket[ket<0] <- 0
  ket
}


### MAIN

## get list of met/flux files
mfile <- dir(inpath,"L3")
mfile <- mfile[grep("txt",mfile)]

## load site data
met <- list(hgt=NULL,prate=NULL,pres=NULL,sh=NULL,tmp=NULL,ugrd=NULL,vgrd=NULL,co2=NULL,dlwrf=NULL,nbdsf=NULL,nddsf=NULL,vbdsf=NULL,vddsf=NULL,date=NULL)
for(i in 1:length(mfile)){
  print(mfile[i])
  ## read ameriflux annual file
  rawmet <- read.csv(mfile[i],header=TRUE,na.strings=c("-9999.00","-9999"))
  rawmet[rawmet==-9999] <- NA
  year <- sub(siteID,"",mfile[i])
  year <- as.numeric(sub("_L3.txt","",year))
  if(!is.numeric(year)|is.na(year)) print(c("ODD YEAR",year,mfile[i]))
  
  ## preprocess some met variables
  w <- rawmet$WS
  wdir <- rawmet$WD

  ## meteorology
  met$prate <- c(met$prate , rawmet$Precip/1800)
  met$tmp   <- c(met$tmp   , rawmet$Ta + Kelvin)
  met$sh    <- c(met$sh    , rh2rv(rawmet$Rh/100,rawmet$Ta))
  met$ugrd  <- c(met$ugrd  , w*cos(wdir*radians))
  met$vgrd  <- c(met$vgrd  , w*sin(wdir*radians))
  met$date  <- rbind(met$date,cbind(rep(year,nrow(rawmet)),rawmet[,1:4])) ## date
  ## should we set CO2


  ## preprocess some radiation variables
  rshort <- rawmet$Rg ## w/m2
  vis <- rawmet$PPFD * WATT ## umol/m2/sec -> w/m2
  vis[vis < 0] <- 0.0
  miss <- which(is.na(rshort))
  rshort[miss] <- vis[miss]/0.45
  rpotn <- rawmet$R_pot
  rdiff <- rawmet$Rd
  fdiff <- rdiff/rshort
  fdir  <- 1-fdiff
  fdir.pot <- dirfrac2(rpotn,rshort)
  miss <- which(is.na(fdir))
  fdir[miss] <-fdir.pot[miss]
  nir <- rshort - vis
  
  
  ## radiation
  met$nbdsf <- c(met$nbdsf , nir*fdir)
  met$vbdsf <- c(met$vbdsf , vis*fdir)
  met$nddsf <- c(met$nddsf , nir*(1-fdir))
  met$vddsf <- c(met$vddsf , vis*(1-fdir))
  met$dlwrf <- c(met$dlwrf , rep(NA,length(vis)))
  
}

##shift tower data to GMT
for(i in 1:13){
  if(!is.null(met[[i]])){
    met[[i]] <- c(rep(mean(met[[i]][1:(-48*tlag)]),-48*tlag),met[[i]])
  }
}

## interp rad to 15min
rad <- list(hgt=NULL,prate=NULL,pres=NULL,sh=NULL,tmp=NULL,ugrd=NULL,vgrd=NULL,co2=NULL,dlwrf=NULL,nbdsf=NULL,nddsf=NULL,vbdsf=NULL,vddsf=NULL,date=NULL)

for(j in 9:13){
  for(yr in yr0:yrf){
    for(mo in 1:12){    
      d0 <- firstday(mo,yr)+1
      df <- firstday(mo+1,yr)
      sel <- which(met[[14]][,1] == yr)
#      sel <- sel[which(met[[14]][sel,2] %in% d0:df)]
      sel <- sel[which(met$date[sel,2] == mo)]
      if(length(sel)>0){
##        rtemp <- smoR(met[[j]][sel],mo,yr,rad_step,1800)
        rtemp <- rep(met[[j]][sel],each=1800/rad_step)
##        if(!is.null(rtemp)){
          if(is.null(rad[[j]])){
            rad[[j]] <- rtemp
          }else{
            rad[[j]] <- c(rad[[j]],rtemp)
          }
##        }
      }
    }
  }
}

narr <- narr.next <- hdf5load(paste(refpath,yr0,month[1],".h5",sep=""),load=FALSE)
## FOR EACH NARR FILE
for(yr in yr0:yrf){
  for(mo in 1:12){    
    
    ##load NARR data
    narr.last <- narr
    narr <- narr.next
    if(mo < 12){
      narr.next <- hdf5load(paste(refpath,yr,month[mo+1],".h5",sep=""),load=FALSE)
    } else {
      if(yr < yrf){
        narr.next <- hdf5load(paste(refpath,yr+1,month[1],".h5",sep=""),load=FALSE)
      }
    }
    
    ##interp to target res
    rmet <- list()
    for(j in c(1,4:8,11)){
      nj = which(names(met) == names(narr)[j])
      rmet[[j]] <- smoo(narr.last[[j]][cellY,cellX,],narr[[j]][cellY,cellX,],narr.next[[j]][cellY,cellX,])
    }
    for(j in c(2,3,9,10)){ #not LW
      rmet[[j]] <- smoR(narr[[j-1]][cellY,cellX,],mo,yr,rad_step)
    }
    
    ##align tower
    d0 <- firstday(mo,yr)+1
    df <- firstday(mo+1,yr)
    
    sel <- which(met[[14]][,1] == yr)
    sel <- sel[which(floor(met[[14]][sel,5]-0.001) %in% d0:df)]
    
    if(length(sel)>0){      
      ##merge atm
      for(j in c(1,4:8)){
        nj = which(names(met)==names(narr)[j])
        if(!is.null(met[[nj]])){
          if(j == 8) { ###precip
            ##snowcor
            snow <- which(rmet[[5]] < Kelvin)
            met[[nj]][snow] <- rmet[[j]][snow] ##fill snow from NARR
          }
          nsel <- which(!is.na(met[[nj]][sel]))
          rmet[[j]][nsel] <- met[[nj]][sel[nsel]] ## fill NARR from tower
        }
      }
      ## merge radiation
      ryr <- rep(met[[14]][,1],each=1800/rad_step)
      rdoy <- rep(met[[14]][,5],each=1800/rad_step)
      rsel <- which(ryr == yr)
      rsel <- which(rdoy[rsel] %in% d0:df)
      for(j in c(2,3,9,10)){
        nj = which(names(met)==names(narr)[j])
        if(!is.null(met[[nj]])){
          nsel <- which(!is.na(rad[[nj]][rsel]))
          rmet[[j]][nsel] <- rad[[nj]][rsel[nsel]] ## fill NARR from tower
        }
      }     
    } ## else just output downscaled NARR
    
    ##output
    ofile <- paste(outpath,yr,month[mo],".h5",sep="")
    Rfile <- paste(outpath,yr,month[mo],".RData",sep="")
    vgrd  <- array(rmet[[1]],c(1,1,length(rmet[[1]])))
    vddsf <- array(rmet[[2]],c(1,1,length(rmet[[2]])))
    vbdsf <- array(rmet[[3]],c(1,1,length(rmet[[3]])))
    ugrd  <- array(rmet[[4]],c(1,1,length(rmet[[4]])))
    tmp   <- array(rmet[[5]],c(1,1,length(rmet[[5]])))
    sh    <- array(rmet[[6]],c(1,1,length(rmet[[6]])))
    pres  <- array(rmet[[7]],c(1,1,length(rmet[[7]])))
    prate <- array(rmet[[8]],c(1,1,length(rmet[[8]])))
    nddsf <- array(rmet[[9]],c(1,1,length(rmet[[9]])))
    nbdsf <- array(rmet[[10]],c(1,1,length(rmet[[10]])))
#    hgt   <- array(rmet[[11]],c(1,1,length(rmet[[11]])))
#    dlwrf <- array(rmet[[12]],c(1,1,length(rmet[[12]])))
    dlwrf <- array(rmet[[11]],c(1,1,length(rmet[[11]])))
    hgt = 50
    hdf5save(ofile,"vgrd","vddsf","vbdsf","ugrd","tmp","sh","pres","prate","nddsf","nbdsf","hgt","dlwrf")
    save(vgrd,vddsf,vbdsf,ugrd,tmp,sh,pres,prate,nddsf,nbdsf,hgt,dlwrf,file=Rfile)
  } ## end month
} ## end year
