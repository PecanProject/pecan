## R Code to convert from NACP intercomparison NETCDF met files
## into ED2 ascii met files

library(ncdf)
library(hdf5)

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

## get site file
site <- read.csv("site_location.csv",header=TRUE)

## Get file list
fname <- dir(".",".nc")

## loop over files
for(i in 33:34){#35:length(fname)){

  ## extract file root name
  froot <- substr(fname[i],1,6)
  print(c(i,froot))

  ## open netcdf
  nc <- open.ncdf(fname[i])

  ## determine GMT adjustment
  lst <- site$LST_shift[which(site$acro == froot)]
  
  ## extract variables
  lat  <- get.var.ncdf(nc,"lat")
  lon  <- get.var.ncdf(nc,"lon")
  sec   <- nc$dim$t$vals
  Tair <- get.var.ncdf(nc,"Tair")
  Qair <- get.var.ncdf(nc,"Qair")  #humidity (kg/kg)
  Wind <- get.var.ncdf(nc,"Wind")
  Rain <- get.var.ncdf(nc,"Rainf")
  pres <- get.var.ncdf(nc,"Psurf")
  SW   <- get.var.ncdf(nc,"SWdown")
  LW   <- get.var.ncdf(nc,"LWdown")
  CO2  <- get.var.ncdf(nc,"CO2air")
  
  dt <- sec[2]-sec[1]
  toff <- -lst*3600/dt

  ##buffer to get to GMT
  slen <- length(SW)
  Tair <- c(rep(Tair[1],toff),Tair)[1:slen]
  Qair <- c(rep(Qair[1],toff),Qair)[1:slen]
  Wind <- c(rep(Wind[1],toff),Wind)[1:slen]
  Rain <- c(rep(Rain[1],toff),Rain)[1:slen]
  pres <- c(rep(pres[1],toff),pres)[1:slen]
  SW <- c(rep(SW[1],toff),SW)[1:slen]
  LW <- c(rep(LW[1],toff),LW)[1:slen]
  CO2 <- c(rep(CO2[1],toff),CO2)[1:slen]
  
  ## determine starting year
  base.time <- as.numeric(substr(nc$dim$t$units,15,18))
  if(is.na(base.time)){
    print(c("did not extract base time correctly",froot,i,nc$dim$t$units))
    break
  }
  
  ##build time variables (year, month, day of year)
  nyr <- floor(length(sec)/86400/365*dt)
  yr <- NULL
  doy <- NULL
  hr <- NULL
  asec <- sec
  for(y in base.time+1:nyr-1){
    ytmp <- rep(y,365*86400/dt)
    dtmp <- rep(1:365,each=86400/dt)
    if(y %% 4 == 0){  ## is leap
      ytmp <- rep(y,366*86400/dt)
      dtmp <- rep(1:366,each=86400/dt)
    }
    if(is.null(yr)){
      yr <- ytmp
      doy <- dtmp
      hr <- rep(NA,length(dtmp))
    } else {
      yr <- c(yr,ytmp)
      doy <- c(doy,dtmp)
      hr <- c(hr,rep(NA,length(dtmp)))
    }
    rng <- length(doy) - length(ytmp):1 + 1
    asec[rng] <- asec[rng] - asec[rng[1]]
    hr[rng] <- (asec[rng] - (dtmp-1)*86400)/86400*24
  }
  mo <- day2mo(yr,doy)
  if(length(yr) < length(sec)){
    rng <- (length(yr)+1):length(sec)
    yr[rng] <- rep(y+1,length(rng))
    doy[rng] <- rep(1:366,each=86400/dt)[1:length(rng)]
    hr[rng] <- rep(seq(0,length=86400/dt,by=dt/86400*24),366)[1:length(rng)]
  }
  
  ## calculate potential radiation
  ## in order to estimate diffuse/direct
  f <- pi/180*(279.5+0.9856*doy)
  et <- (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600  #equation of time -> eccentricity and obliquity
  merid <- floor(lon/15)*15
  if(merid<0) merid <- merid+15
  lc <- (lon-merid)*-4/60  ## longitude correction
  tz <- merid/360*24 ## time zone
  midbin <- 0.5*dt/86400*24 ## shift calc to middle of bin
  t0 <- 12+lc-et-tz-midbin   ## solar time
  h <- pi/12*(hr-t0)  ## solar hour
  dec <- -23.45*pi/180*cos(2*pi*(doy+10)/365)  ## declination

  cosz <- sin(lat*pi/180)*sin(dec)+cos(lat*pi/180)*cos(dec)*cos(h)
  cosz[cosz<0] <- 0
  
  rpot <- 1366*cosz
  rpot <- rpot[1:length(SW)]
  
  SW[rpot < SW] <- rpot[rpot<SW] ## ensure radiation < max
      ### this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
  frac <- SW/rpot
  frac[frac>0.9] <- 0.9  ## ensure some diffuse
  frac[frac < 0.0] <- 0.0
  frac[is.na(frac)] <- 0.0
  frac[is.nan(frac)] <- 0.0
  SWd <- SW*(1-frac)  ## Diffuse portion of total short wave rad
  
### convert to ED2.1 hdf met variables
  n     <- length(Tair)
  nbdsfA <- (SW - SWd) * 0.57 # near IR beam downward solar radiation [W/m2]
  nddsfA <- SWd * 0.48        # near IR diffuse downward solar radiation [W/m2]
  vbdsfA <- (SW - SWd) * 0.43 # visible beam downward solar radiation [W/m2]
  vddsfA <- SWd * 0.52        # visible diffuse downward solar radiation [W/m2]
  prateA <- Rain              # precipitation rate [kg_H2O/m2/s]
  dlwrfA <- LW                # downward long wave radiation [W/m2]
  presA  <- pres              # pressure [Pa]
  hgtA   <- rep(50,n)         # geopotential height [m]
  ugrdA  <- Wind              # zonal wind [m/s]
  vgrdA  <- rep(0,n)          # meridional wind [m/s]
  shA    <- Qair              # specific humidity [kg_H2O/kg_air]
  tmpA   <- Tair              # temperature [K]
  co2A   <- CO2               # surface co2 concentration [ppm]

  ## create directory
  if(system(paste("ls",froot),ignore.stderr=TRUE)>0) system(paste("mkdir",froot))
  
  ## write by year and month
  for(y in base.time+1:nyr-1){
    sely <- which(yr == y)
    for(m in unique(mo[sely])){
      selm <- sely[which(mo[sely] == m)]
      mout <- paste(froot,"/",froot,"_",y,month[m],".h5",sep="")
      dims <- c(1,1,length(selm))
      nbdsf <- array(nbdsfA[selm],dim=dims)
      nddsf <- array(nddsfA[selm],dim=dims)
      vbdsf <- array(vbdsfA[selm],dim=dims)
      vddsf <- array(vddsfA[selm],dim=dims)
      prate <- array(prateA[selm],dim=dims)
      dlwrf <- array(dlwrfA[selm],dim=dims)
      pres  <- array(presA[selm],dim=dims)
      hgt   <- array(hgtA[selm],dim=dims)
      ugrd  <- array(ugrdA[selm],dim=dims)
      vgrd  <- array(vgrdA[selm],dim=dims)
      sh    <- array(shA[selm],dim=dims)
      tmp   <- array(tmpA[selm],dim=dims)
      co2   <- array(co2A[selm],dim=dims)
      hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt","ugrd","vgrd","sh","tmp","co2")
    }
  }

  ## write DRIVER file
  sites <- 1
  metfile <- paste(froot,"/ED_MET_DRIVER_HEADER",sep="")
  metpath <- paste(getwd(),"/",froot,"/",froot,"_",sep="")
  metgrid <- c(1,1,1,1,floor(lon),floor(lat))
  metvar <- c("nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt","ugrd","vgrd","sh","tmp","co2")
  nmet <- length(metvar)
  metfrq <- rep(dt,nmet)
  metflag <- rep(1,nmet)
  write.table("#header",metfile,row.names=FALSE,col.names=FALSE)
  write.table(sites,metfile,row.names=FALSE,col.names=FALSE,append=TRUE)
  write.table(metpath,metfile,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
  write.table(matrix(metgrid,nrow=1),metfile,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
  write.table(nmet,metfile,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
  write.table(matrix(metvar,nrow=1),metfile,row.names=FALSE,col.names=FALSE,append=TRUE)
  write.table(matrix(metfrq,nrow=1),metfile,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
  write.table(matrix(metflag,nrow=1),metfile,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
 
} ### end loop over met files
