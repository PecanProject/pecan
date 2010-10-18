extract2driver <- function(lat, lon, sitename, yr0, yrf) {
  ## Code to convert most recent SREF files into met drivers and figures

##################################
  ##                              ##
  ##    SINGLE SITE TIMESERIES    ## 
  ##                              ##
##################################
  ## how to extract a specific cell

  ## Energy Farm
  LAT <- lat
  LON <- ifelse(lon<0, 360 + lon, lon)

  ## TIME TO GRAB

  dirname <- "/home/scratch/dietze_lab/NARR"
  NDIR <- "/home/scratch/dietze_lab/NOMADS"


  smoR <- function(a,mo,yr,tstep = 900,torig = 10800){  ## radiation
    dat <- rep(a,each=torig/tstep)
    lab <- rep(1:length(a),each=torig/tstep)
    d0 <- firstday(mo,yr)
    df <- firstday(mo+1,yr)-1
    rin <- rpot(rep(d0:df,each=86400/tstep),rep(seq(tstep,86400,tstep),nday(mo,yr)),lat,tlag*24)
    rbar <- rep(tapply(rin,lab,mean),each=torig/tstep)
    r <-apply(cbind(dat*rin/rbar,rep(0,length(dat))),1,max)  
    r[rbar == 0] <- 0

    ## filter
    bound <- which(diff(lab)>0)
    bound <- bound[rin[bound] > 0 & rin[bound+1] >0]
    for(i in bound){
      k1 = c(1/6,1/2,1/2,-1/6)
      k2 = c(-1/6,1/2,1/2,1/6)
      x1 = sum(k1*r[i + 1:4 -2 ])
      x2 = sum(k2*r[i + 1:4 -2 ])
      r[i] = x1
      r[i+1]=x2
    }
    
    return(r)
  }

  month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

  rpot <- function(jday,time,lat,tlag){
    
    dayangle=2.0*3.14159*(jday)/365.25
    declination=0.006918-0.399912*cos(dayangle)+0.070257*sin(dayangle)-0.006758*cos(2.0*dayangle)+0.000907*sin(2.0*dayangle)-0.002697*cos(3.0*dayangle)+0.00148*sin(3.0*dayangle)  
    eccen=1.00011+0.034221*cos(dayangle)+0.00128*sin(dayangle)+0.000719*cos(2.0*dayangle)+0.000077*sin(2.0*dayangle)
    solartime=time/3600.0-12.0+tlag
    ket=1367.0*eccen*(cos(declination)*cos(lat/360.0*2.0*3.14159)*cos(15.0/360.0*2.0*3.14159*(solartime))+sin(declination)*sin((lat)/360.0*2.0*3.14159))
    ket[ket<0] <- 0
    ket
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
  gribgrab <- function(srch,vnam){
    system(paste(NDIR,"/wgrib -d ",srch," ",dirname,"/soi_tmp/",fname[it]," -text -h -o ",dirname,"/soi_tmp/",vnam,".txt", sep=""))
    V <- read.table(paste(dirname,"/soi_tmp/",vnam,".txt",sep=""),skip=1,header=FALSE)
    V <- V[[1]]
    system(paste("rm ",dirname,"/soi_tmp/",vnam,".txt",sep=""))
    rval <- NA
    if(length(V)> 0){
      rval <- sum(V[ROWS]*WT,na.rm=TRUE)
    }
    rval
  }

  dm <- c(1,32,60,91,121,152,182,213,244,274,305,335,366)
  dl <- c(1,32,61,92,122,153,183,214,245,275,306,336,367)
  month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  mon_num <- c("01","02","03","04","05","06","07","08","09","10","11","12")
  m2d <- function(m,y){
    ## convert from month to day of year
    if(y %% 4 == 0){
      return(dl[m])
    }
    return(dm[m])
  }


  ## check if GRIB files are there yet
  gfiles <- dir(dirname,"tar")
  if(length(gfiles) == 0) stop()
  fsplit <- strsplit(gfiles, ".",fixed=TRUE)
  var <- yr <- mo <- sday <- fday <- rep(NA,length(fsplit))
  for(i in 1:length(fsplit)){
    var[i]  <- substr(fsplit[[i]][1],5,7)
    yr[i]   <- substr(fsplit[[i]][1],9,12)
    mo[i]   <- substr(fsplit[[i]][1],13,14)
    sday[i] <- substr(fsplit[[i]][1],16,17)
    fday[i] <- substr(fsplit[[i]][1],18,19)
  }

### SELECT TARGET TIME
  sel <- which(yr >= yr0 & yr <= yrf)
  gfiles <- gfiles[sel]
  var <- var[sel]
  yr <- yr[sel]
  mo <- mo[sel]
  sday <- sday[sel]
  fday <- fday[sel]

  METDATA <- list()
  ## load maps
  map <- "rr-fixed.grb"
  system(paste(NDIR,"/wgrib -d 20 ",map," -text -h -o ",dirname,"/soi_tmp/NLAT.txt", sep=""))
  system(paste(NDIR,"/wgrib -d 19 ",map," -text -h -o ",dirname,"/soi_tmp/ELON.txt", sep=""))
  system(paste(NDIR,"/wgrib -d 16 ",map," -text -h -o ",dirname,"/soi_tmp/LAND.txt", sep=""))

  NLAT <- read.table(paste(dirname,"/soi_tmp/NLAT.txt",sep=""),skip=1,header=FALSE)[,1]
  ELON <- read.table(paste(dirname,"/soi_tmp/ELON.txt",sep=""),skip=1,header=FALSE)[,1]
  LAND <- read.table(paste(dirname,"/soi_tmp/LAND.txt",sep=""),skip=1,header=FALSE)[,1]

  NLAT[NLAT>1.0e20] <- NA
  ELON[ELON>1.0e20] <- NA
  LAND[LAND>1.0e20] <- NA
  lsel <- which(LAND>0)
  ## Determine extraction location
  ROW <- which.min((LAT-NLAT)^2 +(LON-ELON)^2)
  ## Determine 4 nearest neighbors for interpolation
  dist <- (LAT-NLAT)^2 +(LON-ELON)^2
  ROWS <- lsel[order(dist[lsel])[1:4]]
  ROWS <- ROWS[which(LAND[ROWS] > 0)]
  if(length(ROWS) == 0) ROWS = ROW
  WT <- (1/sqrt(dist[ROWS]))/sum(1/sqrt(dist[ROWS]))

  ## loop over large files
  for(i in 1:length(gfiles)){

    ## copy file to temp space and open up
    system(paste("cp",gfiles[i],"soi_tmp"))
    system(paste("cd soi_tmp; tar -xf",gfiles[i]))
    system(paste("rm soi_tmp/",gfiles[i],sep=""))
    
    ## get list of sub-files; parse
    fname <- dir("soi_tmp","merged")
    day <- hr <- rep(NA,length(fname))
    for(j in 1:length(fname)){
      day[j] <- substr(fname[j],21,22)
      hr[j]  <- substr(fname[j],23,24)
    }
    
### LOOP OVER SMALL FILES
    met <- matrix(NA,nrow=length(fname),ncol=11)  
    for(it in 1:length(fname)){
      
#######################
######  IF SFC ########
#######################
      if(var[i] == "sfc"){
        met[it,1] <- gribgrab(33,"CFRZR")
        met[it,2] <- gribgrab(32,"CICEP")
        met[it,3] <- gribgrab(34,"CRAIN")    
        met[it,4] <- gribgrab(31,"CSNOW")
        met[it,5] <- gribgrab(42,"DLWRF")
        met[it,6] <- gribgrab(41,"DSWRF")
        met[it,7] <- gribgrab(3,"PRES")
        met[it,8] <- gribgrab(24,"PRATE")
        met[it,9] <- gribgrab(5,"TMP")
      } else{
        if(var[i] == "flx"){
          met[it,1]  <- gribgrab(38,"TMP10")
          met[it,2]  <- gribgrab(44,"TMP30")
          met[it,3]  <- gribgrab(35,"UGRD10")
          met[it,4]  <- gribgrab(41,"UGRD30")
          met[it,5]  <- gribgrab(36,"VGRD10")
          met[it,6]  <- gribgrab(42,"VGRD30")
          met[it,7]  <- gribgrab(40,"SPFH10")
          met[it,8]  <- gribgrab(46,"SPFH30")
          met[it,9]  <- gribgrab(39,"PRES10")
          met[it,10] <- gribgrab(45,"PRES30")
          met[it,11] <- gribgrab(4,"HGT1")
          
        } else {
          print(c("FILE TYPE UNKNOWN",gfiles[i]))
        }
      }
      system(paste("rm ",dirname,"/soi_tmp/",fname[it],sep="")) ## clean up
    }  ## end loop within file    

    METDATA[[i]] <- met
    print(c(i,"of",length(gfiles)))
  }  ## end loop over ensemble members
  save.image("duke_face.NARR.RData")

#### MERGE CHUNKS INTO TIMESERIES
  precip <- NULL
                                        #library(hdf5,lib.loc="/home/mdietze/lib/R/Rhdf")
  dt <- 10800
  lon <- LON
  if(lon>180) lon <- lon-360
  lat <- LAT
  tlag <- floor(lon/15)/24  ## ceiling?
  for(y in unique(yr)){
    ysel <- which(yr == y)
    for(m in unique(mo[ysel])){
      msel <- ysel[which(mo[ysel] == m)]    
      ssel <- msel[which(var[msel] == "sfc")]
      fsel <- msel[which(var[msel] == "flx")]

      sfc <- METDATA[[ssel[1]]]
      for(i in 2:length(ssel)) sfc <- rbind(sfc,METDATA[[ssel[i]]])

      flx <- METDATA[[fsel[1]]]
      for(i in 2:length(fsel)) flx <- rbind(flx,METDATA[[fsel[i]]])

      met <- cbind(flx,sfc[,1:9])
      n     <- nrow(met)
      
      ## calculate time variables
      doy <- rep(m2d(as.numeric(m),as.numeric(y)):(m2d(as.numeric(m)+1,as.numeric(y))-1),each = 8)
      hr <- rep(seq(0,21,by=3),length=n)
      
      ## calculate potential radiation
      ## in order to estimate diffuse/direct

      f <- pi/180*(279.5+0.9856*doy)
      et <- (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(4*f)
             -429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600
      ##equation of time -> eccentricity and obliquity
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
      
      rpotn <- 1366*cosz

      SW <- met[,17]
      SW[rpotn < SW] <- rpotn[rpotn<SW] ## ensure radiation < max
### this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
      frac <- SW/rpotn
      frac[frac>0.9] <- 0.9  ## ensure some diffuse
      frac[frac < 0.0] <- 0.0
      frac[is.na(frac)] <- 0.0
      frac[is.nan(frac)] <- 0.0
      SWd <- SW*(1-frac)  ## Diffuse portion of total short wave rad
      
      
      ## write as h5
      mout <- paste(froot,"/",froot,"_",y,month[as.numeric(m)],".h5",sep="")
      dims <- c(1,1,n)
      nbdsf <- array((SW - SWd) * 0.57,dim=dims) # near IR beam downward solar radiation [W/m2]
      nddsf <- array(SWd * 0.48,dim=dims)        # near IR diffuse downward solar radiation [W/m2]
      vbdsf <- array((SW - SWd) * 0.43,dim=dims) # visible beam downward solar radiation [W/m2]
      vddsf <- array(SWd * 0.52,dim=dims)        # visible diffuse downward solar radiation [W/m2]
      prate <- array(met[,19],dim=dims)              # precipitation rate [kg_H2O/m2/s]
      prate[prate < 0 ] <- 0.0
      precip <- c(precip,prate)
      dlwrf <- array(met[,16],dim=dims)                # downward long wave radiation [W/m2]
      pres  <- array(met[,18],dim=dims)              # pressure [Pa]
      hgt   <- array(rep(50,n),dim=dims)         # geopotential height [m]
      ugrd  <- array(met[,3],dim=dims)              # zonal wind [m/s]
      vgrd  <- array(met[,5],dim=dims)          # meridional wind [m/s]
      sh    <- array(met[,7],dim=dims)              # specific humidity [kg_H2O/kg_air]
      tmp   <- array(met[,1],dim=dims)              # temperature [K]

      nbdsf <- array(smoR(nbdsf,as.numeric(m),as.numeric(y),900),dim=c(dims[1],dims[2],dims[3]*12))
      nddsf <- array(smoR(nddsf,as.numeric(m),as.numeric(y),900),dim=c(dims[1],dims[2],dims[3]*12))
      vbdsf <- array(smoR(vbdsf,as.numeric(m),as.numeric(y),900),dim=c(dims[1],dims[2],dims[3]*12))
      vddsf <- array(smoR(vddsf,as.numeric(m),as.numeric(y),900),dim=c(dims[1],dims[2],dims[3]*12))
      
      hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt","ugrd","vgrd","sh","tmp")
      
    }  ## end month
  } ## end year
  

###################  CODE TO DOWNSCALE RADIATION FROM 3hr to 15 min based on background radiation
  if (FALSE){

    smoR <- function(a,mo,yr,tstep = 900,torig = 10800){  ## radiation
      dat <- rep(a,each=torig/tstep)
      lab <- rep(1:length(a),each=torig/tstep)
      d0 <- firstday(mo,yr)
      df <- firstday(mo+1,yr)-1
      rin <- rpot(rep(d0:df,each=86400/tstep),rep(seq(tstep,86400,tstep),nday(mo,yr)),lat,tlag*24)
      rbar <- rep(tapply(rin,lab,mean),each=torig/tstep)
      r <-apply(cbind(dat*rin/rbar,rep(0,length(dat))),1,max)  
      r[rbar == 0] <- 0

      ## filter
      bound <- which(diff(lab)>0)
      bound <- bound[rin[bound] > 0 & rin[bound+1] >0]
      for(i in bound){
        k1 = c(1/6,1/2,1/2,-1/6)
        k2 = c(-1/6,1/2,1/2,1/6)
        x1 = sum(k1*r[i + 1:4 -2 ])
        x2 = sum(k2*r[i + 1:4 -2 ])
        r[i] = x1
        r[i+1]=x2
      }
      
      return(r)
    }

    month <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

    rpot <- function(jday,time,lat,tlag){
      
      dayangle=2.0*3.14159*(jday)/365.25
      declination=0.006918-0.399912*cos(dayangle)+0.070257*sin(dayangle)-0.006758*cos(2.0*dayangle)+0.000907*sin(2.0*dayangle)-0.002697*cos(3.0*dayangle)+0.00148*sin(3.0*dayangle)  
      eccen=1.00011+0.034221*cos(dayangle)+0.00128*sin(dayangle)+0.000719*cos(2.0*dayangle)+0.000077*sin(2.0*dayangle)
      solartime=time/3600.0-12.0+tlag
      ket=1367.0*eccen*(cos(declination)*cos(lat/360.0*2.0*3.14159)*cos(15.0/360.0*2.0*3.14159*(solartime))+sin(declination)*sin((lat)/360.0*2.0*3.14159))
      ket[ket<0] <- 0
      ket
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

    froot = "rhinewi"
    lat <-45.6
    lon<- -89.4 
    tlag <- floor(lon/15)/24  ## ceiling?

    library(hdf5)
    hfile <- dir(froot,"h5",full.names=TRUE)
    for(i in 1:length(hfile)){

      hdf5load(hfile[i])

      
      n = nchar(hfile[i])
      yr = as.numeric(substr(hfile[i],n-9,n-6))
      m  = substr(hfile[i],n-5,n-3)
      mo = which(month == m)
      mout = paste(substr(hfile[i],1,n-11),"2",substr(hfile[i],n-10,n),sep="")
      
      nbdsf <- smoR(nbdsf,mo,yr,900)
      nddsf <- smoR(nddsf,mo,yr,900)
      vbdsf <- smoR(vbdsf,mo,yr,900)
      vddsf <- smoR(vddsf,mo,yr,900)

      hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt","ugrd","vgrd","sh","tmp")

    }


  }
}
