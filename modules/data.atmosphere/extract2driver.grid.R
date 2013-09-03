## Code to convert NARR files into met drivers and figures

##################################
##                              ##
##    GRID  TIMESERIES          ## 
##                              ##
##################################
## how to extract a specific cell

## Central New England
LAT <- c(41.705,43.325)
LON <- 360+c(-73.428,-71.224)
YRES <- 0.10
XRES <- 0.10
froot <- "PALEO"

XSEQ <- seq(LON[1]+XRES/2,LON[2],by=XRES)
YSEQ <- seq(LAT[1]+YRES/2,LAT[2],by=YRES)
NX <- length(XSEQ)
NY <- length(YSEQ)

LATgrid <- matrix(rep(YSEQ,length(XSEQ)),NY,NX)
LONgrid <- matrix(rep(XSEQ,each=length(YSEQ)),NY,NX)

## TIME TO GRAB
yr0 <- 1979
yrf <- 1993

dirname <- "/home/scratch/dietze_lab/NARR"
NDIR <- "/home/scratch/dietze_lab/NOMADS"

gribgrab <- function(srch,vnam){
  system(paste(NDIR,"/wgrib -d ",srch," ",dirname,"/tmp/",fname[it]," -text -h -o ",dirname,"/tmp/",vnam,".txt", sep=""))
  V <- read.table(paste(dirname,"/tmp/",vnam,".txt",sep=""),skip=1,header=FALSE)
  V <- V[[1]]
  system(paste("rm ",dirname,"/tmp/",vnam,".txt",sep=""))
  rval <- NA
  if(length(V)> 0){
    rval <- tapply(V[ROWS]*WT,CELL,sum,na.rm=TRUE)
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

unique(yr)

## load maps
map <- "rr-fixed.grb"
system(paste(NDIR,"/wgrib -d 20 ",map," -text -h -o ",dirname,"/NLAT.txt", sep=""))
system(paste(NDIR,"/wgrib -d 19 ",map," -text -h -o ",dirname,"/ELON.txt", sep=""))
system(paste(NDIR,"/wgrib -d 16 ",map," -text -h -o ",dirname,"/LAND.txt", sep=""))

NLAT <- read.table(paste(dirname,"/NLAT.txt",sep=""),skip=1,header=FALSE)[,1]
ELON <- read.table(paste(dirname,"/ELON.txt",sep=""),skip=1,header=FALSE)[,1]
LAND <- read.table(paste(dirname,"/LAND.txt",sep=""),skip=1,header=FALSE)[,1]

NLAT[NLAT>1.0e20] <- NA
ELON[ELON>1.0e20] <- NA
LAND[LAND>1.0e20] <- NA

##plot(ELON-360,NLAT,col=LAND+1,pch="+",xlim=LON-360,ylim=LAT)
##abline(h=seq(LAT[1],LAT[2],YRES),lty=2,col=3)
##abline(v=seq(LON[1],LON[2],XRES)-360,lty=2,col=3)
##map("state",add=TRUE)

lsel <- which(LAND>0)
## Determine extraction location
ROW <- WTS <- list()
cnt <- 1
for(X in 1:NX){
  for(Y in 1:NY){
    ## Determine 4 nearest terrestrial neighbors for interpolation
    dist <- (YSEQ[Y]-NLAT)^2 +(XSEQ[X]-ELON)^2
    ROW[[cnt]] <- lsel[order(dist[lsel])[1:4]]
    WTS[[cnt]] <- (1/sqrt(dist[ROW[[cnt]]]))/sum(1/sqrt(dist[ROW[[cnt]]]))
    cnt <- cnt + 1
  }
}
CELL <- ROWS <- WT <- rep(NA,4*length(ROW))
for(i in 1:length(ROW)){
  j <- 1:4 +(i-1)*4
  ROWS[j] <- ROW[[i]]
  WT[j] <- WTS[[i]]
  CELL[j] <- i
}

METDATA <- list()
## loop over large files
library(hdf5,lib.loc="/home/mdietze/lib/R/Rhdf")
dt <- 10800
for(y in unique(yr)){
  ysel <- which(yr == y)
  for(m in unique(mo[ysel])){
    msel <- ysel[which(mo[ysel] == m)]    
    ssel <- msel[which(var[msel] == "sfc")]
    fsel <- msel[which(var[msel] == "flx")]

    for(i in msel){

##      METDATA <- list()
      
      ## copy file to temp space and open up
      system(paste("cp",gfiles[i],"tmp"))
      system(paste("cd tmp; tar -xf",gfiles[i]))
      system(paste("rm tmp/",gfiles[i],sep=""))
  
      ## get list of sub-files; parse
      fname <- dir("tmp","merged")
      day <- hr <- rep(NA,length(fname))
      for(j in 1:length(fname)){
        day[j] <- substr(fname[j],21,22)
        hr[j]  <- substr(fname[j],23,24)
      }
      
      ## LOOP OVER SMALL FILES
      met <- list()
      for(it in 1:11) met[[it]] <- array(NA,c(NX,NY,length(fname)))
      for(it in 1:length(fname)){
        
#######################
######  IF SFC ########
#######################
        if(var[i] == "sfc"){
          met[[1]][,,it] <- matrix(gribgrab(33,"CFRZR"),NX,NY,byrow=TRUE)
          met[[2]][,,it] <- matrix(gribgrab(32,"CICEP"),NX,NY,byrow=TRUE)
          met[[3]][,,it] <- matrix(gribgrab(34,"CRAIN"),NX,NY,byrow=TRUE)
          met[[4]][,,it] <- matrix(gribgrab(31,"CSNOW"),NX,NY,byrow=TRUE)
          met[[5]][,,it] <- matrix(gribgrab(42,"DLWRF"),NX,NY,byrow=TRUE)
          met[[6]][,,it] <- matrix(gribgrab(41,"DSWRF"),NX,NY,byrow=TRUE)
          met[[7]][,,it] <- matrix(gribgrab(3,"PRES"),NX,NY,byrow=TRUE)
          met[[8]][,,it] <- matrix(gribgrab(24,"PRATE"),NX,NY,byrow=TRUE)
          met[[9]][,,it] <- matrix(gribgrab(5,"TMP"),NX,NY,byrow=TRUE)
        } else{
          if(var[i] == "flx"){
            met[[1]][,,it]  <- matrix(gribgrab(38,"TMP10"),NX,NY,byrow=TRUE)
            met[[2]][,,it]  <- matrix(gribgrab(44,"TMP30"),NX,NY,byrow=TRUE)
            met[[3]][,,it]  <- matrix(gribgrab(35,"UGRD10"),NX,NY,byrow=TRUE)
            met[[4]][,,it]  <- matrix(gribgrab(41,"UGRD30"),NX,NY,byrow=TRUE)
            met[[5]][,,it]  <- matrix(gribgrab(36,"VGRD10"),NX,NY,byrow=TRUE)
            met[[6]][,,it]  <- matrix(gribgrab(42,"VGRD30"),NX,NY,byrow=TRUE)
            met[[7]][,,it]  <- matrix(gribgrab(40,"SPFH10"),NX,NY,byrow=TRUE)
            met[[8]][,,it]  <- matrix(gribgrab(46,"SPFH30"),NX,NY,byrow=TRUE)
            met[[9]][,,it]  <- matrix(gribgrab(39,"PRES10"),NX,NY,byrow=TRUE)
            met[[10]][,,it] <- matrix(gribgrab(45,"PRES30"),NX,NY,byrow=TRUE)
            met[[11]][,,it] <- matrix(gribgrab(4,"HGT1"),NX,NY,byrow=TRUE)
            
          } else {
            print(c("FILE TYPE UNKNOWN",gfiles[i]))
          }
        }
        system(paste("rm ",dirname,"/tmp/",fname[it],sep="")) ## clean up
      }  ## end loop within file    
      
      METDATA[[i]] <- met
    }  ## end loop over ensemble members

    print(c(y,m))
    save.image("paleo.RData")
    
    ## calculate time variables
    doy <- rep(m2d(as.numeric(m),as.numeric(y)):(m2d(as.numeric(m)+1,as.numeric(y))-1),each = 8)
    n <- length(doy)
    hr <- rep(seq(0,21,by=3),length=n)
    
#### MERGE CHUNKS INTO MONTH TIMESERIES

    SW <- prate <- dlwrf <- pres <-  ugrd <- vgrd <- sh <- tmp <- array(NA,c(NX,NY,n))
    ##sfc
    cnt <- 0
    for(j in ssel){
      rws <- cnt+1:(dim(METDATA[[j]][[1]])[3]) 
      SW[,,rws] <- METDATA[[j]][[6]]
      prate[,,rws] <- METDATA[[j]][[8]]
      dlwrf[,,rws] <- METDATA[[j]][[5]]
      cnt <- rev(rws)[1]
    }
    cnt <- 0
    for(j in fsel){
      rws <- cnt+1:(dim(METDATA[[j]][[1]])[3])       
      pres[,,rws] <- METDATA[[j]][[9]]
      ugrd[,,rws] <- METDATA[[j]][[3]]
      vgrd[,,rws] <- METDATA[[j]][[5]]
      sh[,,rws] <- METDATA[[j]][[7]]
      tmp[,,rws] <- METDATA[[j]][[1]]
      cnt <- rev(rws)[1]
    }     
    
    ## calculate potential radiation
    ## in order to estimate diffuse/direct
    f <- pi/180*(279.5+0.9856*doy)
    et <- (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(4*f)
           -429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600
           ##equation of time -> eccentricity and obliquity
    rpot <-array(NA,c(NX,NY,n))
    merid <- floor(LONgrid/15)*15
    #if(merid<0)
    merid[merid>180] <- merid[merid>180]+15-360
    lc <- (LONgrid-merid)*-4/60  ## longitude correction
    tz <- merid/360*24 ## time zone
    midbin <- 0.5*dt/86400*24 ## shift calc to middle of bin
    for(j in 1:n){
      t0 <- 12+lc-et[j]-tz-midbin   ## solar time
      h <- pi/12*(hr[j]-t0)  ## solar hour
      dec <- -23.45*pi/180*cos(2*pi*(doy[j]+10)/365)  ## declination
      cosz <- sin(LATgrid*pi/180)*sin(dec)+cos(LATgrid*pi/180)*cos(dec)*cos(h)
      cosz[cosz<0] <- 0    
      rpot[,,j] <- 1366*cosz
    }
    SW[rpot < SW] <- rpot[rpot<SW] ## ensure radiation < max
    ### this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
    frac <- SW/rpot
    frac[frac>0.9] <- 0.9  ## ensure some diffuse
    frac[frac < 0.0] <- 0.0
    frac[is.na(frac)] <- 0.0
    frac[is.nan(frac)] <- 0.0
    SWd <- SW*(1-frac)  ## Diffuse portion of total short wave rad

    nbdsf <- (SW - SWd) * 0.57# near IR beam downward solar radiation [W/m2]
    nddsf <- SWd* 0.48        # near IR diffuse downward solar radiation [W/m2]
    vbdsf <- (SW - SWd) * 0.43# visible beam downward solar radiation [W/m2]
    vddsf <- SWd * 0.52       # visible diffuse downward solar radiation [W/m2]
    
    ## write as h5
    mout <- paste(froot,"/",froot,"_",y,month[as.numeric(m)],".h5",sep="")
    hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","ugrd","vgrd","sh","tmp")
    
  }  ## end month
} ## end year
    
save.image("paleo.RData")
