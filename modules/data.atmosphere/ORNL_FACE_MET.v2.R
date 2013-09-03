
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
num <- function(d){as.numeric(as.character(d))}

checkNA <- function(val,master){
  sNA <- which(is.na(val))
  if(length(sNA) == 0) return(val)
  ## translate NA's
  mNA <- round(sNA*length(master)/length(val))
  val[sNA] <- master[mNA]
  return(val)
}


lat <- 35.90
lon <- -84.33

fname <- "NCDF/ORNL_met_halfhourly.txt"
dat <- read.table(fname,header=TRUE)

  n      <- nrow(dat)
  tmpA   <- num(dat$Tair.)
  ugrdA  <- num(dat$wind.)
  vgrdA  <- rep(0.0,n)
  prateA <- num(dat$Rainf.)
  hgtA   <- rep(50,n)         # geopotential height [m]
  SW     <- num(dat$SWdown.)  #w/m2
  shA    <- num(dat$Qair.) 
  dlwrfA <- num(dat$LWdown.)  # downward long wave radiation [W/m2]
  presA  <- num(dat$PSurf.)   # pressure [Pa]

  ### RADIATION CALCULATION
  ##build time variables (year, month, day of year)
  dt <- 3600
  yr <- num(dat$year.)
  doy <- num(dat$doy.)
  hr <- num(dat$hod.)
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
  t0 <- 12+lc-et-tz-midbin   ## solar time
  h <- pi/12*(hr-t0)  ## solar hour
  dec <- -23.45*pi/180*cos(2*pi*(doy+10)/365)  ## declination

  cosz <- sin(lat*pi/180)*sin(dec)+cos(lat*pi/180)*cos(dec)*cos(h)
  cosz[cosz<0] <- 0
  
  rpot <- 1366*cosz
  rpot <- rpot[1:n]
  rpotL <-(rpot[c(9:n,1:8)])#rad in local time
  
  SW[rpotL < SW] <- rpotL[rpotL<SW] ## ensure radiation < max
      ### this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
  frac <- SW/rpotL
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

cname <- dir(".","co2_gap")
cname <- cname[grep("dat",cname)]
for(i in 1:length(cname)){

## Load CO2 file
  cyr <- num(substr(cname,9,12))[i]
  year = cyr
  co2file <- read.table(cname[i],skip=9)
  r1 <- co2file[co2file[,4] == 1,5]
  r2 <- co2file[co2file[,4] == 2,5]
  r4 <- co2file[co2file[,4] == 4,5]
  r5 <- co2file[co2file[,4] == 5,5]
  cdoy <- co2file[co2file[,4] == 1,2]
  cmo <- day2mo(cyr,cdoy)
  ELEV <- 0.5*(r1+r2)
  AMB  <- 0.5*(r4+r5)

  ELEV[ELEV > 700] <- 550
  AMB[AMB > 700] <- 360

  
  sely = which(yr == cyr)
  for(m in unique(mo)){
    selm <- sely[which(mo[sely] == m)]-8
    selm[selm < 1] <- 1
    dims <- c(1,1,length(selm))
    nbdsf <- array(nbdsfA[selm],dim=dims)
    nddsf <- array(nddsfA[selm],dim=dims)
    vbdsf <- array(vbdsfA[selm],dim=dims)
    vddsf <- array(vddsfA[selm],dim=dims)
    prate <- array(prateA[selm],dim=dims)
#    dlwrf <- array(dlwrfA[selm],dim=dims)
#    pres  <- array(presA[selm],dim=dims)
    hgt   <- array(hgtA[selm],dim=dims)
    ugrd  <- array(ugrdA[selm],dim=dims)
    vgrd  <- array(vgrdA[selm],dim=dims)
    sh    <- array(shA[selm],dim=dims)
    tmp   <- array(tmpA[selm],dim=dims)
#    co2   <- array(co2A[selm],dim=dims)
   
    ## grab & fill in other vars
##    ncep <- read.table(paste("ncep/",mon_num[m],year,".dat",sep=""))
##    dlwrf <- rep(ncep[,11],each=6)
##    pres  <- 1e5*(rep(ncep[,6],each=6)/1004.)^(1004./287.)  ## exner -> pres
##    narr <- hdf5load(paste("NARR/ORNL_",year,month[m],".h5",sep=""),load=FALSE)
##    dlwrf <- narr$dlwrf
##    pres <- narr$pres
    dlwrf <- array(dlwrfA[selm],dim=dims)
    pres  <- array(presA[selm],dim=dims)

    ## fill missing
    nbdsf <- checkNA(nbdsf,narr$nbdsf)
    nddsf <- checkNA(nddsf,narr$nddsf)
    vbdsf <- checkNA(vbdsf,narr$vbdsf)
    vddsf <- checkNA(vddsf,narr$vddsf)
    prate <- checkNA(prate,narr$prate)
    hgt   <- checkNA(hgt,narr$hgt)
    ugrd  <- checkNA(ugrd,sqrt(narr$ugrd^2+narr$vgrd^2))
    sh    <- checkNA(sh,narr$sh)
    tmp   <- checkNA(tmp,narr$tmp)

    selcm <- which(cmo == m) -4
    selcm[selcm < 1] <- 1
    ##ambient
    co2 <- array(AMB[selcm],dim=c(1,1,length(selcm)))
    mout <- paste("NCDF/AMB_",year,month[m],".h5",sep="")    
    hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt"
             ,"ugrd","vgrd","sh","tmp","co2")
    ## elevated
    co2 <- array(ELEV[selcm],dim=c(1,1,length(selcm)))
    mout <- paste("NCDF/ELEV_",year,month[m],".h5",sep="")    
    hdf5save(mout,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt"
             ,"ugrd","vgrd","sh","tmp","co2")
    
  }
}
