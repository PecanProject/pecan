## Code to convert most recent SREF files into met drivers and figures

##################################
##                              ##
##    SINGLE SITE TIMESERIES    ## 
##                              ##
##################################
## how to extract a specific cell

#GLOBAL CONSTANTS (pun intended)
daysInYear=365.25
secsInHour=60*60
secsInDay=secsInHour*24
axialTilt=23.45

LAT <- 68.0+38.0/60.0
LON <- 360-(149	+36.0/60.0)

# TIME TO GRAB
startyear <- 2010
stopyear <- 2011

timestep <- 3*secsInHour
timelag <- floor(LON/15)/24  # difference from GMT (fractional day)

narrdir <- "/home/scratch/dietze_lab/NARR/"
nomaddir <- "/home/scratch/dietze_lab/NOMADS/"
tempdir  <-paste(narrdir, '/toolik.obs.rad/', sep='')
outdir <- "toolik.obs.rad"


#IO OPERATIONS
readGrib<-function(indices, gribfile, tempfile=paste(tempdir,'GRIB.txt', sep='')){
	command<-paste(nomaddir,"/wgrib -s -d ",indices,' ', gribfile," -text -h -o ", tempfile, sep='')
	system(command, intern=TRUE)
	V <- read.table(paste(tempfile,sep=""),skip=1,header=FALSE)[[1]]
	system(paste("rm ",tempfile,sep=""))
	return(V)
}
readMetGrib <- function(indices, gribfile, 
		nearestCells, weights){
        print(gribfile)
	V<-readGrib(indices, gribfile)
	if(length(V)> 0){
		return(sum(V[nearestCells]*weights,na.rm=TRUE))
	}
	return(NA)
}
extractTar<-function(tarfile, sourcedir, targetdir){
	system(paste("tar -xf ", sourcedir, tarfile, ' -C ', targetdir, sep=''), intern=TRUE)
}
readMetTar<-function(tarfile, indices, 
		nearestCells, weights,
		sourcedir=narrdir, targetdir=tempdir){
        print(tarfile)
	#returns meteorological data from a tar file 
	#as a 2D matrix of parameters and tar subfiles
	
        system(paste("rm ",targetdir,'/merged_AWIP32*',sep="")) # clean up
	# copy file to temp space and open up
	extractTar(tarfile, sourcedir, targetdir)
	
	# get list of sub-files; parse
	subfiles <- dir(targetdir,"merged")
	subfiles <- paste(targetdir, subfiles, sep='')
	
	## LOOP OVER SMALL FILES
	tarMetData <- matrix(NA,nrow=length(subfiles),ncol=length(indices))
	if (length(indices) > 0){
		for(i in 1:length(subfiles)){
			for(k in 1:length(indices)){
				tarMetData[i,k] <- readMetGrib(indices[k],subfiles[i],nearestCells, weights)
			}
			system(paste("rm ",subfiles[i],sep="")) # clean up
		}# end loop within file
	}
	tarMetData
}
readMetTars<-function(tarfiles, indices, 
		nearestCells, weights){
	# returns meteorological data from a list of tar files
	# bound into a single 2 dimensional matrix representing 4 dimensions
	# each column represents a parameter
	# each row represents a lat/lon coordinate at a specific time
	foo<-sapply(tarfiles, 
		function(tarfile){readMetTar(tarfile, indices, nearestCells, weights)})
        if(!is.list(foo)){print(foo);browser()}
        print(tarfiles)
        do.call(rbind, foo)
}
writeHdf5<-function(file, metdata, potential, downscale.radiation=function(x){x}) {

        as.met.array <- function(x) {array(x, dim=c(1,1,length(x)))}

	shortWave <- monthMetData[,17]
	shortWave[potential < shortWave] <- potential[potential<shortWave] # ensure radiation < max
	shortWaveDiffuse <- shortWaveDiffuseRad(potential,shortWave)

	nbdsf <- as.met.array((shortWave - shortWaveDiffuse) * 0.57) # near IR beam downward solar radiation [W/m2]
	nddsf <- as.met.array(shortWaveDiffuse * 0.48)        # near IR diffuse downward solar radiation [W/m2]
	vbdsf <- as.met.array((shortWave - shortWaveDiffuse) * 0.43) # visible beam downward solar radiation [W/m2]
	vddsf <- as.met.array(shortWaveDiffuse * 0.52)        # visible diffuse downward solar radiation [W/m2]
	prate <- as.met.array(metdata[,19]) # precipitation rate [kg_H2O/m2/s]
	dlwrf <- as.met.array(metdata[,16]) # downward long wave radiation [W/m2]
	pres  <- as.met.array(metdata[,18]) # pressure [Pa]
	hgt   <- as.met.array(rep(50,nrow(monthMetData)))   # geopotential height [m]
	ugrd  <- as.met.array(metdata[,3])  # zonal wind [m/s]
	vgrd  <- as.met.array(metdata[,5])  # meridional wind [m/s]
	sh    <- as.met.array(metdata[,7])  # specific humidity [kg_H2O/kg_air]
	tmp   <- as.met.array(metdata[,1])  # temperature [K]

	browser()

	# downscale radiation
	nbdsf <- as.met.array(downscale.radiation(nbdsf))
	nddsf <- as.met.array(downscale.radiation(nddsf))
	vbdsf <- as.met.array(downscale.radiation(vbdsf))
	vddsf <- as.met.array(downscale.radiation(vddsf))
	
	hdf5save(file,"nbdsf","nddsf","vbdsf","vddsf","prate","dlwrf","pres","hgt","ugrd","vgrd","sh","tmp")
}


# MONTH PROCESSING CODE
monthnames <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
monthlengths = c(31,28,31,30,31,30,31,31,30,31,30,31)
monthlengthsLeap = monthlengths
monthlengthsLeap[2] = monthlengthsLeap[2]+1
monthlength <- function(month,year){
	if(year %% 4 == 0){
		return(monthlengthsLeap[month])
	}
	return(monthlengths[month])
}
firstday <- function(month,year){
	sum(monthlength(0:(month-1), year))
}
lastday <- function(month, year){
	sum(monthlength(0:(month), year))-1
}


smoothedRadiation <- function(a, month, year, timelag, new.timestep,old.timestep){  ## radiation
	k1 = c(1/6,1/2,1/2,-1/6)
	k2 = c(-1/6,1/2,1/2,1/6)
	dat <- rep(a,each=old.timestep/new.timestep)
	lab <- rep(seq(a),each=old.timestep/new.timestep)
	startday <- firstday(month,year)
	startday <- lastday(month,year)
	rin <- potentialRadiation(rep(startday:startday,each=secsInDay/new.timestep),
                                  rep(seq(new.timestep,secsInDay,new.timestep),
                                  monthlength(mo,year)), LAT, timelag*24)
	rbar <- rep(tapply(rin,lab,mean),each=old.timestep/new.timestep)
	r <-apply(cbind(dat*rin/rbar,rep(0,length(dat))),1,max)  
	r[rbar == 0] <- 0

	## filter
	bound <- which(diff(lab)>0)
	bound <- bound[rin[bound] > 0 & rin[bound+1] >0]
	for(i in bound){
		x1 = sum(k1*r[i + 1:4 -2 ])
		x2 = sum(k2*r[i + 1:4 -2 ])
		r[i] = x1
		r[i+1]=x2
	}

	return(r)
}

deg2rad<-function(degrees){
	pi/180 * degrees
}
rad2deg<-function(radians){
	180/pi * radians
}
potentialRadiation <- function(day,time,LAT,timelag){
	#radiation as determined only by solar position
	dayangle=2.0*pi*(day)/daysInYear
	declination = 0.006918 - 
			0.399912 * cos(dayangle)+
			0.070257 * sin(dayangle)-
			0.006758 * cos(2.0*dayangle)+
			0.000907 * sin(2.0*dayangle)-
			0.002697 * cos(3.0*dayangle)+
			0.00148  * sin(3.0*dayangle)
	eccentricity=1.00011  +
			0.034221 * cos(dayangle)+
			0.00128  * sin(dayangle)+
			0.000719 * cos(2.0*dayangle)+
			0.000077 * sin(2.0*dayangle)
	solartime=time/secsInHour-12.0+timelag
	radiation = 1367.0 * 
			eccentricity * 
			(cos(declination) * 
				cos(deg2rad(LAT)) * 
				cos(deg2rad(15.0)*(solartime)) +
				sin(declination) * 
				sin(deg2rad(LAT)))
	radiation[radiation<0] <- 0
	radiation
}
potentialRadiation2<-function(lat, lon, day, hours){
	if(lon>180) lon <- lon-360
	f <- deg2rad(279.5+0.9856*day)
	eccentricity <- (-104.7*sin(f)
				+596.2*sin(2*f)
				+4.3*sin(4*f)
				-429.3*cos(f)
				-2.0*cos(2*f)
				+19.3*cos(3*f))/3600
	#equation of time -> eccentricity and obliquity
	meridian <- floor(lon/15)*15
	if(meridian<0) meridian <- meridian+15
	lonCorrection <- (lon-meridian)*-4/60 
	timeZone <- meridian/360*24
	midbin <- 0.5*timestep/secsInHour # shift calc to middle of bin
	solarTime <- 12+lonCorrection-eccentricity-timeZone-midbin
	solarHour <- pi/12*(hours-solarTime)
	dayangle<-2*pi*(day+10)/daysInYear
	declenation <- -deg2rad(axialTilt) * cos(dayangle)
	cosz <- sin(deg2rad(lat))*sin(declenation)+cos(deg2rad(lat))*cos(declenation)*cos(solarHour)
	cosz[cosz<0] <- 0
	1366*cosz
}
shortWaveDiffuseRad<-function(potentialRad, shortWaveRad){
	## this causes trouble at twilight bc of missmatch btw bin avergage and bin midpoint
	frac <- shortWaveRad/potentialRad
	frac[frac>0.9] <- 0.9  # ensure some diffuse
	frac[frac < 0.0] <- 0.0
	frac[is.na(frac)] <- 0.0
	frac[is.nan(frac)] <- 0.0
	shortWaveRad*(1-frac)  # return diffuse portion of total short wave rad
}


# check if GRIB files are there yet
tarfiles <- dir(narrdir,"NARR.*\\.tar$")
if(length(tarfiles) == 0) stop()
years<-substr(tarfiles, 9, 12)
tarfiles<-tarfiles[which(years>=startyear & years<=stopyear)]
vars =substr(tarfiles,5,7)
years=substr(tarfiles,9,12)
months=substr(tarfiles,13,14)

# load maps
mapfile <- paste(narrdir, "rr-fixed.grb", sep='')
NLAT <- readGrib(20, mapfile)
ELON <- readGrib(19, mapfile)
isLand <- readGrib(16, mapfile)
NLAT[NLAT>1.0e20] <- NA
ELON[ELON>1.0e20] <- NA
isLand[isLand>1.0e20] <- NA

landCells <- which(isLand>0)
# Determine extraction location
distance <- (LAT-NLAT)^2 +(LON-ELON)^2
nearestCell <- which.min(distance)
# Determine 4 nearest neighbors for interpolation
nearestCells <- landCells[order(distance[landCells])[1:4]]
nearestCells <- nearestCells[which(isLand[nearestCells] > 0)]
if(length(nearestCells) == 0) nearestCells = nearestCell
weights <- (1/sqrt(distance[nearestCells]))/sum(1/sqrt(distance[nearestCells]))
#print(weights)
#stop()

# loop over large files
#library(hdf5,lib.loc="/home/mdietze/lib/R/Rhdf")
library(hdf5)
for(year in unique(years)){
	yearTars <- which(years == year)
	yearnum<-as.numeric(year)
	for(month in c('08','09','10','11','12')){
                browser()
		monthTars <- yearTars[which(months[yearTars] == month)]
		monthnum<-as.numeric(month)
		print(paste(year,month))
		
		surfaceTars <- monthTars[which(vars[monthTars] == "sfc")]
		surfaceMetData<-readMetTars(tarfiles[surfaceTars],
				list(cfrzr=33,	cicep=32,	crain=34,	csnow=31,	dlwrf=42,	
						dswrf=41,	pres =3,	prate=24,	tmp  =5),
				nearestCells, weights)
		
		fluxTars <- monthTars[which(vars[monthTars] == "flx")]
		fluxMetData<-readMetTars(tarfiles[fluxTars],
				list(tmp10=38,	tmp30=44,	ugrd10=35,	ugrd30=41,	vgrd10=36,
						vgrd30=42,	spfh10=40,	spfh30=46,	pres10=39,	pres30=45,
						hgt1  =4),
				nearestCells, weights)
		monthMetData <- cbind(fluxMetData,surfaceMetData)
		startday<-firstday(monthnum,yearnum)+1
		stopday<-lastday(monthnum,yearnum)+1
		days <- rep(startday:stopday, each = 24/3)
		hours <- rep(seq(0,21,by=3),length=nrow(monthMetData))
		
		potential<-potentialRadiation2(LAT,LON,days, hours)
		# write as h5
		out.file <- paste(narrdir, outdir,"/",
		                  outdir,"_", yearnum,
		                  monthnames[monthnum],".h5",sep="")
		print(out.file)
		writeHdf5(out.file, monthMetData, potential, 
                          downscale.radiation=function(x){x})
                          #downscale.radiation=function(x){smoothedRadiation(x, monthnum, yearnum, timelag, new.timestep=900, old.timestep=timestep)})
	}  # end month
} # end year


