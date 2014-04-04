##this script will access a palsar file on my local machine to extract radar returns from a polygon 
##associated with my UNDERC plot

##----------------------------------------------------------------------------------------------------
##Prior to running this script, use MapReady to create GeoTiffs for each band in each scene
##----------------------------------------------------------------------------------------------------


##Author: Brady S. Hardiman 04/30/2013

##load required pkgs and libraries
require(rgdal)
library(rgdal)
require(raster)
library(raster)
require(sp)
library(sp)

scn_metadata <- read.table(file="/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv", header=T, sep="\t")


##if running this as a function dir can/should be an argument
inpath <- file.path("/home/bhardima/pecan/modules/data.remote/palsar_scenes/Link_to_cheas/geo_corrected_single_gamma")
# setwd(inpath)
# Sys.glob(file.path(working_path,"*.tif")) ##This goes to a shortcut which redirects to the location of the palsar files
# filelist <- as.vector(list.dirs(path=getwd() ,recursive=F))


# for (i in 1:length(filelist)){ 

for (i in 2:4){ 
  HH_filelist <- as.vector(list.files(file.path(inpath, "HH"), pattern=".tif" ,recursive=F))
  HH_inpath <-file.path(inpath,"HH", HH_filelist[i])
  
  HV_filelist <- as.vector(list.files(file.path(inpath, "HV"), pattern=".tif" ,recursive=F))
  HV_inpath <-file.path(inpath,"HV", HV_filelist[i])

# kml   <- attributes(readOGR(Sys.glob(file.path(inpath,"*.kml")),'Layer #0')) ##read info from kml
# kml$proj4string
# bbox  <- kml$bbox

# scnHH <- Sys.glob(file.path(inpath,"*_HH.tif"))
# scnHV <- Sys.glob(file.path(inpath,"*_HV.tif"))

# scnHH <- Sys.glob(file.path(inpath,"ALPSRP071200910-H1.5_UA-zone15_HH.tif"))

# scnHH <- Sys.glob(file.path(inpath,"IMG-HH-*"))
# scnHV <- Sys.glob(file.path(inpath,"IMG-HV-*"))
# scn_dual<-raster(inpath)

HH<-raster(HH_inpath)
HV<-raster(HV_inpath)
# dataType(HH)
# trim(HH)

##UNDERC plot corner coordinates (clockwise from lower left [ie, se]). First coordinate is repeated to close the Polygon.
  coords <-rbind(
  c(303438.40, 5125111.76), 
  c(303421.10, 5125363.16),
  c(303520.75, 5125366.81), 
  c(303536.45, 5125116.63), 
  c(303438.40, 5125111.76)) 


# ## Twin Island Lake bounding box coordinates (clockwise from lower left [ie, se]). First coordinate is repeated to close the Polygon.
#   coords <-rbind(
#   c(299333.08,5121459.90),
#   c(299386.41,5123085.28),
#   c(300909.31,5123035.49),
#   c(300856.39,5121410.11),
#   c(299333.08,5121459.90)) 

##Twin Island Lake bounding box coordinates (clockwise from lower left [ie, se]). First coordinate is repeated to close the Polygon.
#   decdeg_coords <-rbind(
#   c(46.217102,-89.601745),
#   c(46.231731,-89.601745),
#   c(46.231731,-89.581994),
#   c(46.217102,-89.581994),
#   c(46.217102,-89.601745)) 

# ##Pomeroy Lake zero test (POINTS)
# coords <-rbind(
#   c(301137.43, 5127921.18),
#   c(301146.58, 5128202.28),
#   c(301720.76, 5128183.60),
#   c(301711.63, 5127902.50))
# 
# ##Pomeroy Lake zero test (POLYGON)
# coords <-rbind(
#   c(301137.43, 5127921.18),
#   c(301146.58, 5128202.28),
#   c(301720.76, 5128183.60),
#   c(301711.63, 5127902.50),
#   c(301137.43, 5127921.18))
# 
# ##Pomeroy Lake zero test (WERIRD POLYGON)
# coords <-rbind(
#   c(300432.54,5127593.85),
#   c(300564.37,5128034.23),
#   c(300830.18,5128032.83),
#   c(300856.44,5127709.75),
#   c(300432.54,5127593.85))
  
#  ##Lake Laura zero test (POLYGON)
#   coords <-rbind(
#     c(310951.00,5103162.00),
#     c(310951.00,5103477.00),
#     c(311328.00,5103477.00),
#     c(311328.00,5103162.00),
#     c(310951.00,5103162.00))

# swN <-decdeg_coords[1,1] #southwest corner easting=swE, southwest corner northing=swN
# nwN <-decdeg_coords[2,1]
# neN <-decdeg_coords[3,1]
# seN <-decdeg_coords[4,1]
# 
# swE <-decdeg_coords[1,2]
# nwE <-decdeg_coords[2,2]
# neE <-decdeg_coords[3,2]
# seE <-decdeg_coords[4,2]

##-----------------------------------------------------------------------------------------
##  add offset to compensate for lack of registration in PALSAR scenes
##----------------------------------------------------------------------------------------
# offset <- 600
# 
# coords <- cbind(coords[,1]-offset,coords[,2])



Sr1<- Polygon(coords)
Srs1<- Polygons(list(Sr1),"sr1")
SpP<-SpatialPolygons(list(Srs1))
#SpatialPolygons(lpcorners)
plotarea<-SpP@polygons[[1]]@area
  
  hh_crop <- crop(HH, SpP)
  hv_crop <- crop(HV, SpP)
  
  date<-as.character(scn_metadata$scndate[i])
  
  par(mfrow=c(1,2))
  image(hh_crop, main=paste("HH",date, sep=" "), xlab="Easting (m)",ylab="Northing (m)", col=gray(1:255/255))
  image(hv_crop, main=paste("HV",date, sep=" "), xlab="Easting (m)",ylab="Northing (m)", col=gray(1:255/255))

values<-cbind(values, as.vector(extract(HH, SpP)[[1]]), as.vector(extract(HV, SpP)[[1]]))
  
# hh_vals<-extract(HH, coords, method='simple', cellnumbers=T)
# hv_vals<-extract(HV, coords, method='simple', cellnumbers=T)

}

par(mfrow=c(2,3))
plot(values[,1],values[,3])
plot(values[,1],values[,5])
plot(values[,3],values[,5])
plot(values[,2],values[,4])
plot(values[,2],values[,6])
plot(values[,4],values[,6])





