##this script will access a palsar file on my local machine to extract radar returns from a polygon 
##associated with my UNDERC plot

##I'm going to try doing this with rgdal instead of raster package since that seems to be producing nonsense

##Author: Brady S. Hardiman 04/30/2013

##load required pkgs and libraries
require(rgdal)
library(rgdal)
require(sp)
library(sp)

scn_metadata <- read.table(file="/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv", header=T, sep="\t")


##if running this as a function dir can/should be an argument
setwd("/home/bhardima/pecan/modules/data.remote/palsar_scenes/UNDERC/")
filelist <- as.vector(list.dirs(path=getwd() ,recursive=F))
# for (i in 1:length(filelist)){ 
  i=1
  inpath <-filelist[i]

scnHH <- Sys.glob(file.path(inpath,"*_HH.tif"))
scnHV <- Sys.glob(file.path(inpath,"*_HV.tif"))

kml   <- attributes(readOGR(Sys.glob(file.path(inpath,"*.kml")),'Layer #0')) ##read info from kml
bbox  <- kml$bbox

CRS(kml$proj4string)
proj  <- CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 ")
# 
# rasterHH <- raster(scnHH)
# rasterHV <- raster(scnHV)

HH<-readGDAL(scnHH)  ##alt: use readGDAL?
# spTransform(HH,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "))
 
HV<-GDAL.open(scnHV)

#displayDataset(x, offset=c(0, 0), region.dim=dim(x), reduction = 1,band = 1, col = NULL, reset.par = TRUE, max.dim = 500, ...)

#getColorTable(HH, band = 1)




dim(HH)
trim(HH)
projectRaster(HH)
hh<-getRasterTable(HH, band = 1, offset = c(140,650),region.dim = c(250,250)) ##origin is at bottom right (se corner)
#region.dim = (0.03*dim(HH))
x<-unique(hh$x)
y<-unique(hh$y)
hh_mat<-matrix(NA,length(x),length(y),)
for(j in 1:length(x)){
  for(k in 1:length(y)){
    hh_mat[j,k]<-hh$band1[hh$y==y[k] & hh$x==x[j]]
    #hv_mat[j,k]<-hv$band1[hv$x==x[j] & hv$y==y[k]]
  }
  print(c("j=",100-(100*(j/length(x)))))
}
image(hh_mat, col=gray(1:255/255))


plot(density(hh_mat))
plot(hh[1:10,1],hh[1:10,2])
plot(hh[seq(1,nrow(hh),by=100),1],hh[seq(1,nrow(hh),by=100),3])
plot(hh[seq(1,nrow(hh),by=100),2],hh[seq(1,nrow(hh),by=100),3])


