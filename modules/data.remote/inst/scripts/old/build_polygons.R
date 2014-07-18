##Author: Brady S. Hardiman 04/30/2013

inpath <-"/home/bhardima/pecan/modules/data.remote/biometry"

infile <-read.csv(Sys.glob(file.path(inpath,"*.csv")), header=T) #colClasses=c("character","factor", "character", "numeric","numeric","numeric","numeric","numeric"))


# for (i in 1:length(infile)){ 
  
  coords <-cbind(infile$easting,infile$northing)
  
  Sr1<- SpatialPoints(coords,CRS("+proj=utm +zone=16 +a=6378137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0"))
#   Srs1<- Polygons(list(Sr1),"sr1")
#   SpP<-SpatialPolygons(list(Srs1))
  