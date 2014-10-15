palsar_inpath <- file.path("/Users/hardimanb/Desktop/data.remote/palsar_scenes/geo_corrected_single_gamma") ##For Mac
pol_bands<-c("HH", "HV")

filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[2]), pattern=".tif" ,recursive=F))

inpath<-file.path(palsar_inpath,pol_bands[2],filelist[19])
rast<-raster(inpath)

coords<-data.frame(297973.08,5088571.03)
Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
coords<-Sr1@coords

radius<-875
buffext<-as.vector(disc(radius=radius, centre=coords[1,])) ##10m is the smallest buffer size that overlaps >1 cell
ext<-extent(c(buffext[[2]],buffext[[3]])) 

image(crop(rast,ext),col=gray(1:255/255))








disturbance_inpath <-"/Users/hardimanb/Desktop/data.remote/biometry" ##For Mac
# disturbance_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry"

disturbance_infile <-read.csv(file.path(disturbance_inpath,"Cheas_coordinates_disturbance_year.csv"), sep=",", header=T) #disturbance plots

disturbance_coords<-data.frame(cbind(-1*disturbance_infile$dec_lon,disturbance_infile$dec_lat))
dist_df<-data.frame(disturbance_infile$distyr)

coordinates(dist_df)<-disturbance_coords #lat and lon (dec deg)

##Convert to class=SpatialPoints for palsar extraction (this is used for creating kml files and will be used later on)
disturbance_Sr1<- SpatialPoints(dist_df,CRS(as.character(NA)))
