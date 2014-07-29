#Author: Brady S. Hardiman 04/30/2013
require(rgdal)
require(raster)
require(sp)
require(RgoogleMaps)
require(maptools)
require(ggplot2)
require(car)


#####################################################################

##reading points corresponding to centroids of WLEF/Park Falls Ameriflux tower biometry plots

#####################################################################
calib_inpath <-"/Users/hardimanb/Desktop/data.remote/biometry" ##For Mac
# calib_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry"

calib_infile <-read.csv(file.path(calib_inpath,"biometry_trimmed.csv"), sep="\t", header=T) #WLEF plots

coords<-data.frame(calib_infile$easting,calib_infile$northing) #eastings and northings (UTM Zone 15N NAD83)

##Convert to class=SpatialPoints for palsar extraction (this is used for creating kml files and will be used later on)
Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +a=6378137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##reproject to lat lon for export to kml
epsg4326String <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Sr1_4google <- spTransform(Sr1,epsg4326String) #class=SpatialPoints
wlef<-data.frame(paste(calib_infile$plot,calib_infile$subplot,sep="_"))

Sr1_4google <- SpatialPointsDataFrame(Sr1_4google, wlef) #convert to class="SpatialPointsDataFrame" for export as kml

writeOGR(Sr1_4google, layer=1, "WLEF.kml", driver="KML") #export as kml (this puts in in the Home folder)

# #####################################################################
# 
# ##reading points corresponding to centroids of areas of disturbance (ID'd from Landtrendr, courtesy Robert Kennedy and Neeti)
# 
# #####################################################################
# disturbance_inpath <-"/Users/hardimanb/Desktop/data.remote/biometry" ##For Mac
# # disturbance_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry"
# 
# disturbance_infile <-read.csv(file.path(disturbance_inpath,"Cheas_coordinates_disturbance_year.csv"), sep=",", header=T) #disturbance plots
# 
# disturbance_coords<-data.frame(cbind(-1*disturbance_infile$dec_lon,disturbance_infile$dec_lat))
# dist_df<-data.frame(disturbance_infile$distyr)
# 
# coordinates(dist_df)<-disturbance_coords #lat and lon (dec deg)
# 
# ##Convert to class=SpatialPoints for palsar extraction (this is used for creating kml files and will be used later on)
# disturbance_Sr1<- SpatialPoints(dist_df,CRS(as.character(NA)))
# 
# proj4string(disturbance_Sr1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# 
# ##reproject to lat lon for export to kml
# # epsg4326String <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# # disturbance_Sr1_4google <- spTransform(disturbance_Sr1,epsg4326String) #class=SpatialPoints
# # landtrendr_disturbances<-data.frame(paste(infile$plot,infile$subplot,sep="_"))
# 
# # disturbance_Sr1_4google <- SpatialPointsDataFrame(dist_df, disturbance_coords, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) #convert to class="SpatialPointsDataFrame" for export as kml
# 
# writeOGR(dist_df, layer=1, "landtrendr_disturbances.kml", driver="KML") #export as kml (this puts in in the Home folder)
# 
# #####################################################################

##extract palsar data from points that fall in the center of large lakes within PALSAR scenes containing the
##WLEF plots. These should be ~zero (or at least approximate a minimum value that is not subject to annual biomass increment).
##These values should be subtracted from the HH and HV values extracted from the WLEF calibration points to remove noise.

# #####################################################################
# lakes <-file.path("/Users/hardimanb/Desktop/data.remote/plot_coords/Lakes.kml") ##For Mac
# # lakes<-file.path("/home/bhardima/pecan/modules/data.remote/plot_coords/Lakes.kml")
# lake_coord_list<-getKMLcoordinates(lakes,ignoreAltitude=FALSE)
# lake_pts<-data.frame()
# for(i in 1:length(lake_coord_list)){
#   lake_pts<-rbind(lake_pts,lake_coord_list[[i]])
# }
# lake_pts<-lake_pts[,1:2]
# 
# lake_coords<-coordinates(lake_pts) #lon and lat (dec deg)
# 
# ##Convert to class=SpatialPoints for palsar extraction 
# lake_Sr1<- SpatialPoints(lake_coords,CRS(as.character(NA)))
# 
# proj4string(lake_Sr1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# #####################################################################

##extract palsar data from polygons centered on WLEF plot coordinates
##WLEF plots only occur in the 2 westernmost palsar scenes (#'s 1 &5 on the list below)

#####################################################################
palsar_inpath <- file.path("/Users/hardimanb/Desktop/data.remote/palsar_scenes/geo_corrected_single_gamma") ##For Mac
# palsar_inpath <- file.path("/home/bhardima/pecan/modules/data.remote/palsar_scenes/Link_to_cheas/geo_corrected_single_gamma")

file.info<-read.table(file="/Users/hardimanb/Desktop/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t") ##For Mac
# file.info<-read.table(file="/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t")

date.time<-as.vector(substr(file.info$scndate,1,8))
col_names<-c(rbind(paste(date.time, "HH",sep="_"),paste(date.time, "HV",sep="_")))

pol_bands<-c("HH", "HV")
numfiles<-length(date.time)

extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
lake_extracted<-matrix(NA, nrow(lake_coords),length(pol_bands)*numfiles)
disturbance_extracted<-matrix(NA, nrow(disturbance_coords),length(pol_bands)*numfiles)

colnames(extracted)<-col_names
colnames(lake_extracted)<-col_names
colnames(disturbance_extracted)<-col_names

for(i in 1:numfiles){
  for(j in 1:2){        
    
    filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[j]), pattern=".tif" ,recursive=F))
    inpath<-file.path(palsar_inpath,pol_bands[j],filelist[i])
    rast<-raster(inpath)
    
    #        if(as.numeric(substr(projection(rast),17,18)) == as.numeric(substr(projection(Sr1),17,18))){
    
    ################################
    ##calibration data from WLEF plots
    ################################
          data<-extract(rast, Sr1, method="simple",buffer=7, small=T, fun=mean)
          cols<-seq(j,ncol(extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
          extracted[,cols[i]]<-data
    
    #       ################################
    #       ##data from lakes (n=25) in scenes containing WLEF plots
    #       ################################
#     lake_data<-extract(rast, lake_Sr1, method="simple",buffer=20, small=T, fun=mean)
#     lake_cols<-seq(j,ncol(lake_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#     lake_extracted[,lake_cols[i]]<-lake_data
    #       
    #       ################################
    #       ##data from LandTrendr disturbance plots
    #       ################################
    #       disturbance_data<-extract(rast, disturbance_Sr1, method="simple",buffer=20, small=T, fun=mean)
    #       disturbance_cols<-seq(j,ncol(disturbance_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
    #       disturbance_extracted[,disturbance_cols[i]]<-disturbance_data
    #       
    print(paste("i=",i,sep=""))
    print(paste("j=",j,sep=""))
    #       }
  }  
}
extracted <- extracted[ , !apply(is.na(extracted), 2, all)] 
lake_extracted <- lake_extracted[ , !apply(is.na(lake_extracted), 2, all)] 
disturbance_extracted <- disturbance_extracted[ , !apply(is.na(disturbance_extracted), 2, all)] 

head(extracted)
head(lake_extracted)
head(disturbance_extracted)

##For mac
write.table(extracted,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_buff7.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(lake_extracted,file="/Users/hardimanb/Desktop/data.remote/output/data/lake_extracted_buff7.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(disturbance_extracted,file="/Users/hardimanb/Desktop/data.remote/output/data/disturbance_extracted_buff7.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
extracted_buff7<-extracted
extracted_buff7 <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted.csv",sep="\t", header=T)
lake_extracted_buff7 <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/lake_extracted.csv",sep="\t", header=T)
disturbance_extracted_buff7 <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/disturbance_extracted.csv",sep="\t", header=T)

#####################################################################
odds<-seq(1,ncol(extracted_buff7),by=2)
evens<-seq(2,ncol(extracted_buff7),by=2)
HHscn.dates<-as.Date(substr(col_names[odds],1,8),"%Y%m%d")
HVscn.dates<-as.Date(substr(col_names[evens],1,8),"%Y%m%d")

HH_wlef<-extracted_buff7[,odds]
HV_wlef<-extracted_buff7[,evens]
#####################################################################

################################
##plot calibration data from WLEF plots
################################
boxplot(HV_wlef)
plot(HVscn.dates,HV_wlef[1,])
lines(cbind(HVscn.dates,HV_wlef[,]),col=3,lwd=3)

wlef_abg<-read.csv("/Users/hardimanb/Desktop/data.remote/biometry/biometry_trimmed.csv", sep="\t", header=T)
par(mfrow=c(3,length(evens)/3)) ##scatterplots of WLEF with noise NOT subtracted
for(i in 1:ncol(HV_wlef)){
      plot(wlef_abg$ABG_biomass,extracted_buff7[,i], xlab="ABG-biomass", ylab="HV", main=col_names[i])
    par(new=F)
}




