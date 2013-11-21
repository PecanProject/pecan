##Author: Brady S. Hardiman 04/30/2013
require(rgdal)
require(raster)
require(sp)
require(RgoogleMaps)
require(maptools)
require(ggplot2)
require(car)
require(spatsta)


#####################################################################

##reading points corresponding to centroids of WLEF/Park Falls Ameriflux tower biometry plots

#####################################################################
# calib_inpath <-"/Users/hardimanb/Desktop/data.remote/biometry" ##For Mac
calib_inpath <-"/home/bhardima/git/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords"

calib_infile <-read.csv(file.path(calib_inpath,"biometry_trimmed.csv"), sep=",", header=T) #WLEF plots

coords<-data.frame(calib_infile$easting,calib_infile$northing) #eastings and northings (ChEAS: UTM Zone 15N NAD83)

##Convert to class=SpatialPoints for palsar extraction (this is used for creating kml files and will be used later on)
# Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +a=6378137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##reproject to lat lon for export to kml
epsg4326String <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Sr1_4google <- spTransform(Sr1,epsg4326String) #class=SpatialPoints
wlef<-data.frame(paste(calib_infile$plot,calib_infile$subplot,sep="_"))

Sr1_4google <- SpatialPointsDataFrame(Sr1_4google, wlef) #convert to class="SpatialPointsDataFrame" for export as kml

writeOGR(Sr1_4google, layer=1, "WLEF.kml", driver="KML") #export as kml (this puts in in the Home folder)

#####################################################################

##reading points corresponding to centroids of areas of disturbance (ID'd from Landtrendr, courtesy Robert Kennedy and Neeti)

#####################################################################
disturbance_inpath <-"/Users/hardimanb/Desktop/data.remote/biometry" ##For Mac
# disturbance_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry"

disturbance_infile <-read.csv(file.path(disturbance_inpath,"Cheas_coordinates_disturbance_year.csv"), sep=",", header=T) #disturbance plots

disturbance_coords<-data.frame(cbind(-1*disturbance_infile$dec_lon,disturbance_infile$dec_lat))
dist_df<-data.frame(disturbance_infile$distyr)

coordinates(dist_df)<-disturbance_coords #lat and lon (dec deg)

##Convert to class=SpatialPoints for palsar extraction (this is used for creating kml files and will be used later on)
disturbance_Sr1<- SpatialPoints(dist_df,CRS(as.character(NA)))

proj4string(disturbance_Sr1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

##reproject to lat lon for export to kml
# epsg4326String <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# disturbance_Sr1_4google <- spTransform(disturbance_Sr1,epsg4326String) #class=SpatialPoints
# landtrendr_disturbances<-data.frame(paste(infile$plot,infile$subplot,sep="_"))

# disturbance_Sr1_4google <- SpatialPointsDataFrame(dist_df, disturbance_coords, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) #convert to class="SpatialPointsDataFrame" for export as kml

writeOGR(dist_df, layer=1, "landtrendr_disturbances.kml", driver="KML") #export as kml (this puts in in the Home folder)

#####################################################################

##extract palsar data from points that fall in the center of large lakes within PALSAR scenes containing the
##WLEF plots. These should be ~zero (or at least approximate a minimum value that is not subject to annual biomass increment).
##These values should be subtracted from the HH and HV values extracted from the WLEF calibration points to remove noise.

#####################################################################
lakes <-file.path("/Users/hardimanb/Desktop/data.remote/plot_coords/Lakes.kml") ##For Mac
# lakes<-file.path("/home/bhardima/pecan/modules/data.remote/plot_coords/Lakes.kml")
lake_coord_list<-getKMLcoordinates(lakes,ignoreAltitude=FALSE)
lake_pts<-data.frame()
for(i in 1:length(lake_coord_list)){
  lake_pts<-rbind(lake_pts,lake_coord_list[[i]])
}
lake_pts<-lake_pts[,1:2]

lake_coords<-coordinates(lake_pts) #lon and lat (dec deg)

##Convert to class=SpatialPoints for palsar extraction 
lake_Sr1<- SpatialPoints(lake_coords,CRS(as.character(NA)))

proj4string(lake_Sr1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#####################################################################

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

lake_extracted<-matrix(NA, nrow(lake_coords),length(pol_bands)*numfiles)
disturbance_extracted_40m<-matrix(NA, nrow(disturbance_coords),length(pol_bands)*numfiles)

colnames(lake_extracted)<-col_names
colnames(disturbance_extracted)<-col_names
colnames(disturbance_extracted_40m)<-col_names


extracted_7m<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted_40m<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands

colnames(extracted_7m)<-col_names
colnames(extracted)<-col_names
colnames(extracted_40m)<-col_names



for(i in 1:numfiles){
    for(j in 1:2){        
  
      filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[j]), pattern=".tif" ,recursive=F))
      inpath<-file.path(palsar_inpath,pol_bands[j],filelist[i])
      rast<-raster(inpath)
      
#       ################################
#       ##calibration data from WLEF plots 7m BUFFER MEAN
#       ################################
#       data_7m<-extract(rast, Sr1, method="simple",buffer=7, small=T, fun=mean)
#       cols<-seq(j,ncol(extracted_7m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#       extracted_7m[,cols[i]]<-data_7m
#     
#       ###############################
#       #calibration data from WLEF plots 20m BUFFER MEAN
#       ###############################
#       data_20m<-extract(rast, Sr1, method="simple",buffer=20, small=T, fun=mean)
#       cols<-seq(j,ncol(extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#       extracted[,cols[i]]<-data_20m
#       
#       ################################
#       ##calibration data from WLEF plots 40m BUFFER MEAN
#       ################################
#       data_40m<-extract(rast, Sr1, method="simple",buffer=40, small=T, fun=mean)
#       cols<-seq(j,ncol(extracted_40m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#       extracted_40m[,cols[i]]<-data_40m
      
#       ################################
#       ##data from lakes (n=25) in scenes containing WLEF plots
#       ################################
#       lake_data<-extract(rast, lake_Sr1, method="simple",buffer=20, small=T, fun=mean)
#       lake_cols<-seq(j,ncol(lake_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#       lake_extracted[,lake_cols[i]]<-lake_data
      
      ################################
      ##data from LandTrendr disturbance plots
      ################################
      disturbance_data<-extract(rast, disturbance_Sr1, method="simple",buffer=40, small=T, fun=mean)
      disturbance_cols<-seq(j,ncol(disturbance_extracted_40m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
      disturbance_extracted_40m[,disturbance_cols[i]]<-disturbance_data
      
      print(paste("i=",i,sep=""))
      print(paste("j=",j,sep=""))
    }
}

##For mac
write.table(extracted,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(lake_extracted,file="/Users/hardimanb/Desktop/data.remote/output/data/lake_extracted.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(disturbance_extracted_40m,file="/Users/hardimanb/Desktop/data.remote/output/data/disturbance_extracted_40m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(extracted_7m,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_7m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(extracted_40m,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_40m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)

extracted <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted.csv",sep="\t", header=T)
lake_extracted <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/lake_extracted.csv",sep="\t", header=T)
disturbance_extracted <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/disturbance_extracted.csv",sep="\t", header=T)



sd_10m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
sd_20m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
sd_40m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
sd_60m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
sd_80m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands

colnames(sd_10m_extracted)<-col_names
colnames(sd_20m_extracted)<-col_names
colnames(sd_40m_extracted)<-col_names
colnames(sd_60m_extracted)<-col_names
colnames(sd_80m_extracted)<-col_names

coords<-Sr1@coords
for(i in 1:numfiles){
  for(j in 1:2){  
    for(k in 1:nrow(coords)){

    filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[j]), pattern=".tif" ,recursive=F))
    inpath<-file.path(palsar_inpath,pol_bands[j],filelist[i])
    rast<-raster(inpath)
    
    if(as.numeric(substr(projection(rast),17,18)) == as.numeric(substr(projection(Sr1),17,18))){

      ################################
      ##calibration data from WLEF plots 10m BUFFER STANDARD DEVIATION
      ################################
      radius<-10
      #       ROI_sd(radius,coords,k)
      buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
      ext<-extent(c(buffext[[2]],buffext[[3]])) 
      cellnums<-cellsFromExtent(rast,ext)
      
      cols<-seq(j,ncol(sd_10m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
      #       sd_20m_extracted[k,cols[i]]<- sd_ROI
      sd_10m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))
      ################################
      ##calibration data from WLEF plots 20m BUFFER STANDARD DEVIATION
      ################################
      radius<-20
#       ROI_sd(radius,coords,k)
      buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
      ext<-extent(c(buffext[[2]],buffext[[3]])) 
      cellnums<-cellsFromExtent(rast,ext)
      
      cols<-seq(j,ncol(sd_20m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#       sd_20m_extracted[k,cols[i]]<- sd_ROI
      sd_20m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))
    
      ################################
      ##calibration data from WLEF plots 40m BUFFER STANDARD DEVIATION
      ################################
      radius<-40
      #       ROI_sd(radius,coords,k)
      buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
      ext<-extent(c(buffext[[2]],buffext[[3]])) 
      cellnums<-cellsFromExtent(rast,ext)
      
      cols<-seq(j,ncol(sd_40m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
      #       sd_20m_extracted[k,cols[i]]<- sd_ROI
      sd_40m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))
      
      ################################
      ##calibration data from WLEF plots 60m BUFFER STANDARD DEVIATION
      ################################
      radius<-60
      #       ROI_sd(radius,coords,k)
      buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
      ext<-extent(c(buffext[[2]],buffext[[3]])) 
      cellnums<-cellsFromExtent(rast,ext)
      
      cols<-seq(j,ncol(sd_60m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
      #       sd_20m_extracted[k,cols[i]]<- sd_ROI
      sd_60m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))
      
      ################################
      ##calibration data from WLEF plots 80m BUFFER STANDARD DEVIATION
      ################################
      radius<-80
      #       ROI_sd(radius,coords,k)
      buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
      ext<-extent(c(buffext[[2]],buffext[[3]])) 
      cellnums<-cellsFromExtent(rast,ext)
      
      cols<-seq(j,ncol(sd_80m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
      #       sd_20m_extracted[k,cols[i]]<- sd_ROI
      sd_80m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))
      
      print(paste("i=",i,sep=""))
      print(paste("j=",j,sep=""))
      print(paste("k=",k,sep=""))
                     
    }
  }
}
}


par(mfrow=c(1,1))
for(i in 1:nrow(coords)){
plot(c(10,20,40,60,80),c(sd_10m_extracted[i,1],sd_20m_extracted[i,1],sd_40m_extracted[i,1], sd_60m_extracted[i,1], sd_80m_extracted[i,1]), xlim=c(10,80),ylim=c(0,3), xlab="plot radius (m)",ylab="STDEV of extracted PALSAR returns (HH, gamma (dB))",type="n")
lines(c(10,20,40,60,80),c(sd_10m_extracted[i,1],sd_20m_extracted[i,1],sd_40m_extracted[i,1], sd_60m_extracted[i,1], sd_80m_extracted[i,1]), type="b")
par(new=TRUE)
}

plot(sd_10m_extracted[,1],sd_20m_extracted[,1], xlim=c(0,2), ylim=c(0,2))
plot(sd_20m_extracted[,1],sd_40m_extracted[,1], xlim=c(0,2), ylim=c(0,2))    
plot(sd_10m_extracted[,1],sd_40m_extracted[,1], xlim=c(0,2), ylim=c(0,2))   


extracted <- extracted[ , !apply(is.na(extracted), 2, all)] 
lake_extracted <- lake_extracted[ , !apply(is.na(lake_extracted), 2, all)] 
disturbance_extracted <- disturbance_extracted[ , !apply(is.na(disturbance_extracted), 2, all)] 

head(extracted)
head(lake_extracted)
head(disturbance_extracted)

extracted_7m
extracted
extracted_40m

sd_7m_extracted
sd_20m_extracted
sd_40m_extracted



##For linux
# write.table(extracted,file="/home/bhardima/pecan/modules/data.remote/output/data/WLEF_extracted.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
# write.table(lake_extracted,file="/home/bhardima/pecan/modules/data.remote/output/data/lake_extracted.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
# write.table(disturbance_extracted,file="/home/bhardima/pecan/modules/data.remote/output/data/disturbance_extracted.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
# 
# extracted <- read.table(file="/home/bhardima/pecan/modules/data.remote/output/data/WLEF_extracted.csv",sep="\t", header=T)
# lake_extracted <- read.table(file="/home/bhardima/pecan/modules/data.remote/output/data/lake_extracted.csv",sep="\t", header=T)
# disturbance_extracted <- read.table(file="/home/bhardima/pecan/modules/data.remote/output/data/disturbance_extracted.csv",sep="\t", header=T)

#####################################################################
odds<-seq(1,ncol(extracted),by=2)
evens<-seq(2,ncol(extracted),by=2)
HHscn.dates<-as.Date(substr(col_names[odds],1,8),"%Y%m%d")
HVscn.dates<-as.Date(substr(col_names[evens],1,8),"%Y%m%d")

HH_wlef<-extracted[,odds]
HV_wlef<-extracted[,evens]
#####################################################################

################################
##plot calibration data from WLEF plots
################################
par(mfrow=c(2,1))
boxplot(HV_wlef,ylab="HV_gamma",main='WLEF_plots (n=609)',xaxt="n")
axis(1, at=seq(1, ncol(HV_wlef), by=1), labels = F)
text(seq(1, ncol(HV_wlef), by=1),par("usr")[3]-0.02,labels = HVscn.dates, srt = 45, pos = 1, xpd = TRUE)

boxplot(HH_wlef, ylab="HH_gamma", xaxt="n")
axis(1, at=seq(1, ncol(HH_wlef), by=1), labels = F)
text(seq(1, ncol(HH_wlef), by=1),par("usr")[3]-0.15,labels = HHscn.dates, srt = 45, pos = 1, xpd = TRUE)

################################
##plot data from lakes (n=25) in scenes containing WLEF plots
################################
HH_lakes<-lake_extracted[,odds]
HV_lakes<-lake_extracted[,evens]

par(mfrow=c(2,1))
boxplot(HV_lakes,ylab="HV_gamma",main='lakes (n=25)',xaxt="n")
axis(1, at=seq(1, 12, by=1), labels = F)
text(seq(1, 12, by=1),par("usr")[3]-0.2,labels = HVscn.dates, srt = 45, pos = 1, xpd = TRUE)

boxplot(HH_lakes, ylab="HH_gamma", xaxt="n")
axis(1, at=seq(1, 12, by=1), labels = F)
text(seq(1, 12, by=1),par("usr")[3]-0.2,labels = HHscn.dates, srt = 45, pos = 1, xpd = TRUE)

################################
##plot data from LandTrendr disturbance plots
################################
dist_odds<-seq(1,ncol(disturbance_extracted),by=2)
dist_evens<-seq(2,ncol(disturbance_extracted),by=2)

HH_disturb<-disturbance_extracted[,dist_odds]
HV_disturb<-disturbance_extracted[,dist_evens]

par(mfrow=c(2,1))
boxplot(HV_disturb,ylab="HV_gamma",main='LandTrendr-Disturbance Plots (n=?)',xaxt="n")
axis(1, at=seq(1, ncol(HV_disturb), by=1), labels = F)
text(seq(0, ncol(HV_disturb)-1, by=1),par("usr")[3]-0.02,labels = HVscn.dates, srt = 45, pos = 1, xpd = TRUE)

boxplot(HH_disturb, ylab="HH_gamma", xaxt="n")
axis(1, at=seq(1, ncol(HH_disturb), by=1), labels = F)
text(seq(0, ncol(HH_disturb)-1, by=1),par("usr")[3]-0.075,labels = HHscn.dates, srt = 45, pos = 1, xpd = TRUE)

#####################################################################
##scatterplots of WLEF with noise NOT subtracted
#####################################################################
wlef_abg<-read.csv("/Users/hardimanb/Desktop/data.remote/biometry/biometry_trimmed.csv", sep="\t", header=T)

HVcol_names<-col_names[evens]
HHcol_names<-col_names[odds]

par(mfrow=c(3,length(odds)/3)) ##scatterplots of WLEF with noise NOT subtracted
for(i in 1:ncol(extracted)){
  if(i%%2==0){
    if(extracted[,i])
      plot(wlef_abg$ABG_biomass,extracted[,i], ylim=c(0,0.18), xlab="ABG-biomass", ylab="HV", main=col_names[i])
    par(new=F)
  }
}
par(mfrow=c(3,length(odds)/3))  ##scatterplots of WLEF with noise NOT subtracted
for(i in 1:ncol(extracted)){
  if(i%%2!=0){
    plot(wlef_abg$ABG_biomass,extracted[,i], ylim=c(0,1), xlab="ABG-biomass", ylab="HH", main=col_names[i])
    par(new=F)
  }
}

#####################################################################
##subtract lake values from WLEF returns (attempting to subtract "noise")
#####################################################################
noise <- colMeans(lake_extracted,na.rm=TRUE,1)
signal_extracted<-matrix(NA,nrow(extracted),ncol(extracted))
colnames(signal_extracted)<-colnames(extracted)

for(i in 1:ncol(extracted)){
  signal_extracted[,i]<-extracted[,i]-noise[i]
}

HH_signal<-signal_extracted[,odds]
HV_signal<-signal_extracted[,evens]

par(mfrow=c(2,1))
boxplot(HV_signal,ylab="HV_gamma",main='Corrected WLEF returns (n=609 plots)',xaxt="n")
axis(1, at=seq(1, 12, by=1), labels = F)
text(seq(1, 12, by=1),par("usr")[3]-0.025,labels = HVscn.dates, srt = 45, pos = 1, xpd = TRUE)

boxplot(HH_signal, ylab="HH_gamma", xaxt="n")
axis(1, at=seq(1, 12, by=1), labels = F)
text(seq(1, 12, by=1),par("usr")[3]-0.2,labels = HHscn.dates, srt = 45, pos = 1, xpd = TRUE)

###############################################################################################
##scatterplots of WLEF biomass (with noise subtracted) vs palsar returns
###############################################################################################
HVcol_names<-col_names[evens]
HHcol_names<-col_names[odds]

par(mfrow=c(2,ncol(HV_signal)/2))
for(i in 1:ncol(HV_signal)){
    plot(wlef_abg$ABG_biomass,HV_signal[,i], ylim=c(0,0.2),xlab="ABG-biomass", ylab="HV_signal", main=HVcol_names[i])
    par(new=F)
}
par(mfrow=c(2,ncol(HH_signal)/2)) ##scatterplots of WLEF with noise subtracted
for(i in 1:ncol(HH_signal)){
    plot(wlef_abg$ABG_biomass,HH_signal[,i], ylim=c(0,1),xlab="ABG-biomass", ylab="HH_signal", main=HHcol_names[i])
    par(new=F)
}

par(mfrow=c(1,2))
plot(wlef_abg$ABG_biomass,HH_signal[,1], ylim=c(0,1),xlab="ABG-biomass", ylab="HV_signal", main=HHcol_names[1])
plot(wlef_abg$ABG_biomass,HV_signal[,1], ylim=c(0,0.2),xlab="ABG-biomass", ylab="HH_signal", main=HVcol_names[1])

par(mfrow=c(1,2))
scatter.smooth(wlef_abg$ABG_biomass,HH_signal[,1],col="#CCCCCC",xlab="ABG_biomass",ylab="HH_corrected",main=HHcol_names[1])
scatter.smooth(wlef_abg$ABG_biomass,HV_signal[,1],col="#CCCCCC",xlab="ABG_biomass",ylab="HV_corrected",main=HVcol_names[1])

###############################################################################################
##Funtion to estimate likelihood
###############################################################################################
k<-100
HVmax<-.07
sd<-sd(HV_signal[,1])

params<-c(k,HVmax,sd)

y<-HV_signal[,1]
x<-wlef_abg$ABG_biomass
sel = which(x>0)
x = x[sel];y=y[sel]

ll.monod(params,x,y)

fit1 = optim(par=params,ll.monod,x=x,y=y)
fit1
params = fit1$par
plot(x,y,ylim=c(0,max(y)))
xseq = seq(min(x),max(x),length=1000)
lines(xseq,params[2]*xseq/(xseq+params[1]),col=2,lwd=3)
lines(cbind(biomass,HVvals),col=3,lwd=3)

params2 = c(50,0.7,0.2,1)
fit2 = optim(par=params2,ll.monod2,x=x,y=y)
fit2
params2 = fit2$par
lines(xseq,params2[2]*xseq/(xseq+params2[1])+params2[3],col=4,lwd=3)
lines(lowess(x,y),col=5,lwd=3)

bin.size = 25
xbin = seq(0,450,bin.size)
bin = findInterval(x,xbin)
bin.mu = tapply(y,bin,mean,na.rm=TRUE)
bin.sd = tapply(y,bin,sd,na.rm=TRUE)
points(xbin[sort(as.numeric(names(bin.mu)))]+bin.size/2,bin.mu,col="orange",cex=3,pch=18)
points(xbin[sort(as.numeric(names(bin.mu)))]+bin.size/2,bin.mu+bin.sd,col="orange",cex=3,pch="_")
points(xbin[sort(as.numeric(names(bin.mu)))]+bin.size/2,bin.mu-bin.sd,col="orange",cex=3,pch="_")

biomass<-loess.smooth(wlef_abg$ABG_biomass,HV_signal[,1])$x
HVvals<-loess.smooth(wlef_abg$ABG_biomass,HV_signal[,1])$y
par(mfrow=c(1,1))
plot(cbind(biomass,HVvals))
plot(loess.smooth(wlef_abg$ABG_biomass,HV_signal[,1]))
###############################################################################################
##fitting loess curves to scatterplots of WLEF biomass (with noise subtracted) vs palsar returns
###############################################################################################
par(mfrow=c(2,ncol(HH_signal)/2))
# HH_fitted_vals<-df()
for(i in 1:ncol(HH_signal)){
scatter.smooth(wlef_abg$ABG_biomass,HH_signal[,i],ylim=c(0,1),col="#CCCCCC",xlab="ABG_biomass",ylab="HH_corrected",main=HHcol_names[i])
par(new=F)
}
x<-loess.smooth(wlef_abg$ABG_biomass,HH_signal[,i])$x
y<-loess.smooth(wlef_abg$ABG_biomass,HH_signal[,i])$y

par(mfrow=c(2,ncol(HV_signal)/2))
for(i in 1:ncol(HV_signal)){
scatter.smooth(wlef_abg$ABG_biomass,HV_signal[,i],ylim=c(0,0.2),col="#CCCCCC",xlab="ABG_biomass",ylab="HV_corrected",main=HVcol_names[i])
par(new=F)
}
x<-loess.smooth(wlef_abg$ABG_biomass,HV_signal[,i])$x
y<-loess.smooth(wlef_abg$ABG_biomass,HV_signal[,i])$y

###############################################################################################
## chronosequence of palsar returns from disturbed plots
###############################################################################################

disturbance_signal<-matrix(NA,nrow(disturbance_extracted),ncol(disturbance_extracted))
colnames(disturbance_signal)<-colnames(disturbance_extracted)

HH_noise<-mean(noise[seq(1,length(noise),by=2)])
HV_noise<-mean(noise[seq(2,length(noise),by=2)])
noise_constant<-c(HH_noise,HV_noise)

for(i in 1:ncol(disturbance_extracted)){
  if(i%%2==0){
    disturbance_signal[,i]<-disturbance_extracted[,i]-noise_constant[1]
  }else{
    disturbance_signal[,i]<-disturbance_extracted[,i]-noise_constant[2]
  }
}

HH_disturb<-disturbance_signal[,dist_odds]
HV_disturb<-disturbance_signal[,dist_evens]

scn.dates<-as.Date(substr(colnames(disturbance_extracted),2,9),"%Y%m%d")
scn.yr<-substr(colnames(disturbance_extracted),2,5)
scn.yr<-as.numeric(scn.yr[dist_odds])

colnames(HH_disturb)<-as.character(scn.dates[dist_odds])
colnames(HV_disturb)<-as.character(scn.dates[dist_evens])

disturbance_ages<-matrix(NA,nrow(HH_disturb),length(scn.yr))
colnames(disturbance_ages)<-as.character(scn.dates[dist_evens])
for(i in 1:length(scn.yr)){
disturbance_ages[,i]<-scn.yr[i]-disturbance_infile$distyr
}

# chrono<-matrix(NA,nrow(HH_disturb),length(scn.yr))

##I am trying to plot palsar return by age of plot. the indexing for x and y below is correct, but gives output as a vector>>how can I retain the matrix structure for boxplotting???

# age<-matrix(NA,length(disturbance_ages[!is.na(HH_disturb[,i]),i]),ncol(HH_disturb))
# HH_disturb_noNAs<-matrix(NA,length(disturbance_ages[!is.na(HH_disturb[,i]),i]),ncol(HH_disturb))
# for(i in 1:length(scn.dates[odds])){
# }

# age<-seq(min(disturbance_ages),max(disturbance_ages),by=1)
disturbance_ages[is.na(HH_disturb)]=NA

par(mfrow=c(1,1))
for(i in 1:ncol(HH_disturb)){
    plot(disturbance_ages[,i]>0, HH_disturb[,i], pch=i)
    par(new=T)
}

ltzero<-HH_disturb[disturbance_ages[,1]<=0,1]
ltfive<-HH_disturb[disturbance_ages[,1]>0 & disturbance_ages[,1]<=5,1]
ltten<-HH_disturb[disturbance_ages[,1]>5 & disturbance_ages[,1]<=10,1]
ltfifteen<-HH_disturb[disturbance_ages[,1]>10 & disturbance_ages[,1]<=15,1]
lttwenty<-HH_disturb[disturbance_ages[,1]>15 & disturbance_ages[,1]<=20,1]
lttwentyfive<-HH_disturb[disturbance_ages[,1]>20 & disturbance_ages[,1]<=25,1]

n <- max(length(ltzero), length(ltfive), length(ltten) , length(ltfifteen) , length(lttwenty), length(lttwentyfive))
length(ltzero) <- n                      
length(ltfive) <- n
length(ltten) <- n
length(ltfifteen) <- n
length(lttwenty) <- n
length(lttwentyfive) <- n

binned<-cbind(ltfive,ltten,ltfifteen,lttwenty,lttwentyfive)
par(mfrow=c(1,1))
boxplot(binned)
par(mfrow=c(1,1))
plot(c(0,5,10,15,20,25),binned)


neg<-mean(HH_disturb[disturbance_ages<=0],na.rm=T)
five<-mean(HH_disturb[disturbance_ages>0 & disturbance_ages<=5],na.rm=T)
ten<-mean(HH_disturb[disturbance_ages>5 & disturbance_ages<=10],na.rm=T)
fif<-mean(HH_disturb[disturbance_ages>10 & disturbance_ages<=15],na.rm=T)
twen<-mean(HH_disturb[disturbance_ages>15 & disturbance_ages<=20],na.rm=T)
twenfi<-mean(HH_disturb[disturbance_ages>20 & disturbance_ages<=25],na.rm=T)

binned<-cbind(c(0,5,10,15,20,25),c(neg,five,ten,fif,twen,twenfi))
par(mfrow=c(1,1))
plot(binned[,1],binned[,2], ylim=c(0,.2))


scatter.smooth(disturbance_ages[disturbance_ages>0 & disturbance_ages<=5],HH_disturb[disturbance_ages>0 & disturbance_ages<=5],col="#CCCCCC",xlab="Time since disturbance (years)",ylab="HH_gamma (noise-corrected, power ratio)")
scatter.smooth(disturbance_ages[disturbance_ages>5 & disturbance_ages<=10],HH_disturb[disturbance_ages>5 & disturbance_ages<=10],col="#CCCCCC",xlab="Time since disturbance (years)",ylab="HH_gamma (noise-corrected, power ratio)")
scatter.smooth(disturbance_ages[disturbance_ages>10 & disturbance_ages<=15],HH_disturb[disturbance_ages>10 & disturbance_ages<=15],col="#CCCCCC",xlab="Time since disturbance (years)",ylab="HH_gamma (noise-corrected, power ratio)")

plot(disturbance_infile$distyr,HH_disturb[,1])



## HERE THER BE DRAGONS (JUNK/INCOMPLETE/BAD/OBSELETE CODE) ##



boxplot(HH_disturb, xlab="Time since disturbance (years)",ylab="HH_gamma", xaxt="n")
axis(1, at=seq(1, length(age)-1, by=1), labels = F)
text(seq(min(age), max(age), by=1),par("usr")[3]-0.01,labels = age, srt = 0, pos = 1, xpd = TRUE)



boxplot(HH_disturb) ~ disturbance_ages),disturbance_ages,labels=NULL)
Boxplot(HV_disturb,disturbance_ages,labels=NULL)

#   if(i==1){
#     chrono<-matrix(NA,length(y),length(scn.yr))
#   }
#   chrono[,i]<-y
# }


  if(i==1){
    plot(x,y, xlab="Disturbance Age", ylab="HH",main="PALSAR returns from disturbed plots", pch=i)
}
  else{plot(x,y,axes=F, xlab="",ylab="",pch=i)
  }
  par(new=T)
}

par(mfrow=c(1,2))
par(mfg=c(2,1))
for(i in 1:ncol(HH_disturb)){
  if(i==1){plot(disturbance_ages[!is.na(HH_disturb[,i]),i],HH_disturb[!is.na(HH_disturb[,i]),i], xlab="Time since disturbance", ylab="HH",main="PALSAR returns from disturbed plots", pch=i)
  }
  else{plot(disturbance_ages[!is.na(HH_disturb[,i]),i],HH_disturb[!is.na(HH_disturb[,i]),i],axes=F, xlab="",ylab="",pch=i)
  }
  
  par(new=T)
}
# par(new=F)
par(mfg=c(1,2))
for(i in 1:ncol(HV_disturb)){
  if(i==1){plot(disturbance_ages[!is.na(HV_disturb[,i]),i],HV_disturb[!is.na(HV_disturb[,i]),i], xlab="Time since disturbance", ylab="HV",main="PALSAR returns from disturbed plots", pch=i)
  }
  else{plot(disturbance_ages[!is.na(HV_disturb[,i]),i],HV_disturb[!is.na(HV_disturb[,i]),i],axes=F, xlab="",ylab="",pch=i)
  }
  par(new=T)
}
mean(HV_disturb[disturbance_ages[,1]<0,1])


plot(disturbance_ages[!is.na(HV_disturb[,1])],HV_disturb[!is.na(HV_disturb[,1]),1], xlab="Disturbance Date", ylab="HV",main=paste("scn date",scn.dates[1],sep=" "))


################  So, do I fit curves to the noise-corrected returns, or to the raw returns?


lm_data<-cbind(wlef_abg$ABG_biomass,extracted)
summary(lm(wlef_abg$ABG_biomass ~ extracted[,1]+extracted[,2]))




for(i in 1:nrow(extracted)){
plot(HH[i,],NULL, axes=F,ylim=c(0,max(HH)), main="HH",type="n")
lines(HH[i,],NULL,'b')
par(new=T)
}

par(mfrow=c(1,1), new=F)
plot()

for(i in 1:nrow(extracted)){
  plot(HV[i,],NULL, axes=F,ylim=c(0,max(HV)), main="HV", type="n")
  lines(HV[i,],NULL,'b')
  par(new=T)
}

par(mfrow=c(2,1))
plot(extracted[,1],extracted[,3], xlim=c(0,.4), ylim=c(0,.6),xlab="2007 HH gamma", ylab="2010 HH gamma",main="2007 vs 2010 WLEF plots")
abline(0,1,col="red")
plot(extracted[,2],extracted[,4], xlab="2007 HV gamma", ylab="2010 HV gamma")
abline(0,1,col="red")

# for(i in 1:nrow(extracted)){
#   plot(extracted[i,1],extracted[i,3], type="b",add=T)
# }

 

# HH_filelist <- as.vector(list.files(file.path(palsar_inpath, "HH"), pattern=".tif" ,recursive=F))
# HH_inpath <-file.path(palsar_inpath,"HH", HH_filelist[1])
# 
# HV_filelist <- as.vector(list.files(file.path(palsar_inpath, "HV"), pattern=".tif" ,recursive=F))
# HV_inpath <-file.path(palsar_inpath,"HV", HV_filelist[1])

# HH<-raster(HH_inpath)
# HV<-raster(HV_inpath)
# 
# image(HV,col=gray(1:255/255))
# plot(Sr1,col="yellow", add=TRUE)

# box<-bbox(Sr1)

# image(crop(HH,box),col=gray(1:255/255))
# plot(Sr1,col="yellow", add=TRUE)

# extracted<-cbind(extract(HH, Sr1, method="simple",buffer=20, small=T, fun=mean), extract(HV, Sr1, method="simple",buffer=20, small=T, fun=mean))

par(mfrow=c(2,1))
plot(infile$ANPPW,extracted[,1], xlab="ABG-biomass", ylab="HH")
plot(infile$ANPPW,extracted[,2], xlab="ABG-biomass", ylab="HV")