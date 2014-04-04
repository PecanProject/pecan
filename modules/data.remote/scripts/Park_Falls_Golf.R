require(rgdal)
require(raster)
require(sp)
require(RgoogleMaps)
require(maptools)
require(ggplot2)
require(car)
require(spatstat)

# coords<-data.frame(rbind(c(698996.57, 5091229.03),
#                 c(699219.52, 5091258.81),
#                 c(699331.11, 5091263.73),
#                 c(699264.17, 5091564.24),
#                 c(699152.12, 5091552.76),
#                 c(699337.37, 5091929.27),
#                 c(699385.47, 5091930.04)))
# Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))


calib_inpath <-"/Users/hardimanb/Desktop/data.remote/biometry" ##For Mac
# calib_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry"

calib_infile <-read.csv(file.path(calib_inpath,"biometry_trimmed.csv"), sep=",", header=T) #WLEF plots

coords<-data.frame(calib_infile$easting[1:100],calib_infile$northing[1:100]) #eastings and northings (UTM Zone 15N NAD83)

##Convert to class=SpatialPoints for palsar extraction (this is used for creating kml files and will be used later on)
Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +a=6378137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))



palsar_inpath <- file.path("/Users/hardimanb/Desktop/data.remote/palsar_scenes/geo_corrected_single_gamma") ##For Mac
# palsar_inpath <- file.path("/home/bhardima/pecan/modules/data.remote/palsar_scenes/Link_to_cheas/geo_corrected_single_gamma")

file.info<-read.table(file="/Users/hardimanb/Desktop/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t") ##For Mac
# file.info<-read.table(file="/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t")

date.time<-as.vector(substr(file.info$scndate,1,8))
date.time<-as.Date(date.time,"%Y%m%d")
col_names<-c(rbind(paste(date.time, "HH",sep="_"),paste(date.time, "HV",sep="_")))

pol_bands<-c("HH", "HV")
numfiles<-length(date.time)

extracted_10m<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted_20m<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted_40m<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted_60m<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted_80m<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands

colnames(extracted_10m)<-col_names
colnames(extracted_20m)<-col_names
colnames(extracted_40m)<-col_names
colnames(extracted_60m)<-col_names
colnames(extracted_80m)<-col_names

for(i in 1:numfiles){
  for(j in 1:2){        
    
    filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[j]), pattern=".tif" ,recursive=F))
    inpath<-file.path(palsar_inpath,pol_bands[j],filelist[i])
    rast<-raster(inpath)
    
#     ################################
#     ##golf plots 10m BUFFER MEAN
#     ################################
#     data_10m<-extract(rast, Sr1, method="simple",buffer=10, small=T, fun=mean)
#     cols<-seq(j,ncol(extracted_10m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#     extracted_10m[,cols[i]]<-data_10m
#     
#     ###############################
#     #golf plots 20m BUFFER MEAN
#     ###############################
#     data_20m<-extract(rast, Sr1, method="simple",buffer=20, small=T, fun=mean)
#     cols<-seq(j,ncol(extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#     extracted_20m[,cols[i]]<-data_20m
    
    ################################
    ##golf plots 40m BUFFER MEAN
    ################################
    data_40m<-extract(rast, Sr1, method="simple",buffer=40, small=T, fun=mean)
    cols<-seq(j,ncol(extracted_40m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
    extracted_40m[,cols[i]]<-data_40m
    
#     ################################
#     ##golf plots 60m BUFFER MEAN
#     ################################
#     data_60m<-extract(rast, Sr1, method="simple",buffer=60, small=T, fun=mean)
#     cols<-seq(j,ncol(extracted_60m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#     extracted_60m[,cols[i]]<-data_60m
#     
#     ################################
#     ##golf plots 80m BUFFER MEAN
#     ################################
#     data_80m<-extract(rast, Sr1, method="simple",buffer=80, small=T, fun=mean)
#     cols<-seq(j,ncol(extracted_80m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#     extracted_80m[,cols[i]]<-data_80m
    
    
    print(paste("i=",i,sep=""))
    print(paste("j=",j,sep=""))
  }
}

write.table(extracted_10m,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_10m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(extracted_20m,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_20m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(extracted_40m,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_40m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(extracted_60m,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_60m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)
write.table(extracted_80m,file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_80m.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=T)

extracted_10m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_10m.csv",sep="\t", header=T)
extracted_20m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_20m.csv",sep="\t", header=T)
extracted_40m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_40m.csv",sep="\t", header=T)
extracted_60m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_60m.csv",sep="\t", header=T)
extracted_80m <- read.table(file="/Users/hardimanb/Desktop/data.remote/output/data/WLEF_extracted_80m.csv",sep="\t", header=T)

# par(new=F)
# par(mfrow=c(1,1))
# for(i in 1:nrow(coords)){
#   plot(c(10,20,40,60,80),c(extracted_10m[i,1],extracted_20m[i,1],extracted_40m[i,1], extracted_60m[i,1], extracted_80m[i,1]), xlim=c(10,80),ylim=c(-16,0), xlab="plot radius (m)",ylab="MEAN of extracted PALSAR returns (HH, gamma (dB))",type="n")
#   lines(c(10,20,40,60,80),c(extracted_10m[i,1],extracted_20m[i,1],extracted_40m[i,1], extracted_60m[i,1], extracted_80m[i,1]), type="b")
#   par(new=TRUE)
# }
# 
par(new=F)
par(mfrow=c(1,1))
for(i in 1:nrow(coords)){
  plot(c(10,20,40,60,80),c(extracted_10m[i,2],extracted_20m[i,2],extracted_40m[i,2], extracted_60m[i,2], extracted_80m[i,2]), xlim=c(10,80),ylim=c(-18,-6), xlab="plot radius (m)",ylab='MEAN of extracted PALSAR returns \n HV, gamma (dB))',type="n")
  lines(c(10,20,40,60,80),c(extracted_10m[i,2],extracted_20m[i,2],extracted_40m[i,2], extracted_60m[i,2], extracted_80m[i,2]), type="b")
  par(new=TRUE)
}


# evens<-seq(2,ncol(extracted_10m),by=2)
# odds<-seq(1,ncol(extracted_10m),by=2)
# 
# ##plot time series 
# colors<-c("red","blue","green","orange","yellow","black","magenta")
# 
# par(new=F)
# par(mfrow=c(1,1))
# for(j in 1:nrow(coords)){
#     plot(date.time,extracted_20m[j,evens],ylim=c(-26,-5),xlab="scn date",ylab="MEAN of extracted PALSAR returns (HV, gamma (dB))",main="Putting Greens",type="n")
#     lines(date.time,extracted_20m[j,evens],ylim=c(-26,-5),col=colors[j],type="b")
#     par(new=TRUE)
# }
# 
# colnames(extracted_10m[,evens])<-date.time
# colnames(extracted_10m[,odds])<-date.time
# boxplot(extracted_20m[,evens],xlab="scn date",ylab="MEAN of extracted PALSAR returns (HV, gamma (dB))",main="WLEF: mean HV (gamma (dB), 10m buffer)",type="n")



se_10m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
se_20m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
se_40m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
se_60m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
se_80m_extracted<-matrix(NA, nrow(coords),length(pol_bands)*numfiles) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands

colnames(se_10m_extracted)<-col_names
colnames(se_20m_extracted)<-col_names
colnames(se_40m_extracted)<-col_names
colnames(se_60m_extracted)<-col_names
colnames(se_80m_extracted)<-col_names

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
        
        cols<-seq(j,ncol(se_10m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
        #       sd_20m_extracted[k,cols[i]]<- sd_ROI
        se_10m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))/sqrt(length(cellnums))
        ################################
        ##calibration data from WLEF plots 20m BUFFER STANDARD DEVIATION
        ################################
        radius<-20
        #       ROI_sd(radius,coords,k)
        buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
        ext<-extent(c(buffext[[2]],buffext[[3]])) 
        cellnums<-cellsFromExtent(rast,ext)
        
        cols<-seq(j,ncol(se_20m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
        #       sd_20m_extracted[k,cols[i]]<- sd_ROI
        se_20m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))/sqrt(length(cellnums))
        
        ################################
        ##calibration data from WLEF plots 40m BUFFER STANDARD DEVIATION
        ################################
        radius<-40
        #       ROI_sd(radius,coords,k)
        buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
        ext<-extent(c(buffext[[2]],buffext[[3]])) 
        cellnums<-cellsFromExtent(rast,ext)
        
        cols<-seq(j,ncol(se_40m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
        #       sd_20m_extracted[k,cols[i]]<- sd_ROI
        se_40m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))/sqrt(length(cellnums))
        
        ################################
        ##calibration data from WLEF plots 60m BUFFER STANDARD DEVIATION
        ################################
        radius<-60
        #       ROI_sd(radius,coords,k)
        buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
        ext<-extent(c(buffext[[2]],buffext[[3]])) 
        cellnums<-cellsFromExtent(rast,ext)
        
        cols<-seq(j,ncol(se_60m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
        #       sd_20m_extracted[k,cols[i]]<- sd_ROI
        se_60m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))/sqrt(length(cellnums))
        
        ################################
        ##calibration data from WLEF plots 80m BUFFER STANDARD DEVIATION
        ################################
        radius<-80
        #       ROI_sd(radius,coords,k)
        buffext<-as.vector(disc(radius=radius, centre=coords[k,])) ##10m is the smallest buffer size that overlaps >1 cell
        ext<-extent(c(buffext[[2]],buffext[[3]])) 
        cellnums<-cellsFromExtent(rast,ext)
        
        cols<-seq(j,ncol(se_80m_extracted),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
        #       sd_20m_extracted[k,cols[i]]<- sd_ROI
        se_80m_extracted[k,cols[i]]<- sd(extract(rast,cellnums))/sqrt(length(cellnums))
        
        print(paste("i=",i,sep=""))
        print(paste("j=",j,sep=""))
        print(paste("k=",k,sep=""))
        
      }
    }
  }
}

colors<-c("red","blue","green","orange","cyan","black","magenta")
par(new=F)
par(mfrow=c(1,1))
for(j in 1:nrow(coords)){
  plot(date.time,se_20m_extracted[j,evens],xlab="scn date",ylim=c(0,5),ylab="STDERR of extracted PALSAR returns (HV, gamma (dB))",main="WLEF Plots (n=609)",type="n")
  lines(date.time,se_20m_extracted[j,evens],ylim=c(0,5),type="b")
  par(new=TRUE)
}


par(mfrow=c(1,2))

for(i in 1:nrow(coords)){
  plot(c(10,20,40,60,80),c(se_10m_extracted[i,1],se_20m_extracted[i,1],se_40m_extracted[i,1], se_60m_extracted[i,1], se_80m_extracted[i,1]), xlim=c(10,80),ylim=c(0,0.07), ylab="",xlab="",type="n", axes=FALSE)
  lines(c(10,20,40,60,80),c(se_10m_extracted[i,1],se_20m_extracted[i,1],se_40m_extracted[i,1], se_60m_extracted[i,1], se_80m_extracted[i,1]), type="b")
  par(new=TRUE)
}
par(new=T)
plot(c(10,20,40,60,80),c(se_10m_extracted[i,1],se_20m_extracted[i,1],se_40m_extracted[i,1], se_60m_extracted[i,1], se_80m_extracted[i,1]), xlim=c(10,80),ylim=c(0,0.07), xlab="plot radius (m)",ylab="S.E. of extracted HH returns (gamma)",type="n")

for(i in 1:nrow(coords)){
  plot(c(10,20,40,60,80),c(se_10m_extracted[i,2],se_20m_extracted[i,2],se_40m_extracted[i,2], se_60m_extracted[i,2], se_80m_extracted[i,2]), xlim=c(10,80),ylim=c(0,0.025), ylab="",xlab="",type="n", axes=FALSE)
  lines(c(10,20,40,60,80),c(se_10m_extracted[i,2],se_20m_extracted[i,2],se_40m_extracted[i,2], se_60m_extracted[i,2], se_80m_extracted[i,2]), type="b")
  par(new=TRUE)
}
par(new=T)
plot(c(10,20,40,60,80),c(se_10m_extracted[i,1],se_20m_extracted[i,1],se_40m_extracted[i,1], se_60m_extracted[i,1], se_80m_extracted[i,1]), xlab="plot radius (m)",ylab="S.E. of extracted HV returns (gamma)",type="n")
