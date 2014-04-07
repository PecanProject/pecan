##this script will access a palsar file on my local machine to extract radar returns from a polygon 
##associated with my UNDERC plot

##Author: Brady S. Hardiman 04/30/2013

##load required pkgs and libraries
require(rgdal)
require(proj4)
require(raster)
library(sp)
library(rgdal)
library(proj4)
library(spatstat)
library(maptools)

scn_metadata <- read.table(file="/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv", header=T, sep="\t")


##if running this as a function dir can/should be an argument
setwd("/home/bhardima/pecan/modules/data.remote/palsar_scenes/UNDERC/")
filelist <- as.vector(list.dirs(path=getwd() ,recursive=F))

##read in polygons correspoding to plots with biomass ground-data

#read in table here
#convert coordinates to UTM (to match PALSAR files)
# x <- c(-89.55009302960, -89.55042213000, -89.54882422370,  -89.54913190390)
# y <- c(46.25113458880, 46.25338996770, 46.25120674830, 46.25345157750)
# xy<- cbind(x,y)
# 
# ptransform(xy,'+proj=longlat +ellps=sphere','+proj=merc +ellps=sphere') 
head<-c('scnID', 'scnDate', 'cellsw', 'cellnw', 'cellne', 'cellse', 'HHsw', 'HHnw', 'HHne', 'HHse', 'HVsw', 'HVnw', 'HVne', 'HVse')
output<-matrix(NA,length(filelist)+1,length(head))
output[1,]<-head
output[2:(length(filelist)+1),1]<-as.character(scn_metadata$scnid)
output[2:(length(filelist)+1),2]<-as.character(scn_metadata$scndate)



for (i in 1:length(filelist)){ 
    
  inpath <-filelist[i]
  
  ##read in PALSAR bands (HH and HV polarizations)
  scnHH <- Sys.glob(file.path(inpath,"IMG-HH-*-H1.5_UA"))
  scnHV <- Sys.glob(file.path(inpath,"IMG-HV-*-H1.5_UA"))
  rasterHH <- raster(scnHH)
  rasterHV <- raster(scnHV)
  
  ##UNDERC plot corner coordinates
  #decdeg_coords <-rbind(c(46.25113458880, -89.55009302960), c(46.25338996770, -89.55042213000),c(46.25345157750, -89.54913190390), c(46.25120674830, -89.54882422370), c(46.25113458880, -89.55009302960)) ##Dec degrees
  ##Pomeroy Lake zero test
  decdeg_coords <-rbind(
    c(46.275724, -89.581100),
    c(46.278254, -89.581100),
    c(46.278254, -89.573647),
    c(46.275724, -89.573647))
  
  
  
  swN <-decdeg_coords[1,1] #southwest corner easting=swE, southwest corner northing=swN
  nwN <-decdeg_coords[2,1]
  neN <-decdeg_coords[3,1]
  seN <-decdeg_coords[4,1]
  
  swE <-decdeg_coords[1,2]
  nwE <-decdeg_coords[2,2]
  neE <-decdeg_coords[3,2]
  seE <-decdeg_coords[4,2]
  
  a0 <- scn_metadata$scn_coord2pix_a0[scn_metadata$scnid==scn_metadata$scnid[i]]
  a1 <- scn_metadata$scn_coord2pix_a1[scn_metadata$scnid==scn_metadata$scnid[i]]
  a2 <- scn_metadata$scn_coord2pix_a2[scn_metadata$scnid==scn_metadata$scnid[i]]
  a3 <- scn_metadata$scn_coord2pix_a3[scn_metadata$scnid==scn_metadata$scnid[i]]
  a4 <- scn_metadata$scn_coord2pix_a4[scn_metadata$scnid==scn_metadata$scnid[i]]
  a5 <- scn_metadata$scn_coord2pix_a5[scn_metadata$scnid==scn_metadata$scnid[i]]
  a6 <- scn_metadata$scn_coord2pix_a6[scn_metadata$scnid==scn_metadata$scnid[i]]
  a7 <- scn_metadata$scn_coord2pix_a7[scn_metadata$scnid==scn_metadata$scnid[i]]
  a8 <- scn_metadata$scn_coord2pix_a8[scn_metadata$scnid==scn_metadata$scnid[i]]
  a9 <- scn_metadata$scn_coord2pix_a9[scn_metadata$scnid==scn_metadata$scnid[i]]
  b0 <- scn_metadata$scn_coord2pix_b0[scn_metadata$scnid==scn_metadata$scnid[i]]
  b1 <- scn_metadata$scn_coord2pix_b1[scn_metadata$scnid==scn_metadata$scnid[i]]
  b2 <- scn_metadata$scn_coord2pix_b2[scn_metadata$scnid==scn_metadata$scnid[i]]
  b3 <- scn_metadata$scn_coord2pix_b3[scn_metadata$scnid==scn_metadata$scnid[i]]
  b4 <- scn_metadata$scn_coord2pix_b4[scn_metadata$scnid==scn_metadata$scnid[i]]
  b5 <- scn_metadata$scn_coord2pix_b5[scn_metadata$scnid==scn_metadata$scnid[i]]
  b6 <- scn_metadata$scn_coord2pix_b6[scn_metadata$scnid==scn_metadata$scnid[i]]
  b7 <- scn_metadata$scn_coord2pix_b7[scn_metadata$scnid==scn_metadata$scnid[i]]
  b8 <- scn_metadata$scn_coord2pix_b8[scn_metadata$scnid==scn_metadata$scnid[i]]
  b9 <- scn_metadata$scn_coord2pix_b9[scn_metadata$scnid==scn_metadata$scnid[i]]
  
  ##l=line p=pixel
  sw_p <- a0 + a1*swN + a2*swE + a3*swN*swE + a4*swN^2 + a5*swE^2 + a6*swN^2*swE + a7*swN*swE^2 + a8*swN^3 + a9*swE^3
  nw_p <- a0 + a1*nwN + a2*nwE + a3*nwN*nwE + a4*nwN^2 + a5*nwE^2 + a6*nwN^2*nwE + a7*nwN*nwE^2 + a8*nwN^3 + a9*nwE^3
  ne_p <- a0 + a1*neN + a2*neE + a3*neN*neE + a4*neN^2 + a5*neE^2 + a6*neN^2*neE + a7*neN*neE^2 + a8*neN^3 + a9*neE^3
  se_p <- a0 + a1*seN + a2*seE + a3*seN*seE + a4*seN^2 + a5*seE^2 + a6*seN^2*seE + a7*seN*seE^2 + a8*seN^3 + a9*seE^3
  
  sw_l <- b0 + b1*swN + b2*swE + b3*swN*swE + b4*swN^2 + b5*swE^2 + b6*swN^2*swE + b7*swN*swE^2 + b8*swN^3 + b9*swE^3
  nw_l <- b0 + b1*nwN + b2*nwE + b3*nwN*nwE + b4*nwN^2 + b5*nwE^2 + b6*nwN^2*nwE + b7*nwN*nwE^2 + b8*nwN^3 + b9*nwE^3
  ne_l <- b0 + b1*neN + b2*neE + b3*neN*neE + b4*neN^2 + b5*neE^2 + b6*neN^2*neE + b7*neN*neE^2 + b8*neN^3 + b9*neE^3
  se_l <- b0 + b1*seN + b2*seE + b3*seN*seE + b4*seN^2 + b5*seE^2 + b6*seN^2*seE + b7*seN*seE^2 + b8*seN^3 + b9*seE^3
  
  lpcoords <-rbind(c(sw_l, sw_p), c(nw_l, nw_p),c(ne_l, ne_p), c(se_l, se_p)) ##l=line p=pixel
  
  HHcells<-extract(rasterHH, lpcoords, method='simple',cellnumbers=T)
  HVcells<-extract(rasterHV, lpcoords, method='simple',cellnumbers=T)
  
  cells <- HHcells[,1]
  
  output[i+1,3]<-cells[1]
  output[i+1,4]<-cells[2]
  output[i+1,5]<-cells[3]
  output[i+1,6]<-cells[4]
  
  LUT<-cbind(unique(rowFromCell(rasterHH,cells[1:length(cells)])),unique(colFromCell(rasterHH,cells[1:length(cells)])))
  
  HH<-vector(mode='numeric',length(rows))
  HV<-vector(mode='numeric',length(rows))
  
  for(j in 1:nrow(LUT)){
      HH[j]<-rasterHH[LUT[j,1],LUT[j,2]]
      HV[j]<-rasterHV[LUT[j,1],LUT[j,2]]
  }

  output[i+1,7] <- HH[1]
  output[i+1,8] <- HH[2]
  output[i+1,9] <- HH[3]
  output[i+1,10]<- HH[4]
  
  output[i+1,11] <-HV[1]
  output[i+1,12] <-HV[2]
  output[i+1,13] <-HV[3]
  output[i+1,14] <-HV[4]
  
}


dates<-as.date(as.character(substr(output[2:nrow(output),2],1,8)),order='ymd')
max_return<-vector()
##To plot an 8 panel time series (line plot) of each of 4 coordinate's HH and HV returns
par(mfcol=c(4,2))
for(l in 1:8){
  max_return[l]<-max(as.numeric(as.character(output[2:nrow(output),l+6])),na.rm=T)
  plot(dates,as.numeric(as.character(output[2:nrow(output),l+6])), type="n",xlab='Date', ylab=output[1,l+6], ylim=c(0,max(max_return)))
  lines(dates,as.numeric(as.character(output[2:nrow(output),l+6])), type="b")
}
par(mfcol=c(1,1))
plot(dates,as.numeric(as.character(output[2:nrow(output),7])), col='red', ylim=c(0,max(max_return)),pch=19,ps=20,type="n")
title(main='HH')
points(dates,as.numeric(as.character(output[2:nrow(output),7])), col='red', pch=19,ps=20)
points(dates,as.numeric(as.character(output[2:nrow(output),8])), col='green', pch=19,ps=20)
points(dates,as.numeric(as.character(output[2:nrow(output),9])), col='blue', pch=19,ps=20)
points(dates,as.numeric(as.character(output[2:nrow(output),10])), col='black', pch=19,ps=20)

par(mfcol=c(1,1))
plot(dates,as.numeric(as.character(output[2:nrow(output),7])), col='red', ylim=c(0,max(max_return)),pch=19,ps=20,type="n")
title(main='HV')
points(dates,as.numeric(as.character(output[2:nrow(output),11])), col='red', pch=19,ps=20)
points(dates,as.numeric(as.character(output[2:nrow(output),12])), col='green', pch=19,ps=20)
points(dates,as.numeric(as.character(output[2:nrow(output),13])), col='blue', pch=19,ps=20)
points(dates,as.numeric(as.character(output[2:nrow(output),14])), col='black', pch=19,ps=20)

# 
# 
# write.table(output,file="/home/bhardima/pecan/modules/data.remote/output/data/output_point-value-timeseries.csv",quote=F,sep="\t",eol="\r\n", row.names=F,col.names=F)
# path="/home/bhardima/pecan/modules/data.remote/output/data/output_point-value-timeseries.csv"
# output<-read.delim(path, header=F, sep="\t", fill=T)
# 
# values<as.matrix(output[2:nrow(output),7:ncol(output)],na.rm=TRUE)





     