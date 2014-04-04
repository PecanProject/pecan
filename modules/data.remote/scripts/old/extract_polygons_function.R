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
# x <- c(-89.55009302960, -89.55042213000, -89.54882422370,	-89.54913190390)
# y <- c(46.25113458880, 46.25338996770, 46.25120674830, 46.25345157750)
# xy<- cbind(x,y)
# 
# ptransform(xy,'+proj=longlat +ellps=sphere','+proj=merc +ellps=sphere') 
for (i in 1:length(filelist)){ 
  
  inpath <-filelist[i]
  
  ##read in PALSAR bands (HH and HV polarizations)
  scnHH <- Sys.glob(file.path(inpath,"*_HH.tif"))
  scnHV <- Sys.glob(file.path(inpath,"*_HV.tif"))
  rasterHH <- raster(scnHH)
  rasterHV <- raster(scnHV)
  par(mfrow=(c(1,1)))
  image(rasterHV)
  # image(rasterHH)
  
  # rasterHH[96,1111]
  # rasterHV[96,1111]
  
  ##overlay polygon
    #overlay(scnHH scnHVpolygon filename)
    #see also: Extract values from Raster objects
  # coords <-rbind(c(303438,5125112), c(303421,5125363),c(303521,5125367), c(303536,5125117), c(303438,5125112)) ##UTM
  
  ##UNDERC plot corner coordinates (clockwise from lower left [ie, se]). First coordinate is repeated to close the Polygon.
  decdeg_coords <-rbind(c(46.25113458880, -89.55009302960), c(46.25338996770, -89.55042213000),c(46.25345157750, -89.54913190390), c(46.25120674830, -89.54882422370), c(46.25113458880, -89.55009302960)) ##Dec degrees
  
  ##Twin Island Lake bounding box coordinates (clockwise from lower left [ie, se]). First coordinate is repeated to close the Polygon.
#   decdeg_coords <-rbind(
#   c(46.217102,-89.601745),
#   c(46.231731,-89.601745),
#   c(46.231731,-89.581994),
#   c(46.217102,-89.581994),
#   c(46.217102,-89.601745)) 
      
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
  
  # B11 <- scn_metadata$scn_pix2coord_b11[scn_metadata$scnid==scn_metadata$scnid[i]]
  # B12 <- scn_metadata$scn_pix2coord_b12[scn_metadata$scnid==scn_metadata$scnid[i]]
  # B13 <- scn_metadata$scn_pix2coord_b13[scn_metadata$scnid==scn_metadata$scnid[i]]
  # B14 <- scn_metadata$scn_pix2coord_b14[scn_metadata$scnid==scn_metadata$scnid[i]]
  # B21 <- scn_metadata$scn_pix2coord_b21[scn_metadata$scnid==scn_metadata$scnid[i]]
  # B22 <- scn_metadata$scn_pix2coord_b22[scn_metadata$scnid==scn_metadata$scnid[i]]
  # B23 <- scn_metadata$scn_pix2coord_b23[scn_metadata$scnid==scn_metadata$scnid[i]]
  # B24 <- scn_metadata$scn_pix2coord_b24[scn_metadata$scnid==scn_metadata$scnid[i]]
  
  # scn_metadata$scnUTMzone[scn_metadata$scnid==scn_metadata$scnid[i]]
  
  # sw_l= B11 + B12*swE + B13*swN + B14*swE*swN
  # nw_l= B11 + B12*nwE + B13*nwN + B14*nwE*nwN
  # ne_l= B11 + B12*neE + B13*neN + B14*neE*neN
  # se_l= B11 + B12*seE + B13*seN + B14*seE*seN 
  # 
  # sw_p= B21 + B22*swE + B23*swN + B24*swE*swN
  # nw_p= B21 + B22*nwE + B23*nwN + B24*nwE*nwN
  # ne_p= B21 + B22*neE + B23*neN + B24*neE*neN
  # se_p= B21 + B22*seE + B23*seN + B24*seE*seN
  
  ##l=line p=pixel
  sw_p <- a0 + a1*swN + a2*swE + a3*swN*swE + a4*swN^2 + a5*swE^2 + a6*swN^2*swE + a7*swN*swE^2 + a8*swN^3 + a9*swE^3
  nw_p <- a0 + a1*nwN + a2*nwE + a3*nwN*nwE + a4*nwN^2 + a5*nwE^2 + a6*nwN^2*nwE + a7*nwN*nwE^2 + a8*nwN^3 + a9*nwE^3
  ne_p <- a0 + a1*neN + a2*neE + a3*neN*neE + a4*neN^2 + a5*neE^2 + a6*neN^2*neE + a7*neN*neE^2 + a8*neN^3 + a9*neE^3
  se_p <- a0 + a1*seN + a2*seE + a3*seN*seE + a4*seN^2 + a5*seE^2 + a6*seN^2*seE + a7*seN*seE^2 + a8*seN^3 + a9*seE^3
  
  sw_l <- b0 + b1*swN + b2*swE + b3*swN*swE + b4*swN^2 + b5*swE^2 + b6*swN^2*swE + b7*swN*swE^2 + b8*swN^3 + b9*swE^3
  nw_l <- b0 + b1*nwN + b2*nwE + b3*nwN*nwE + b4*nwN^2 + b5*nwE^2 + b6*nwN^2*nwE + b7*nwN*nwE^2 + b8*nwN^3 + b9*nwE^3
  ne_l <- b0 + b1*neN + b2*neE + b3*neN*neE + b4*neN^2 + b5*neE^2 + b6*neN^2*neE + b7*neN*neE^2 + b8*neN^3 + b9*neE^3
  se_l <- b0 + b1*seN + b2*seE + b3*seN*seE + b4*seN^2 + b5*seE^2 + b6*seN^2*seE + b7*seN*seE^2 + b8*seN^3 + b9*seE^3
  
  lpcoords <-rbind(c(sw_l, sw_p), c(nw_l, nw_p),c(ne_l, ne_p), c(se_l, se_p), c(sw_l, sw_p)) ##l=line p=pixel
  
  Sr1<- Polygon(lpcoords)
  Srs1<- Polygons(list(Sr1),"sr1")
  SpP<-SpatialPolygons(list(Srs1))
  #SpatialPolygons(lpcorners)
  plotarea<-SpP@polygons[[1]]@area
  
  # utmext <- extent(303438,303521,5125112,5125367)
  #extract(rasterHH, SpP, method='simple', buffer=NULL, small=T, fun=mean, df=T)
  HHcells<-as.data.frame(extract(rasterHH, SpP, method='simple',cellnumbers=T)[[1]])
  HVcells<-as.data.frame(extract(rasterHV, SpP, method='simple',cellnumbers=T)[[1]])
#   par(mfrow=c(1,1))
#   plot(HHcells$value,HVcells$value)
  
  HHcells$cell==HVcells$cell ##check to make sure HH and HV are aligned
  
  cells <- HHcells$cell
  # r<-rasterFromCells(rasterHH,cells)
  #r@extent ##useful, but I'm not using this yet
  rows<-unique(rowFromCell(rasterHH,cells))
  cols<-unique(colFromCell(rasterHH,cells))
  HH<-matrix(NA,length(rows),length(cols))
  HV<-matrix(NA,length(rows),length(cols))

#######
##This step is slow and inelegant. I am filling each cell in an m x n matrix
  for(j in 1:length(rows)){
    for(k in 1:length(cols)){
      HH[j,k]<-rasterHH[rows[j],cols[k]]
      HV[j,k]<-rasterHV[rows[j],cols[k]]
      #print(c("k=",100-(100*(k/length(cols)))))
    }
    print(c("j=",100-(100*(j/length(cols)))))
  }
image(HH, col=gray(1:255/255))
#overlay(HH,HV, fun=function(HH,HV){return(HH*HV)} )
  dates<-as.date(as.character(substr(output[2:nrow(output),2],1,8)),order='ymd')
#   jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  
  
  par(mfrow=c(1,3))
  image(HH, col=gray(1:255/255))  #, xlim=c(swE,seE), ylim=c(swN,nwN))
  title(main=c(as.character(dates[i]),'HH'))
  image(HV, col=gray(1:255/255))  #, xlim=c(swE,seE), ylim=c(swN,nwN))
  title(main=c(as.character(dates[i]),'HV'))
  image(HH-HV, col=gray(1:255/255))
  title(main=c(as.character(dates[i]),'HH-HV'))
  
  scn_metadata$scnid[i]
}

# image(xyLayer(HH,HV),asp=1)
# 
# cbind(1:ncell(r), getValues(r))
# 
# nrow = sum(diff(HH$cell)>1)+1
# #ncol = ceiling(length(HH$value)/nrow)
# ncol=1;myrow = 1; mycol=1
# for(i in 1:length(HH$value)){  
#   if(c(diff(HH$cell),1)[i]>1){
#     ncol = max(c(ncol,mycol))
#     myrow = myrow + 1
#     mycol = 1
#   } else{
#     mycol = mycol + 1
#   }  
# }
# HHmat = matrix(NA,nrow,ncol)
# myrow = 1; mycol=1
# for(i in 1:length(HH$value)){  
#   HHmat[myrow,mycol] = HH$value[i]
#   if(c(diff(HH$cell),1)[i]>1){
#     myrow = myrow + 1
#     mycol = 1
#   } else{
#     mycol = mycol + 1
#   }  
# }
# image(HHmat)




 



