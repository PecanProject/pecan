### PRISM climate data extraction 
### for FIA plot locations

# download of PRISM netcdf files was here: http://climexp.knmi.nl/selectfield_obs2.cgi?id=someone@somewhere

setwd("C:/Users/mekevans/Documents/Cdrive/Bayes/DemogRangeMod/ProofOfConcept/treerings/FIAmetadata/ArizonaData/MergedDatabase/New")
### deal with trees that don't have cores
# load in data
Tree2Tree <- read.csv("AZ-PIPO-T1-T2-6-29-17.csv", header = T)

### extract PRISM climate data at FIA plot locations
### step 1: extract FIA plot locations (14,155 tree-to-tree data points)
FIA.coord<-data.frame(ID1=rep("FIA_AZ_PIPO",nrow(Tree2Tree)),
                      ID2=paste0(Tree2Tree$COUNTYCD, Tree2Tree$T1_PLOT),
                      lat=Tree2Tree$LAT,
                      lon=Tree2Tree$LON,
                      el=Tree2Tree$ELEV)
FIA.coord$el<-round(FIA.coord$el*0.3048,0)
#write.csv(FIA.coord,"tree2tree_coords.csv", quote=F, row.names=F)
max.lat <- round(max(FIA.coord$lat), 0); min.lat <- round(min(FIA.coord$lat), 0)
max.lon <- round(max(FIA.coord$lon), 0); min.lon <- round(min(FIA.coord$lon), 0)
require(ncdf4)
library(fields)

# Load ncdf files
ppt_prism <- nc_open("ppt_prism.nc")
xdim_ppt <- round(ppt_prism$dim[[1]]$vals, digits = 5)# lon (length 1405)
ydim_ppt <- round(ppt_prism$dim[[2]]$vals, digits = 5) # lat (length 621)
zdim_ppt <- ppt_prism$dim[[3]]$vals # time (1452 = 12months*121years)

tmax_prism <- nc_open("tmax_prism.nc")
xdim_tmax <- round(tmax_prism$dim[[1]]$vals, digits = 5)# lon
ydim_tmax <- round(tmax_prism$dim[[2]]$vals, digits = 5) # lat
zdim_tmax <- tmax_prism$dim[[3]]$vals # time

tmin_prism <- nc_open("tmin_prism.nc")
xdim_tmin <- round(tmin_prism$dim[[1]]$vals, digits = 5)# lon
ydim_tmin <- round(tmin_prism$dim[[2]]$vals, digits = 5) # lat
zdim_tmin <- tmin_prism$dim[[3]]$vals # time

#tmean_prism <- nc_open("tmean_prism.nc")
#xdim_tmin <- round(tmin_prism$dim[[1]]$vals, digits = 5)# lon
#ydim_tmin <- round(tmin_prism$dim[[2]]$vals, digits = 5) # lat
#zdim_tmin <- tmin_prism$dim[[3]]$vals # time

#boundary coordinates to crop the big ncdf file
xstart <- which(xdim_ppt == -114.0) #westernmost location, should be xx.0 or xx.5, otherwise the maximum decimal is .91667
ystart <-  which(ydim_ppt == 32.0) #southernmost location
xstop <- which(xdim_ppt == -109.0) #easternmost location 
ystop <-  which(ydim_ppt == 36.0) #northernmost location
zs <- 1 # z-dimension start at Jan 1895, first entry

Clim_var.nc_ppt <- ncvar_get(ppt_prism, start = c(xstart,ystart,zs), count = c(xstop-xstart+1,ystop-ystart+1,dim(zdim_ppt)))     #1895-2015,crop the whole PRISM grid to the above-defined area 
rm(ppt_prism)
Clim_var.nc_tmin <- ncvar_get(tmin_prism, start = c(xstart,ystart,zs), count = c(xstop-xstart+1,ystop-ystart+1,dim(zdim_ppt)))     #1895-2015
rm(tmin_prism)
Clim_var.nc_tmax <- ncvar_get(tmax_prism, start = c(xstart,ystart,zs), count = c(xstop-xstart+1,ystop-ystart+1,dim(zdim_ppt)))    #1895-2015
rm(tmax_prism)
#Clim_var.nc_tmean <- ncvar_get(tmean_prism, start = c(xstart,ystart,zs), count = c(xstop-xstart+1,ystop-ystart+1,dim(zdim_ppt)))    #1895-2015
#rm(tmean_prism)

dim(Clim_var.nc_ppt)[1] # 121 (longitude)
dim(Clim_var.nc_ppt)[2] # 97 (latitude)
dim(Clim_var.nc_ppt)[3] # 1452 (values - 12months*121years)

longitude <- xdim_ppt[c(xstart:xstop)] #express x and y in coordinates
latitude <- ydim_ppt[c(ystart:ystop)]
#la <- rep(latitude, each=c(xstart:xstop)) only needed for SPEI/Waterbalance/PET calculation, but it's probably faster to do that after the plot location extraction
start_yr.nc<-1895# start year of the climate variable
n_clim <- dim(zdim_ppt)/12 # monthly (121 years of data)
clim_time <- as.numeric(seq(start_yr.nc, start_yr.nc+(n_clim-1),by = 1)) # 121 years of data

ppt_data_2dim<- matrix(Clim_var.nc_ppt,
                       nrow = dim(zdim_ppt),ncol = dim(Clim_var.nc_ppt)[1]*dim(Clim_var.nc_ppt)[2],byrow = T) #transform the cropped area into a matrix
tmin_data_2dim<- matrix(Clim_var.nc_tmin,
                        nrow = dim(zdim_ppt),ncol = dim(Clim_var.nc_ppt)[1]*dim(Clim_var.nc_ppt)[2],byrow = T)
tmax_data_2dim<- matrix(Clim_var.nc_tmax,
                        nrow = dim(zdim_ppt),ncol = dim(Clim_var.nc_ppt)[1]*dim(Clim_var.nc_ppt)[2],byrow = T)

#tmean_data_2dim<- matrix(Clim_var.nc_tmean,
#                         nrow = dim(zdim_ppt),ncol = dim(Clim_var.nc_ppt)[1]*dim(Clim_var.nc_ppt)[2],byrow = T)

# plot test
image.plot(Clim_var.nc_tmax[,,7]) #plots your cropped area, snapshot on 7th timestep, july 1895, just to see if the cropping worked and there's data in the matrix
# see? it's the Mogollon Rim

###the following were some SPEI calculation trials
# PET_prism_harg_test<- hargreaves(tmin_data_2dim, tmax_data_2dim, 
#                                  Ra = NA, lat = la, Pre = NA, na.rm = T)
# PET_prism_harg_test_reorg <- array(data = PET_prism_harg_test, dim = c(1452, 200,200))
# image.plot(PET_prism_harg_test_reorg[7,,])

#PET_prism_harg_test.1 <- thornthwaite(tmean_data_2dim, lat=la, na.rm = T)
#PET_prism_harg_test.1_reorg <- array(data = PET_prism_harg_test.1, dim = c(1452, 200,200))
#image.plot(PET_prism_harg_test.1_reorg[7,,])


#dim(PET_prism_harg_test.1)


#dat <- ppt_data_2dim-PET_prism_harg_test.1


#system.time(SPEI_prism_2D <-apply(dat,2,function(x)spei(x ,scale=3, na.rm = TRUE)$fitted))
#plot.ts(SPEI_prism_2D[,7])
#str(SPEI_prism_2D[,1])
#mat <- array(data = SPEI_prism_2D, dim = c(1452, 200,200))

#dim(mat)

#image.plot(mat[7,,])
#plot.ts(Clim_var.nc_ppt[200,200,])
#SPEI_prism_2D[,18]==(mat[,18,1])
#plot.ts(mat[,1,2])

#image.plot(mat[7,,])


#####extract coordinates#####
#system.time(ncvar_get(ppt_prism, start = c( which.min(abs(xdim_ppt- -110.48)),which.min(abs(ydim_ppt- 36.9995)),1), count = c(1,1,dim(zdim_ppt))))   
#which(AZFIA_PIPO$ID2=="170005")
#Clim_var.nc_ppt[which.min(abs(longitude-AZFIA_PIPO$lon[i])),which.min(abs(latitude-AZFIA_PIPO$lat[i])),1:5]

PRISM_ppt_AZFIA_PIPO<-matrix(NA,nrow=1452,ncol=length(FIA.coord$ID2)) #set up an empty matrix. Speeds up the loop! 1452= 121 years *12 months, ID2 can be any column, probably ncol=nrow(YOURDATAFRAME) would work as well
PRISM_tmin_AZFIA_PIPO<-matrix(NA,nrow=1452,ncol=length(FIA.coord$ID2))
PRISM_tmax_AZFIA_PIPO<-matrix(NA,nrow=1452,ncol=length(FIA.coord$ID2))
for(i in 1:length(FIA.coord$ID2)){
  PRISM_ppt_AZFIA_PIPO[,i]<-Clim_var.nc_ppt[which.min(abs(longitude-FIA.coord$lon[i])),which.min(abs(latitude-FIA.coord$lat[i])),]  # extract the PRISM cell which is closest to the target (FIAplot) coordinates, you might need to change lon/lat to LON/LAT depending on your respective column names of course.
  PRISM_tmin_AZFIA_PIPO[,i]<-Clim_var.nc_tmin[which.min(abs(longitude-FIA.coord$lon[i])),which.min(abs(latitude-FIA.coord$lat[i])),]
  PRISM_tmax_AZFIA_PIPO[,i]<-Clim_var.nc_tmax[which.min(abs(longitude-FIA.coord$lon[i])),which.min(abs(latitude-FIA.coord$lat[i])),]
}
colnames(PRISM_ppt_AZFIA_PIPO)<-FIA.coord$ID2 # name the columns with plot ID
colnames(PRISM_tmin_AZFIA_PIPO)<-FIA.coord$ID2
colnames(PRISM_tmax_AZFIA_PIPO)<-FIA.coord$ID2

##you can also skip the following step
write.csv(PRISM_ppt_AZFIA_PIPO,row.names=F,file="PRISM_ppt_AZFIA_PIPO.csv") #write/export matrix # 
write.csv(PRISM_tmin_AZFIA_PIPO,row.names=F,file="PRISM_tmin_AZFIA_PIPO.csv")
write.csv(PRISM_tmax_AZFIA_PIPO,row.names=F,file="PRISM_tmax_AZFIA_PIPO.csv")

##now you need to transform those values into strings
#ncol(PRISM_tmax_AZFIA_PIPO) should match nrow(AZFIA_PIPO)
for(i in 1:ncol(PRISM_tmax_AZFIA_PIPO)){
  
  testmatrix<-matrix(PRISM_ppt_AZFIA_PIPO[,i],ncol=12,byrow=T)
  testmatrix2<-matrix(PRISM_tmin_AZFIA_PIPO[,i],ncol=12,byrow=T)
  testmatrix3<-matrix(PRISM_tmax_AZFIA_PIPO[,i],ncol=12,byrow=T)
  
  FIA.coord$PPTJan[i]<-paste0(testmatrix[,1],collapse=",")
  FIA.coord$PPTFeb[i]<-paste0(testmatrix[,2],collapse=",")
  FIA.coord$PPTMar[i]<-paste0(testmatrix[,3],collapse=",")
  FIA.coord$PPTApr[i]<-paste0(testmatrix[,4],collapse=",")
  FIA.coord$PPTMay[i]<-paste0(testmatrix[,5],collapse=",")
  FIA.coord$PPTJun[i]<-paste0(testmatrix[,6],collapse=",")
  FIA.coord$PPTJul[i]<-paste0(testmatrix[,7],collapse=",")
  FIA.coord$PPTAug[i]<-paste0(testmatrix[,8],collapse=",")
  FIA.coord$PPTSep[i]<-paste0(testmatrix[,9],collapse=",")
  FIA.coord$PPTOct[i]<-paste0(testmatrix[,10],collapse=",")
  FIA.coord$PPTNov[i]<-paste0(testmatrix[,11],collapse=",")
  FIA.coord$PPTDec[i]<-paste0(testmatrix[,12],collapse=",")
  ##same for tmin
  FIA.coord$TMINJan[i]<-paste0(testmatrix2[,1],collapse=",")
  FIA.coord$TMINFeb[i]<-paste0(testmatrix2[,2],collapse=",")
  FIA.coord$TMINMar[i]<-paste0(testmatrix2[,3],collapse=",")
  FIA.coord$TMINApr[i]<-paste0(testmatrix2[,4],collapse=",")
  FIA.coord$TMINMay[i]<-paste0(testmatrix2[,5],collapse=",")
  FIA.coord$TMINJun[i]<-paste0(testmatrix2[,6],collapse=",")
  FIA.coord$TMINJul[i]<-paste0(testmatrix2[,7],collapse=",")
  FIA.coord$TMINAug[i]<-paste0(testmatrix2[,8],collapse=",")
  FIA.coord$TMINSep[i]<-paste0(testmatrix2[,9],collapse=",")
  FIA.coord$TMINOct[i]<-paste0(testmatrix2[,10],collapse=",")
  FIA.coord$TMINNov[i]<-paste0(testmatrix2[,11],collapse=",")
  FIA.coord$TMINDec[i]<-paste0(testmatrix2[,12],collapse=",")
  #and tmax
  FIA.coord$TMAXJan[i]<-paste0(testmatrix3[,1],collapse=",")
  FIA.coord$TMAXFeb[i]<-paste0(testmatrix3[,2],collapse=",")
  FIA.coord$TMAXMar[i]<-paste0(testmatrix3[,3],collapse=",")
  FIA.coord$TMAXApr[i]<-paste0(testmatrix3[,4],collapse=",")
  FIA.coord$TMAXMay[i]<-paste0(testmatrix3[,5],collapse=",")
  FIA.coord$TMAXJun[i]<-paste0(testmatrix3[,6],collapse=",")
  FIA.coord$TMAXJul[i]<-paste0(testmatrix3[,7],collapse=",")
  FIA.coord$TMAXAug[i]<-paste0(testmatrix3[,8],collapse=",")
  FIA.coord$TMAXSep[i]<-paste0(testmatrix3[,9],collapse=",")
  FIA.coord$TMAXOct[i]<-paste0(testmatrix3[,10],collapse=",")
  FIA.coord$TMAXNov[i]<-paste0(testmatrix3[,11],collapse=",")
  FIA.coord$TMAXDec[i]<-paste0(testmatrix3[,12],collapse=",")
}

# add the PRISM strings to the Tree2Tree object
Tree2Tree <- c(Tree2Tree, FIA.coord[,6:41])

# write that to csv
write.csv(Tree2Tree,row.names=F,file="Tree2Tree.csv")

#### follow up with the CMD stuff
library(SPEI)
#and so on