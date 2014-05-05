##Author Brady S. Hardiman 11/12/2013

##Read in fuzzed FIA coordinates supplied by Andy Finley (MSU)

################################
## Load Required Packages
################################
require(rgdal)
require(raster)
require(sp)
require(RgoogleMaps)
require(maptools)
require(ggplot2)
require(car)
require(spatsta)

################################
## Options
################################

#generate and save kml files?


################################
## Read in FIA data for calibrationof PALSAR backscatter returns
##    Uses PALSAR metadata file to determine geographic extent of available PALSAR data and crops FIA data 
##      to match PALSAR extent.
##    Creates and saves KML file to show FIA points in Google Earth.
################################
##Extract FIA points
# calib_inpath <-"/home/bhardima/git/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords"
# calib_infile <-read.csv(file.path(calib_inpath,"wi-biomass-fuzzed.csv"), sep=",", header=T) #Wisconsin FIA plots
# coords<-data.frame(calib_infile$FUZZED_LON,calib_infile$FUZZED_LAT) #lat and lon (ChEAS: UTM Zone 15N NAD83)
# Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# wi.fia<-data.frame(calib_infile$FUZZED_LAT,calib_infile$FUZZED_LON)
# FIA.points <- SpatialPointsDataFrame(Sr1, wi.fia) #convert to class="SpatialPointsDataFrame" for export as kml
## writeOGR(FIA.points, layer=1, "WI_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 
################################
##Extract WLEF points
calib_inpath <-"/home/bhardima/git/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords"
calib_infile <-read.csv(file.path(calib_inpath,"biometry_trimmed.csv"), sep="\t", header=T) #WLEF plots
coords<-data.frame(calib_infile$easting,calib_infile$northing) #eastings and northings (ChEAS: UTM Zone 15N NAD83)
##Convert to class=SpatialPoints for palsar extraction (this is used for creating kml files and will be used later on)
# Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +a=6378137 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
##reproject to lat lon for export to kml
epsg4326String <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
Sr1_4google <- spTransform(Sr1,epsg4326String) #class=SpatialPoints
wlef<-data.frame(paste(calib_infile$plot,calib_infile$subplot,sep="_"))

plot_biomass<-aggregate(calib_infile$ABG_biomass, by=list(calib_infile$plot),mean)


metadata<- read.csv("~/git/pecan/modules/data.remote/output/metadata/output_metadata.csv", sep="\t", header=T)

## corner coords for cheas domain based on avaialable PALSAR data.
ChEAS_PLASAR_extent <-rbind(cbind(min(metadata$scn_nwlon),max(metadata$scn_nwlat)),
                            cbind(max(metadata$scn_nelon),max(metadata$scn_nelat)),
                            cbind(max(metadata$scn_selon),min(metadata$scn_selat)),
                            cbind(min(metadata$scn_swlon),min(metadata$scn_swlat)),
                            cbind(min(metadata$scn_nwlon),max(metadata$scn_nwlat))) 

ChEAS_PLASAR_extent<- Polygon(ChEAS_PLASAR_extent) #spatial polygon from cheas-palsar extent
Srs1<- Polygons(list(ChEAS_PLASAR_extent),"ChEAS_PLASAR_extent") #spatial polygons (plural)
ChEAS_PLASAR_extent<-SpatialPolygons(list(Srs1),proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
FIA.in.cheas<-as.vector(over(FIA.points,ChEAS_PLASAR_extent)) #subset of FIA plots that falls within Cheas-PALSAR extent
FIA.in.cheas[is.na(FIA.in.cheas)]<-0 #replace na's with 0's for indexing

# biomass<-calib_infile[as.logical(FIA.in.cheas),4]

cheasFIA<-Sr1@coords[FIA.in.cheas==1] 
# spcheasFIA <- SpatialPoints(cheasFIA,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) #For FIA
spcheasFIA <- SpatialPoints(cheasFIA,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) #For WLEF

# writeOGR(cheasFIA, layer=1, "cheas_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 

################################
## Begin extracting PALSAR values at FIA plot coordinates
################################
palsar_inpath <- file.path("/home/bhardima/git/pecan/modules/data.remote/palsar_scenes/Link_to_cheas/geo_corrected_single_sigma")

# file.info<-read.table(file="/home/bhardima/git/pecan/modules/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t")

# date<-as.Date(metadata$scndate, format='%Y%m%d')
# col_names<-c(rbind(paste(date, "HH",sep="_"),paste(date, "HV",sep="_")))

pol_bands<-c("HH", "HV")
numfiles<-length(date.time)

WLEF_extracted_48m<-matrix(NA, nrow=numfiles*nrow(spcheasFIA@coords),ncol=7) #matrix to store extracted palsar values. 
WLEF_extracted_60m<-matrix(NA, nrow=numfiles*nrow(spcheasFIA@coords),ncol=7) #matrix to store extracted palsar values. 
# extracted_48m<-matrix(NA, nrow=numfiles*nrow(spcheasFIA@coords),ncol=7) #matrix to store extracted palsar values. 
# extracted_60m<-matrix(NA, nrow=numfiles*nrow(spcheasFIA@coords),ncol=7) #matrix to store extracted palsar values. 

# colnames(extracted_48m)<-pol_bands
# colnames(extracted_60m)<-pol_bands

for(i in 1:numfiles){ 
  HH_filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[1]), pattern=".tif" ,recursive=F))
  HH_inpath<-file.path(palsar_inpath, pol_bands[1],HH_filelist[i])
  HH_rast<-raster(HH_inpath)
  
  HV_filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[2]), pattern=".tif" ,recursive=F))
  HV_inpath<-file.path(palsar_inpath, pol_bands[2],HV_filelist[i])
  HV_rast<-raster(HV_inpath)
  
  ################################################
  ##Check each FIA coordinate to see if it falls inside the bbox of this palsar scene. Only extract it if it does.
  ################################################
  spcheasFIA<-spTransform(spcheasFIA,HH_rast@crs) #Convert coords being extracted to CRS of PALSAR raster files
  rast_box<-bbox(HH_rast@extent) #bounding box of single palsar scene
  rast_Poly<-Polygon(rbind(
    c(rast_box[1,1],rast_box[2,2]),
    c(rast_box[1,2],rast_box[2,2]),
    c(rast_box[1,2],rast_box[2,1]),
    c(rast_box[1,1],rast_box[2,1]),
    c(rast_box[1,1],rast_box[2,2]))) #polygon from bbox
  Srs1<- Polygons(list(rast_Poly),"PALSAR_extent") #spatial polygon
  pals_ext<-SpatialPolygons(list(Srs1),proj4string=HH_rast@crs) 
  
  fia.in.rast<-over(spcheasFIA,pals_ext) #FIA coords (alread filtered to fall within the ChEAS domain) that fall within this palsar scene
  fia.in.rast[is.na(fia.in.rast)]<-0 #replace na's with 0's for indexing
  fia.in.rast<-as.logical(fia.in.rast)
  
  ################################
  ##calibration data from FIA plots 48m BUFFER MEAN
  ################################
  HH_data_48m<-extract(HH_rast, spcheasFIA[fia.in.rast], method="simple",buffer=48, small=T, fun=mean) #this step is very slow
  HV_data_48m<-extract(HV_rast, spcheasFIA[fia.in.rast], method="simple",buffer=48, small=T, fun=mean) #this step is very slow
  
  filename<-matrix(substr(as.character(HV_filelist[i]),1,15),nrow=length(HH_data_48m),ncol=1)
  palsar_date<-matrix(as.character(as.Date(substr(as.character(metadata$scndate[metadata$scnid==filename[1]]),1,8),"%Y%m%d")),nrow=length(HH_data_48m),ncol=1)
  
  all_48<-cbind(filename,palsar_date,spcheasFIA[fia.in.rast]@coords,biomass[fia.in.rast],HH_data_48m,HV_data_48m)
  if(i==1){
    WLEF_extracted_48m[i : nrow(spcheasFIA[fia.in.rast]@coords),]<-all_48
  }else if(i>1){
    WLEF_extracted_48m[((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+1) : ((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+nrow(spcheasFIA[fia.in.rast]@coords)),1:7]<-all_48
  }
  ###############################
  #calibration data from FIA plots 60m BUFFER MEAN
  ###############################
  HH_data_60m<-extract(HH_rast, spcheasFIA[fia.in.rast], method="simple",buffer=60, small=T, fun=mean) #this step is very slow
  HV_data_60m<-extract(HV_rast, spcheasFIA[fia.in.rast], method="simple",buffer=60, small=T, fun=mean) #this step is very slow
  
  filename<-matrix(substr(as.character(HV_filelist[i]),1,15),nrow=length(HH_data_60m),ncol=1)
  colnames(filename)<-"scnid"
  palsar_date<-matrix(as.character(as.Date(substr(as.character(metadata$scndate[metadata$scnid==filename[1]]),1,8),"%Y%m%d")),nrow=length(HH_data_60m),ncol=1)
  colnames(palsar_date)<-"scndate"
  
  all_60<-cbind(filename,palsar_date,spcheasFIA[fia.in.rast]@coords,biomass[fia.in.rast],HH_data_60m,HV_data_60m)
  if(i==1){
    WLEF_extracted_60m[i : nrow(spcheasFIA[fia.in.rast]@coords),]<-all_60
  }else if(i>1){
    WLEF_extracted_60m[((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+1) : ((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+nrow(spcheasFIA[fia.in.rast]@coords)),1:7]<-all_60
  }
  
  print(paste("i=",i,sep=""))
  #     print(nrow(spcheasFIA[fia.in.rast]@coords))
  #     print(paste("j=",j,sep=""))
  #   }
}
# extracted_48m<-na.omit(extracted_48m)
# extracted_60m<-na.omit(extracted_60m)
WLEF_dat48<-extracted_48m #Create working copy of data (so that I don't need to re-extract if I screw up the data)
WLEF_dat60<-extracted_60m #Create working copy of data (so that I don't need to re-extract if I screw up the data)
dat48<-extracted_48m #Create working copy of data (so that I don't need to re-extract if I screw up the data)
dat60<-extracted_60m #Create working copy of data (so that I don't need to re-extract if I screw up the data)

colnames(dat48)<-c("scnid","scndate", "UTM.lat", "UTM.lon", "biomass","HH.sigma.48", "HV.sigma.48")
colnames(dat60)<-c("scnid","scndate", "UTM.lat", "UTM.lon", "biomass","HH.sigma.60", "HV.sigma.60")

extracted_48m[,1]<-as.character(extracted_48m[,1])
extracted_48m[,2]<-as.Date(extracted_48m[,2],"%Y-%M-%d")
extracted_48m[,3:7]<- as.numeric(as.character(extracted_48m[,3:7]))


HH_48<-as.numeric(dat48[,6])
HV_48<-as.numeric(dat48[,7])

HH_60<-as.numeric(dat60[,6])
HV_60<-as.numeric(dat60[,7])

##QC plots
par(mfrow=c(1,2)) #checking coordinate alignment of different extraction buffer sizes
plot(dat48[,3],dat60[,3])
plot(dat48[,4],dat60[,4])

par(mfrow=c(1,2)) # checking extracted backscatter values for extraction buffers (should bedifferent but not TOO different)
plot(HH_48,HH_60,xlab="48m",ylab="60m",main="HH")
plot(HV_48,HV_60,xlab="48m",ylab="60m",main="HV")

##Data Exploration
par(mfrow=c(2,2))
plot(dat48[,5],HH_48/HV_48,xlab="biomass",ylab="HH/HV",main="48m")
plot(dat60[,5],HH_60/HV_60,xlab="biomass",ylab="HH/HV",main="60m")

plot(dat48[,5],HH_48*HV_48,xlab="biomass",ylab="HHxHV",main="48m")
plot(dat60[,5],HH_60*HV_60,xlab="biomass",ylab="HHxHV",main="60m")

par(mfrow=c(1,2))
plot(dat48[,5],(HH_48-HV_48)/(HH_48+HV_48),xlab="biomass",ylab="(HH-HV)/(HH+HV)",main="48m")
plot(dat60[,5],(HH_60-HV_60)/(HH_60+HV_60),xlab="biomass",ylab="(HH-HV)/(HH+HV)",main="60m")


########################################################
########################################################
## Curve fitting
param_est<-matrix(NA,nrow=length(spp)*length(trt),ncol=length(trt)+4) #columns for spp, trt, and 3 params,+ AIC
param_est[,1]<-spp
param_est[,2]<-trt

colnames(param_est)<-c("spp","trt","Pmax","ki","sd","AIC")

par(mfrow=c(length(spp),length(trt)))
for(j in 1:length(trt)){
  for(i in 1:length(spp)){
    
    y<-data_pts$photo[data_pts$TRT== trt[j] & data_pts$spp== spp[i]]
    x<-data_pts$par  [data_pts$TRT== trt[j] & data_pts$spp== spp[i]]
    
    Pmax<-mean(data_pts$photo[data_pts$spp==spp[i] & data_pts$TRT==trt[j] & data_pts$par>1500])
    ki<- round(mean(data_pts$par[data_pts$photo > (Pmax/2)-0.5 & data_pts$photo < (Pmax/2)+0.5]))
    sd<-sd(data_pts$photo[data_pts$spp==spp[i] & data_pts$TRT==trt[j]])
    
    params<-c(Pmax,ki,sd)
    
    rect.hyperbola <- function(params,x,y){
      Pmax<-params[1]
      ki<-params[2]
      sd<-params[3]
      
      photo_pred<-(Pmax*x)/(ki+x)
      LL<- -sum(dnorm(y,photo_pred,sd,log=TRUE))
      return(LL)
    } #function
    
    fit.recthyp = optim(par=params,rect.hyperbola,x=x,y=y)
    fit.recthyp
    aic.recthyp<- -2*fit.recthyp$value + 2*length(params)
    
    params = c(fit.recthyp$par,aic.recthyp) #this par means paramaters, not light level
    param_est[param_est[,1]==spp[i] & param_est[,2]==trt[j],3:6]<-params
    xseq = seq(0,max(x),length=1000)
    
    plot(x,y,xlab="Q (PAR)",ylab="A",main=paste(spp[i], "treatment=",trt[j]),
         xlim=c(min(data_pts$par),max(data_pts$par)), 
         ylim=c(min(data_pts$photo),max(data_pts$photo)),
         pch=16,col="#CCCCCC")
    abline(a=0,b=0)
    lines(cbind(xseq,(params[1]*xseq)/(params[2]+xseq)),lwd=3) #closed circles and solid line for CTRL
  }#for j looping over trt
}#for i looping over spp
param_est


#diagnotics?


