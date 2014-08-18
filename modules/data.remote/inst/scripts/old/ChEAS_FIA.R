##Author Brady S. Hardiman 11/12/2013

##Read in fuzzed FIA coordinates supplied by Andy Finley (MSU), extract palsar backscatter values, fit curves, save figures and extracted values (with coordinates)

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
kml=0 #1 = generate and save kml files of extraction coordinates; 0 = do not generate new kml
fia=1 #1 = use FIA coordinates, 0 = use WLEF/Park Falls Tower coordinates
metadata<- read.csv("~/git/pecan/modules/data.remote/output/metadata/output_metadata.csv", sep="\t", header=T)
################################
## Read in coordinate data for calibration of PALSAR backscatter returns
##    Uses PALSAR metadata file to determine geographic extent of available PALSAR data and crops extraction coord set 
##      to match PALSAR extent. Reprojects extraction coords to match PALSAR geotiffs.
################################
if(fia==1){ #EXTRACTS FROM FIA COORDINATES
  calib_inpath <-"/home/bhardima/git/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords"
  calib_infile <-read.csv(file.path(calib_inpath,"wi-biomass-fuzzed.csv"), sep=",", header=T) #Wisconsin FIA plots
  coords<-data.frame(calib_infile$FUZZED_LON,calib_infile$FUZZED_LAT) #lon and lat (ChEAS: UTM Zone 15N NAD83)
  Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#   wi.fia<-data.frame(calib_infile$FUZZED_LAT,calib_infile$FUZZED_LON)
  latlon<-data.frame(calib_infile$FUZZED_LAT,calib_infile$FUZZED_LON)
#   FIA.points <- SpatialPointsDataFrame(Sr1, wi.fia) #convert to class="SpatialPointsDataFrame" for export as kml
  spdf.latlon <- SpatialPointsDataFrame(Sr1, latlon) #convert to class="SpatialPointsDataFrame" for export as kml
  if(kml==1){writeOGR(spdf.latlon, layer=1, "WI_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 
  }
}

if(fia==0){#EXTRACTS FROM WLEF COORDINATES
  calib_inpath <-"/home/bhardima/git/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords"
  calib_infile <-read.csv(file.path(calib_inpath,"biometry_trimmed.csv"), sep="\t", header=T) #WLEF plots
  
#   upid<-paste(calib_infile$plot,calib_infile$subplot,sep='.') #create unique plot identifier
#   calib_infile<-cbind(calib_infile[,1:2],upid,calib_infile[,3:8])
  calib_infile<-aggregate(calib_infile, list(calib_infile[,1]), mean)
  calib_infile$plot<-calib_infile$Group.1
  calib_infile<-cbind(calib_infile[,2],calib_infile[,5:9])
  colnames(calib_infile)<-c("plot","easting","northing","adult_density","sapling_density","ABG_biomass")
  
  coords<-data.frame(calib_infile$easting,calib_infile$northing) #eastings and northings (ChEAS: UTM Zone 15N NAD83)
  Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  wlef<-data.frame(paste(calib_infile$plot,calib_infile$subplot,sep="_"))
  epsg4326String <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  Sr1_4google <- spTransform(Sr1,epsg4326String) #class=SpatialPoints
  Sr1_4google <- SpatialPointsDataFrame(Sr1_4google, wlef) #convert to class="SpatialPointsDataFrame" for export as kml
  if(kml==1){writeOGR(Sr1_4google, layer=1, "WLEF.kml", driver="KML") #export as kml (this puts in in the Home folder)
  }
}

## corner coords for cheas domain based on avaialable PALSAR data. (Maybe switch to bounding.box.xy()? )
ChEAS_PLASAR_extent <-rbind(cbind(min(metadata$scn_nwlon),max(metadata$scn_nwlat)),
                            cbind(max(metadata$scn_nelon),max(metadata$scn_nelat)),
                            cbind(max(metadata$scn_selon),min(metadata$scn_selat)),
                            cbind(min(metadata$scn_swlon),min(metadata$scn_swlat)),
                            cbind(min(metadata$scn_nwlon),max(metadata$scn_nwlat))) 
                            
ChEAS_PLASAR_extent<- Polygon(ChEAS_PLASAR_extent) #spatial polygon from cheas-palsar extent
Srs1<- Polygons(list(ChEAS_PLASAR_extent),"ChEAS_PLASAR_extent") #spatial polygons (plural)
ChEAS_PLASAR_extent<-SpatialPolygons(list(Srs1),proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

Sr1<-spTransform(Sr1,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# FIA.in.cheas<-as.vector(over(FIA.points,ChEAS_PLASAR_extent)) #subset of FIA plots that falls within Cheas-PALSAR extent
coords.in.cheas<-as.vector(over(Sr1,ChEAS_PLASAR_extent)) #subset of plots that falls within Cheas-PALSAR extent

# FIA.in.cheas[is.na(FIA.in.cheas)]<-0 #replace na's with 0's for indexing
coords.in.cheas[is.na(coords.in.cheas)]<-0 #replace na's with 0's for indexing

##Plot Biomass source data
if(fia==1){
  biomass<-calib_infile[as.logical(coords.in.cheas),4] #for FIA
} else{
  biomass<-calib_infile[as.logical(coords.in.cheas),'ABG_biomass'] #for WLEF
}

##Plot ID will be used later on for generating time series of backscatter values
if(fia==1){
  plot<-NA #for FIA NOTE: Add in FIA plot unique identifiers if available 
} else{
  plot<-calib_infile[as.logical(coords.in.cheas),'plot'] #for WLEF
}

## Subset extraction coords that fall within PALSAR observation area
# cheasFIA<-Sr1@coords[FIA.in.cheas==1,] 
cheas.coords<-Sr1@coords[coords.in.cheas==1,] ##subset of coords that falls within Cheas-PALSAR extent
# spcheasFIA <- SpatialPoints(cheasFIA,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
spcheascoords <- SpatialPoints(cheas.coords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# writeOGR(cheasFIA, layer=1, "cheas_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 

################################
## Begin extracting PALSAR values at FIA plot coordinates
################################
palsar_inpath <- file.path("/home/bhardima/git/pecan/modules/data.remote/palsar_scenes/Link_to_cheas/geo_corrected_single_sigma")

# file.info<-read.table(file="/home/bhardima/git/pecan/modules/data.remote/output/metadata/output_metadata.csv",header=T,sep="\t")

# date<-as.Date(metadata$scndate, format='%Y%m%d')
# col_names<-c(rbind(paste(date, "HH",sep="_"),paste(date, "HV",sep="_")))

pol_bands<-c("HH", "HV")
numfiles<-length(list.files(file.path(palsar_inpath, pol_bands[1]), pattern=".tif" ,recursive=F))

# lake_extracted<-matrix(NA, nrow(lake_coords),length(pol_bands)*numfiles)
# disturbance_extracted_40m<-matrix(NA, nrow(disturbance_coords),length(pol_bands)*numfiles)
# 
# colnames(lake_extracted)<-col_names
# colnames(disturbance_extracted)<-col_names
# colnames(disturbance_extracted_40m)<-col_names

# extracted_48m<-matrix(NA, nrow=length(spcheasFIA)*numfiles, ncol=length(pol_bands)) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
# extracted_60m<-matrix(NA, nrow=length(spcheasFIA)*numfiles, ncol=length(pol_bands)) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
# extracted_48m<-matrix(NA, nrow=numfiles*nrow(spcheasFIA@coords),ncol=7) #matrix to store extracted palsar values. 
# extracted_60m<-matrix(NA, nrow=numfiles*nrow(spcheasFIA@coords),ncol=7) #matrix to store extracted palsar values. 
extracted_48m<-matrix(NA, nrow=numfiles*nrow(spcheascoords@coords),ncol=8) #matrix to store extracted palsar values. 
extracted_60m<-matrix(NA, nrow=numfiles*nrow(spcheascoords@coords),ncol=8) #matrix to store extracted palsar values. 

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
        ## Check each extraction coordinate to see if it falls inside the bounding box of this palsar scene. 
        ## Only extract the ones that do.
        ## NOTE: The bounding box for the PALSAR scene will be larger than the PALSAR scene itself (due to a tilted orbital path).
        ##        This means that the extraction loop will some number of palsar scenes that have all zeros for the backscatter values.
        ##        These zeros are truncated in post processing, prior to curve fitting.
        ################################################
#         spcheasFIA<-spTransform(spcheasFIA,HH_rast@crs) #Convert coords being extracted to CRS of PALSAR raster files
        spcheascoords<-spTransform(spcheascoords,HH_rast@crs) #Convert coords being extracted to CRS of PALSAR raster files
        rast_box<-bbox(HH_rast@extent) #bounding box of single palsar scene
        rast_Poly<-Polygon(rbind(
          c(rast_box[1,1],rast_box[2,2]),
          c(rast_box[1,2],rast_box[2,2]),
          c(rast_box[1,2],rast_box[2,1]),
          c(rast_box[1,1],rast_box[2,1]),
          c(rast_box[1,1],rast_box[2,2]))) #polygon from bbox
        Srs1<- Polygons(list(rast_Poly),"PALSAR_extent") #spatial polygon
        pals_ext<-SpatialPolygons(list(Srs1),proj4string=HH_rast@crs) 
        
#         fia.in.rast<-over(spcheasFIA,pals_ext) #FIA coords (already filtered to fall within the ChEAS domain) that fall within this palsar scene
#         fia.in.rast[is.na(fia.in.rast)]<-0 #replace na's with 0's for indexing
#         fia.in.rast<-as.logical(fia.in.rast)
        coords.in.rast<-over(spcheascoords,pals_ext) #extraction coords (already filtered to fall within the ChEAS domain) that fall within this palsar scene
        coords.in.rast[is.na(coords.in.rast)]<-0 #replace na's with 0's for indexing
        if(max(coords.in.rast)!=1){
          next
        }
        coords.in.rast<-as.logical(coords.in.rast)
        
  ################################
  ##calibration PASLAR data from extraction coords (mean of pixles w/in 48m buffer radius)
  ################################
#         HH_data_48m<-extract(HH_rast, spcheasFIA[fia.in.rast], method="simple",buffer=48, small=T, fun=mean) #this step is very slow
#         HV_data_48m<-extract(HV_rast, spcheasFIA[fia.in.rast], method="simple",buffer=48, small=T, fun=mean) #this step is very slow
        HH_data_48m<-extract(HH_rast, spcheascoords[coords.in.rast], method="simple",buffer=48, small=T, fun=mean) #this step is very slow
        HV_data_48m<-extract(HV_rast, spcheascoords[coords.in.rast], method="simple",buffer=48, small=T, fun=mean) #this step is very slow
        
        filename<-matrix(substr(as.character(HV_filelist[i]),1,15),nrow=length(HH_data_48m),ncol=1)
        palsar_date<-matrix(as.character(as.Date(substr(as.character(metadata$scndate[metadata$scnid==filename[1]]),1,8),"%Y%m%d")),nrow=length(HH_data_48m),ncol=1)
        
#         all_48<-cbind(filename,palsar_date,spcheasFIA[fia.in.rast]@coords,biomass[fia.in.rast],HH_data_48m,HV_data_48m)
        all_48<-cbind(filename,palsar_date,calib_infile$plot[coords.in.rast],spcheascoords[coords.in.rast]@coords,biomass[coords.in.rast],HH_data_48m,HV_data_48m)
        if(i==1){
#           extracted_48m[i : nrow(spcheasFIA[fia.in.rast]@coords),]<-all_48
          extracted_48m[i : nrow(spcheascoords[coords.in.rast]@coords),]<-all_48
        }else if(i>1){
#           extracted_48m[((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+1) : ((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+nrow(spcheasFIA[fia.in.rast]@coords)),1:7]<-all_48
          extracted_48m[((i-1)*nrow(spcheascoords[coords.in.rast]@coords)+1) : ((i-1)*nrow(spcheascoords[coords.in.rast]@coords)+nrow(spcheascoords[coords.in.rast]@coords)),1:8]<-all_48
        }
  ###############################
  #calibration PASLAR data from extraction coords (mean of pixles w/in 60m buffer radius)
  ###############################
#         HH_data_60m<-extract(HH_rast, spcheasFIA[fia.in.rast], method="simple",buffer=60, small=T, fun=mean) #this step is very slow
#         HV_data_60m<-extract(HV_rast, spcheasFIA[fia.in.rast], method="simple",buffer=60, small=T, fun=mean) #this step is very slow
        HH_data_60m<-extract(HH_rast, spcheascoords[coords.in.rast], method="simple",buffer=60, small=T, fun=mean) #this step is very slow
        HV_data_60m<-extract(HV_rast, spcheascoords[coords.in.rast], method="simple",buffer=60, small=T, fun=mean) #this step is very slow
        
        filename<-matrix(substr(as.character(HV_filelist[i]),1,15),nrow=length(HH_data_60m),ncol=1)
        colnames(filename)<-"scnid"
        palsar_date<-matrix(as.character(as.Date(substr(as.character(metadata$scndate[metadata$scnid==filename[1]]),1,8),"%Y%m%d")),nrow=length(HH_data_60m),ncol=1)
        colnames(palsar_date)<-"scndate"
        
#         all_60<-cbind(filename,palsar_date,spcheasFIA[fia.in.rast]@coords,biomass[fia.in.rast],HH_data_60m,HV_data_60m)
        all_60<-cbind(filename,palsar_date,calib_infile$plot[coords.in.rast],spcheascoords[coords.in.rast]@coords,biomass[coords.in.rast],HH_data_60m,HV_data_60m)
        if(i==1){
#           extracted_60m[i : nrow(spcheasFIA[fia.in.rast]@coords),]<-all_60
          extracted_60m[i : nrow(spcheascoords[coords.in.rast]@coords),]<-all_60
        }else if(i>1){
#           extracted_60m[((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+1) : ((i-1)*nrow(spcheasFIA[fia.in.rast]@coords)+nrow(spcheasFIA[fia.in.rast]@coords)),1:7]<-all_60
          extracted_60m[((i-1)*nrow(spcheascoords[coords.in.rast]@coords)+1) : ((i-1)*nrow(spcheascoords[coords.in.rast]@coords)+nrow(spcheascoords[coords.in.rast]@coords)),1:8]<-all_60
        }
      
    print(paste("i=",i,sep=""))
#     print(nrow(spcheasFIA[fia.in.rast]@coords))
#     print(paste("j=",j,sep=""))
#   }
}
# extracted_48m<-na.omit(extracted_48m)
# extracted_60m<-na.omit(extracted_60m)
colnames(extracted_48m)<-c("scnid","scndate", "plot", "UTM.lat", "UTM.lon", "biomass","HH.sigma.48", "HV.sigma.48")
colnames(extracted_60m)<-c("scnid","scndate", "plot", "UTM.lat", "UTM.lon", "biomass","HH.sigma.60", "HV.sigma.60")

## Create working copy of data (so that I don't need to re-extract if I screw up the data)
## NOTE: Here I remove the NAs from coords that don't fall with in the scene and 
##       the zeros that are an artifact of the mismatch between palsar bbox dim and palsar raster dim (due to tilted orbital path)
dat48<-data.frame(na.omit(extracted_48m[extracted_48m[,7]>0 & extracted_48m[,8]>0,]))
dat60<-data.frame(na.omit(extracted_60m[extracted_48m[,7]>0 & extracted_48m[,8]>0,]))

## NOTE: Converting to dataframe changes all values to factor, so here I reformat the data
dat48$scnid<-as.character(dat48$scnid)
dat48$scndate<-as.Date(dat48$scndate,"%Y-%M-%d")
dat48$UTM.lat<- as.numeric(as.character(dat48$UTM.lat))
dat48$UTM.lon<- as.numeric(as.character(dat48$UTM.lon))
dat48$biomass<- as.numeric(as.character(dat48$biomass))
dat48$HH.sigma.48<- as.numeric(as.character(dat48$HH.sigma.48))
dat48$HV.sigma.48<- as.numeric(as.character(dat48$HV.sigma.48))
write.table(dat48,file="dat48.csv",quote=FALSE,sep=",",row.names=F)

dat60$scnid<-as.character(dat60$scnid)
dat60$scndate<-as.Date(dat60$scndate,"%Y-%M-%d")
dat60$UTM.lat<- as.numeric(as.character(dat60$UTM.lat))
dat60$UTM.lon<- as.numeric(as.character(dat60$UTM.lon))
dat60$biomass<- as.numeric(as.character(dat60$biomass))
dat60$HH.sigma.60<- as.numeric(as.character(dat60$HH.sigma.60))
dat60$HV.sigma.60<- as.numeric(as.character(dat60$HV.sigma.60))
write.table(dat60,file="dat60.csv",quote=FALSE,sep=",",row.names=F)

pdf("ExtractionQCplots.pdf",width = 6, height = 6, paper='special')
##QC plots
par(mfrow=c(1,2)) #checking coordinate alignment of different extraction buffer sizes
plot(dat48$UTM.lat,dat60$UTM.lat)
plot(dat48$UTM.lon,dat60$UTM.lon)

par(mfrow=c(1,2)) # checking extracted backscatter values for extraction buffers (should be different but not TOO different)
plot(dat48$HH.sigma.48,dat60$HH.sigma.60,xlab="48m",ylab="60m",main="HH")
plot(dat48$HV.sigma.48,dat60$HV.sigma.60,xlab="48m",ylab="60m",main="HV")

##Data Exploration
# par(mfrow=c(2,2))
# plot(dat48$biomass,dat48$HH.sigma.48,xlab="biomass",ylab="HH",main="48m")
# plot(dat48$biomass,dat48$HV.sigma.48,xlab="biomass",ylab="HV",main="48m")
# plot(dat60$biomass,dat60$HH.sigma.60,xlab="biomass",ylab="HH",main="60m")
# plot(dat60$biomass,dat60$HV.sigma.60,xlab="biomass",ylab="HV",main="60m")

par(mfrow=c(2,2))
scatter.smooth(dat48$biomass,dat48$HH.sigma.48,xlab="biomass",ylab="HH",main="48m",col="grey")
scatter.smooth(dat48$biomass,dat48$HV.sigma.48,xlab="biomass",ylab="HV",main="48m",col="grey")
scatter.smooth(dat60$biomass,dat60$HH.sigma.60,xlab="biomass",ylab="HH",main="60m",col="grey")
scatter.smooth(dat60$biomass,dat60$HV.sigma.60,xlab="biomass",ylab="HV",main="60m",col="grey")

par(mfrow=c(2,2))
scatter.smooth(dat48$biomass,dat48$HH.sigma.48/dat48$HV.sigma.48,xlab="biomass",ylab="HH/HV",main="48m",col="grey")
scatter.smooth(dat60$biomass,dat60$HH.sigma.60/dat60$HV.sigma.60,xlab="biomass",ylab="HH/HV",main="60m",col="grey")
scatter.smooth(dat48$biomass,dat48$HH.sigma.48*dat48$HV.sigma.48,xlab="biomass",ylab="HHxHV",main="48m",col="grey")
scatter.smooth(dat60$biomass,dat60$HH.sigma.60*dat60$HV.sigma.60,xlab="biomass",ylab="HHxHV",main="60m",col="grey")

par(mfrow=c(1,2))
scatter.smooth(dat48$biomass,(dat48$HH.sigma.48-dat48$HV.sigma.48)/(dat48$HH.sigma.48+dat48$HV.sigma.48),xlab="biomass",ylab="(HH-HV)/(HH+HV)",main="48m", col="gray")
scatter.smooth(dat60$biomass,(dat60$HH.sigma.60-dat60$HV.sigma.60)/(dat60$HH.sigma.60+dat60$HV.sigma.60),xlab="biomass",ylab="(HH-HV)/(HH+HV)",main="60m", col="gray")
dev.off()
# sort.dat48<-dat48[with(dat48,order(dat48$scndate)),]
# sort.dat60<-dat60[with(dat60,order(dat60$scndate)),]

# ##Plot timeseries of backscatter values from each set of coords
# par(mfrow=c(2,2))
# par(new=T)
# plot(sort.dat48$scndate,sort.dat48$HH.sigma.48,ylim=c(0,1),type='n',xlab="scndate",ylab="HH",main="48m")
# for(i in unique(dat48$plot)){
#   par(new=T)
# lines(sort.dat48$scndate[sort.dat48$plot==i],sort.dat48$HH.sigma.48[sort.dat48$plot==i],type='b',col=i)
# }
# plot(sort.dat48$scndate,sort.dat48$HV.sigma.48,ylim=c(0,0.11),type='n',xlab="scndate",ylab="HV",main="48m")
# for(i in unique(dat48$plot)){
#   par(new=T)
#   lines(sort.dat48$scndate[sort.dat48$plot==i],sort.dat48$HV.sigma.48[sort.dat48$plot==i],type='b',col=i)
# }
# plot(sort.dat60$scndate,sort.dat60$HH.sigma.60,ylim=c(0,1),type='n',xlab="scndate",ylab="HH",main="60m")
# for(i in unique(dat60$plot)){
#   par(new=T)
#   lines(sort.dat60$scndate[sort.dat60$plot==i],sort.dat60$HH.sigma.60[sort.dat60$plot==i],type='b',col=i)
# }
# plot(sort.dat60$scndate,sort.dat60$HV.sigma.60,ylim=c(0,0.11),type='n',xlab="scndate",ylab="HV",main="60m")
# for(i in unique(dat60$plot)){
#   par(new=T)
#   lines(sort.dat60$scndate[sort.dat60$plot==i],sort.dat60$HV.sigma.60[sort.dat60$plot==i],type='b',col=i)
# }

# ##Average palsar values for scenes acquired within the same month?
# months(sort.dat48$scndate[i])
# format(sort.dat48$scndate[i], "%Y")
# tapply()
# plot(sort.dat48$scndate,sort.dat48$HH.sigma.48,ylim=c(0,1),type='n',xlab="scndate",ylab="HH",main="48m")

########################################################
## Curve fitting
########################################################

#######################
##Use Non-linear Least Squares (nls) to fit rectangular hyperbola NOTE:This method is not preferred.
#######################
##NOTE: backscatter=((alpha*beta*biomass)/(beta + alpha*biomass))
# buff<-c("48", "60")
# col.names<-c("pol_band", 
#              "buffer_radius(m)",
#              "biomass_R2", 
#              "mod.alpha", 
#              "pval.alpha", 
#              "alpha.ci.lower", 
#              "alpha.ci.upper",
#              "mod.beta", 
#              "pval.b", 
#              "beta.ci.upper",
#              "beta.ci.upper",
#              "num.iters", 
#              "convergence")
# mod.params<-matrix(nrow=1,ncol=length(col.names))
# colnames(mod.params)<-col.names
# 
# par(mfrow=c(length(pol_bands),length(buff)))
# for(i in 1:length(pol_bands)){
#   for(j in 1:length(buff)){
#     
#     y<-eval(parse(text=paste(paste("dat",buff[j],sep=''),paste("$",sep=''),paste(pol_bands[i],'.sigma.',buff[j],sep=''))))
#     x<-(eval(parse(text=paste(paste("dat",buff[j],sep=''),paste("$biomass",sep='')))))^0.5
#     
#     # Plot backscatter v biomass
#     plot(x, y,
#          xlab=expression(sqrt(biomass)),
#          ylab=pol_bands[i],
#          main=buff[j],
#          col=51,pch=19, cex=0.6,
#          xlim=c(min(x),max(x)), 
#          ylim=c(min(y),max(y)),
#          las=1, cex.axis=1.2)
#     
#     # Calculate rectangular hyperbola between backscatter and biomass
#     biomass_curve <- nls(formula=y ~ ((alpha*beta*x)/(beta + alpha*x)), # also in Gu 2002
#                          data=list(y = y, x = x), 
#                          start=list(alpha = .75, beta = mean(y[x > quantile(x)[4]])), 
#                          na.action="na.exclude", trace=F)
#     biomass_R2 <- 1 - var(residuals(biomass_curve)) / var(y)  # R2
#     
#     # Plot rectangular hyperbola model fit
#     mod.alpha <- summary(biomass_curve)$parameters[1]  	# alpha value
#     mod.beta  <- summary(biomass_curve)$parameters[2]		# Beta value
#     mod.biomass <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), by=0.01)	
#     mod.HH <- (mod.alpha*mod.beta*mod.biomass)/(mod.beta + mod.alpha*mod.biomass) 
#     lines(x=mod.biomass, y=mod.HH, col="black", lty=1, lwd=2.5)
#     
#     legend("topright", legend=c(paste("R^2=", format(biomass_R2, digits=2)),
#                                 paste("alpha=",format(mod.alpha,digits=2)),
#                                 paste("beta=",format(mod.beta,digits=2))), bty="n",cex=1.2)
#     
#     # Write model parameters to output file
#     num.iters  <- as.numeric(biomass_curve$convInfo[2])
#     conv 	     <- as.numeric(biomass_curve$convInfo[1])
#     pval.a	   <- as.numeric(summary(biomass_curve)$parameters[7])
#     #     if(i==1 & j==2){alpha.ci   <- as.vector(confint(lgt_curve,summary(lgt_curve)$parameters, level = 0.95))
#     #                         alpha.ci.lower   <- as.numeric(alpha.ci[1])
#     #                         alpha.ci.upper   <- as.numeric(alpha.ci[2])
#     #     }else if(i==2 & j==1){alpha.ci   <- as.vector(confint(lgt_curve,summary(lgt_curve)$parameters[1,], level = 0.95))
#     #                               alpha.ci.lower   <- as.numeric(alpha.ci[1])
#     #                               alpha.ci.upper   <- as.numeric(alpha.ci[2])
#     #     }else
#     ci               <- as.matrix(confint(biomass_curve))
#     alpha.ci.lower   <- as.numeric(ci[1,1])
#     alpha.ci.upper   <- as.numeric(ci[1,2])
#     beta.ci.lower    <- as.numeric(ci[2,1])
#     beta.ci.upper    <- as.numeric(ci[2,2])
#     pval.b	   <- as.numeric(summary(biomass_curve)$parameters[8])
#     mod.params <- rbind(mod.params,as.vector(c(pol_bands[i], 
#                                                buff[j], 
#                                                biomass_R2, 
#                                                mod.alpha, 
#                                                pval.a, 
#                                                alpha.ci.lower, 
#                                                alpha.ci.upper,
#                                                mod.beta, 
#                                                pval.b,
#                                                beta.ci.lower, 
#                                                beta.ci.upper,
#                                                num.iters, 
#                                                conv)))
#     print(paste(pol_bands[i],buff[j]))
#   }}
# mod.params<-mod.params[2:nrow(mod.params),]
# 
# xs<-seq(from=1, to=4,by=1)
# ys<-as.numeric(mod.params[,4])
# upper<-as.numeric(mod.params[,7])
# lower<-as.numeric(mod.params[,6])
# par(mfrow=c(1,1))
# plot(xs,ys,ylim=c(0,max(upper)+0.1*max(upper)),ylab="Alpha estimate",xlab="pol.band/buffer.radius")
# segments(xs,lower,xs,upper,col="black",lwd=2)
# legend("topright",legend=c("1=48,HH", "2=60,HH","3=48,HV", "4=60,HV"))
# 
# rhyp<-mod.params


#######################
##Use Maximum likelihood to fit Holling Type 4
#######################
param_est<-matrix(NA,nrow=0,ncol=8) 
# param_est[,1]<-pol_bands
# param_est[,2]<-buff

par(mfrow=c(length(pol_bands),length(buff)))
pdf("HollingIV_curvefits.pdf",width = 6, height = 6, paper='special')
for(i in 1:length(pol_bands)){
  for(j in 1:length(buff)){
    
    y<-eval(parse(text=paste(paste("dat",buff[j],sep=''),paste("$",sep=''),paste(pol_bands[i],'.sigma.',buff[j],sep='')))) #backscatter values
    x<-(eval(parse(text=paste(paste("dat",buff[j],sep=''),paste("$biomass",sep='')))))^0.5
    
#     x<-x[x<round(quantile(x, 0.99))] #trim off top 1% of biomass values
#     y<-y[x<round(quantile(x, 0.99))] #trim off top 1% of biomass values
#     max.y<-mean(y[x>=quantile(x)[4]]) #starting est. of max backscatter is taken as the mean backscatter value when biomass > the 75th quantile
    a<- mean(y[x>=quantile(x,names=FALSE)[4]])*100
# #     ki<- round(mean(x[y > (max.y/2)-0.05*max.y & y < (max.y/2)+0.05*max.y])) #value of x (biomass) at half of max.y
#     b<- round(mean(x[y > (a/2)-0.05*a & y < (a/2)+0.05*a]))
#     c<- -1
# a<-0.5
b<-quantile(x,names=FALSE)[4]
c<--1
    sd<-sd(y) #stdev of backscatter values 
#     params<-c(max.y,ki,sd)
    params<-c(a,b,c,sd)
    
    fit <- function(params,x,y){
#       max.y<-params[1]
#       ki<-params[2]
#       sd<-params[3]
      a <-params[1]
      b <-params[2]
      c <-params[3]
      sd<-params[4]
#       y.pred<-(max.y*x)/(ki+x)
      y.pred<-(a*x^2)/(b+(c*x)+x^2)
      
      LL<- -sum(dnorm(y,y.pred,sd,log=TRUE))
      return(LL)
    } #function
    
    fit.mod = optim(par=params,fit,x=x,y=y)
    fit.mod
    aic.mod<- -2*fit.mod$value + 2*length(params)
    
    params <- c(pol_bands[i],buff[j],fit.mod$par[1:3],2*fit.mod$par[2]/fit.mod$par[3],fit.mod$par[4],aic.mod) #par means parameter estimates
    param_est<-rbind(param_est, params)
    xseq = seq(0,max(x),length=1000)

    plot(x,y, xlab=expression(sqrt(biomass)),ylab=pol_bands[i],main=buff[i], #something wrong here with main
         xlim=c(min(x),max(x)), 
         ylim=c(min(y),max(y)),
         pch=16,col="#CCCCCC")
    abline(a=0,b=0)
#     lines(cbind(xseq,(as.numeric(params[3])*xseq)/(as.numeric(params[4])+xseq)),lwd=3) #closed circles and solid line for CTRL
    lines(cbind(xseq,(as.numeric(params[3])*xseq^2)/(as.numeric(params[4])+(as.numeric(params[5])*xseq)+xseq^2)),lwd=3,col="green")
    legend('topright', legend=c(paste("a=", format(fit.mod$par[1], digits=2)),
                                paste("b=",format(fit.mod$par[2],digits=2)),
                                paste("c=",format(fit.mod$par[3],digits=2)),
                                paste("aic=", format(aic.mod, digits=4))), bty="n",cex=0.85)
    
  }#for j looping over pol_bands
}#for i looping over buff
dev.off()
colnames(param_est)<-c("pol_bands","buff","a","b","c","bio.at.peak.backscatter", "sd","AIC")
param_est
write.table(param_est,file="HollingIV_param_estimates.csv",quote=FALSE,sep=",",row.names=F)
##################################
##################################


#################
##diagnotics?
#################


