##Author Brady S. Hardiman 11/12/2013

##Read in fuzzed FIA coordinates supplied by Andy Finley (MSU), extract palsar backscatter values, fit curves, save figures and extracted values (with coordinates)

################################
## Load Required Packages
################################
require(sp)
require(rgdal)
require(raster)
require(chron)
require(RgoogleMaps)
require(maptools)
require(ggplot2)
require(car)
require(spatstat)
require(fields)
require(reshape)
require(rjags)
require(R2HTML)

################################
## OPTIONS
################################
kml=0 #1 = generate and save kml files of extraction coordinates; 0 = do not generate new kml
fia=0 #1 = use FIA coordinates, 0 = use WLEF/Park Falls Tower coordinates
leaf.off=0 #1=include PALSAR scenes acquired duing leaf off period of the year, 0=exclude leaf off scene dates
plot_ext=1 #Generate figures showing palsar scn extent with overlay of plot coords? (1=yes, 0=no)

# buff=c(48) #vector of buffer sizes (in meters) to extract
coord.set<-c("WLEF", "FIA")

#Brady's Linux paths
metadata<- read.csv("/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv", sep="\t", header=T) ##for Brady's Linux
palsar_inpath <- file.path("/home/bhardima/Desktop/cheas/geo_corrected_single_sigma") ##location of PALSAR raw files
calib_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords" ##location of file containing (FIA) plot coords and biomass values for calibrating PALSAR backscatter 
outpath <- file.path("/home/bhardima/pecan/modules/data.remote/output/data") ##For saving

#Brady's Mac paths
# metadata<- read.csv("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/output/metadata/output_metadata.csv", sep="\t", header=T) ##location of PALSAR metadata table
# palsar_inpath <- file.path("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/palsar_scenes/geo_corrected_single_sigma") ##location of PALSAR raw files
# calib_inpath <-"/Users/hardimanb/Desktop/data.remote(Andys_Copy)/biometry" ##location of file containing (FIA) plot coords and biomass values for calibrating PALSAR backscatter 
# outpath <- file.path("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/output/data") ##For saving

################################
## Read in coordinate data for calibration of PALSAR backscatter returns
##    Uses PALSAR metadata file to determine geographic extent of available PALSAR data and crops extraction coord set 
##      to match PALSAR extent. Reprojects extraction coords to match PALSAR geotiffs.
################################
if(fia==1){ #EXTRACTS FROM FIA COORDINATES
  calib_infile <-read.csv(file.path(calib_inpath,"wi-biomass-fuzzed.csv"), sep=",", header=T) #Wisconsin FIA plots
  coords<-data.frame(calib_infile$FUZZED_LON,calib_infile$FUZZED_LAT) #lon and lat (ChEAS: UTM Zone 15N NAD83)
  Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  latlon<-data.frame(calib_infile$FUZZED_LAT,calib_infile$FUZZED_LON)
  spdf.latlon <- SpatialPointsDataFrame(Sr1, latlon) #convert to class="SpatialPointsDataFrame" for export as kml
  if(kml==1){writeOGR(spdf.latlon, layer=1, "WI_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 
  }
}else{#EXTRACTS FROM WLEF COORDINATES
  calib_infile <-read.csv(file.path(calib_inpath,"biometry_trimmed.csv"), sep="\t", header=T) #WLEF plots
  calib_infile<-aggregate(calib_infile, list(calib_infile[,1]), mean) ##This will give errors, but these can be safely ignored
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
coords.in.cheas<-as.vector(over(Sr1,ChEAS_PLASAR_extent)) #subset of plots that falls within Cheas-PALSAR extent

# FIA.in.cheas[is.na(FIA.in.cheas)]<-0 #replace na's with 0's for indexing
coords.in.cheas[is.na(coords.in.cheas)]<-0 #replace na's with 0's for indexing

##Biomass source data
if(fia==1){
  biomass<-calib_infile[as.logical(coords.in.cheas),4] #for FIA
} else{
  biomass<-calib_infile[as.logical(coords.in.cheas),'ABG_biomass'] #for WLEF
}

## Subset extraction coords that fall within PALSAR observation area
cheas.coords<-Sr1@coords[coords.in.cheas==1,] ##subset of coords that falls within Cheas-PALSAR extent
spcheascoords <- SpatialPoints(cheas.coords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

##Plot-IDs; will be used later on for generating time series of backscatter values
if(fia==1){
  plot<-seq(1,nrow(cheas.coords),1) #for FIA NOTE: Add in FIA plot unique identifiers if available 
} else{
  plot<-calib_infile[as.logical(coords.in.cheas),'plot'] #for WLEF
}

# writeOGR(cheasFIA, layer=1, "cheas_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 

################################
## Begin extracting PALSAR values at FIA plot coordinates
################################
pol_bands<-c("HH", "HV")
numfiles<-length(list.files(file.path(palsar_inpath, pol_bands[1]), pattern=".tif" ,recursive=F))

# lake_extracted<-matrix(NA, nrow(lake_coords),length(pol_bands)*numfiles)
# disturbance_extracted_40m<-matrix(NA, nrow(disturbance_coords),length(pol_bands)*numfiles)
# 
# colnames(lake_extracted)<-col_names
# colnames(disturbance_extracted)<-col_names
# colnames(disturbance_extracted_40m)<-col_names

extracted_48m<-matrix(nrow=0, ncol=10) #matrix to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands

#Start file of scene extent figures
pdf(paste(outpath,"/",coord.set[fia+1], "_SceneExtent_with_plot_overlay.pdf",sep=""),width = 6, height = 6, paper='special')

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
        scnid<-substr(as.character(HV_filelist[i]),1,15)
        
        ##create data.frame from raster corner coords by querying metadata
        ##NOTE: I multiply the lon coord by -1 to make it work with the 'longlat' projection
        pals.ext<-Polygon(rbind(
          c(xmin(HH_rast),ymin(HH_rast)),
          c(xmin(HH_rast),ymax(HH_rast)),
          c(xmax(HH_rast),ymax(HH_rast)),
          c(xmax(HH_rast),ymin(HH_rast)),
          c(xmin(HH_rast),ymin(HH_rast))))


        ##make spatial polygon from raster extent
        pals.ext.poly<- Polygons(list(pals.ext),"pals.ext") #spatial polygons (plural)
        scn.extent<-SpatialPolygons(list(pals.ext.poly),proj4string=CRS(CRSargs(HH_rast@crs))) 
        
        scn.extent<- spTransform(scn.extent,HH_rast@crs)
          spcheascoords<-spTransform(spcheascoords,HH_rast@crs) #Convert coords being extracted to CRS of PALSAR raster files    
                    
        coords.in.rast<-over(spcheascoords,scn.extent) #extraction coords (already filtered to fall within the ChEAS domain) that fall within this palsar scene
        coords.in.rast[is.na(coords.in.rast)]<-0 #replace na's with 0's for indexing
        if(max(coords.in.rast)!=1){ #jump to next palsar file if no extraction coordinates fall within this one
          next
        }
        coords.in.rast<-as.logical(coords.in.rast)
        
  ################################
  ##calibration PASLAR data from extraction coords (mean of pixels w/in 48m buffer radius)
  ################################
        HH_data_48m<-extract(HH_rast, spcheascoords[coords.in.rast], method="simple",buffer=48, small=T, fun=mean) #Extract backscatter values from all coords in this scn. This step is very slow
        HV_data_48m<-extract(HV_rast, spcheascoords[coords.in.rast], method="simple",buffer=48, small=T, fun=mean) #Extract backscatter values from all coords in this scn. This step is very slow
    
    #extract SE's also
        #Get cell numbers of pixels within buffer of each set of coords
        buff.dim.list<-extract(HH_rast, spcheascoords[coords.in.rast], method="simple",buffer=48, small=T, cellnumbers=TRUE)
        #number of pixles in each buffer
        ncells<-matrix(unlist(lapply(buff.dim.list,dim)),nrow=2)[1,]   
        #Extract stdev of cell values in buffer, divide by sqrt(n)  
        HHse_data_48m<-extract(HH_rast, spcheascoords[coords.in.rast], method="simple",buffer=48, small=T, fun=sd)/sqrt(ncells)  
        HVse_data_48m<-extract(HV_rast, spcheascoords[coords.in.rast], method="simple",buffer=48, small=T, fun=sd)/sqrt(ncells)
        
        scnid<-matrix(substr(as.character(HV_filelist[i]),1,15),nrow=length(HH_data_48m),ncol=1) #vector of this scnid. length = number of coords in this scene
        palsar_date<-matrix(as.character(as.Date(substr(as.character(metadata$scndate[metadata$scnid==scnid[1]]),1,8),"%Y%m%d")),nrow=length(HH_data_48m),ncol=1) # same as above for scn date
        
        ##cbind for output
        if(fia==1){
          all_48<- cbind(scnid,palsar_date,plot,spcheascoords[coords.in.rast]@coords,biomass[coords.in.rast],HH_data_48m,HV_data_48m,HHse_data_48m,HVse_data_48m) #for FIA (no plot identifiers)
        } else{
          all_48<- cbind(scnid,palsar_date,as.character(calib_infile$plot[coords.in.rast]),spcheascoords[coords.in.rast]@coords,biomass[coords.in.rast],HH_data_48m,HV_data_48m,HHse_data_48m,HVse_data_48m) #for WLEF
        }

        extracted_48m<-rbind(extracted_48m,all_48)
  
  #Create figure showing palsar scn extent and overlay with plot coordinates
  #NOTE:This was created to verify that zeros int he output of palsar data actually come from plots
  #     that fall outside of a particular scene
  if(plot_ext==1){
  plot(extent(HV_rast))
  points(spcheascoords,pch=19,col="red")
  points(spcheascoords[coords.in.rast],pch=19)
  legend("top",legend=c("Inside","Outside"),pch=19,col=c("black", "red"),bty="n")
  mtext(paste(unique(scnid), unique(palsar_date),sep=" "))
  }

    print(paste("i=",i,sep=""))
    print(scnid[1])
    print(palsar_date[1])
}
dev.off()

## Create working copy of data (so that I don't need to re-extract if I screw up the data)
## NOTE: Here I remove the NAs from coords that don't fall with in the scene and 
##       the zeros that are an artifact of the mismatch between palsar bbox dim and palsar raster dim (due to tilted orbital path)
dat48<-data.frame(na.exclude(extracted_48m))
colnames(dat48)<-c("scnid","scndate", "plot", "UTM.lat", "UTM.lon", "biomass","HH.sigma.48", "HV.sigma.48","HHse_data_48m","HVse_data_48m")


## NOTE: Converting to dataframe changes all values to factor, so here I reformat the data and save it
dat48$scnid<-as.character(dat48$scnid)
dat48$scndate<-as.Date(dat48$scndate,"%Y-%m-%d")
dat48$plot<-as.character(dat48$plot)
dat48$UTM.lat<- as.numeric(as.character(dat48$UTM.lat))
dat48$UTM.lon<- as.numeric(as.character(dat48$UTM.lon))
dat48$biomass<- as.numeric(as.character(dat48$biomass))
dat48$HH.sigma.48<- as.numeric(as.character(dat48$HH.sigma.48))
dat48$HV.sigma.48<- as.numeric(as.character(dat48$HV.sigma.48))
dat48$year<-as.numeric(format(dat48$scndate,"%Y"))
dat48$month<-as.numeric(format(dat48$scndate,"%m"))
dat48$HHse_data_48m<- as.numeric(as.character(dat48$HHse_data_48m))
dat48$HVse_data_48m<- as.numeric(as.character(dat48$HVse_data_48m))

#This will exclude scenes from the leaf off period (Nov-April)
if(leaf.off==1){ #include leaf off data
  dat48<-dat48
}else{ #exclude leaf off data
  dat48<-dat48[as.numeric(format(dat48$scndate,"%m"))>=05 & as.numeric(format(dat48$scndate,"%m"))<=10,]
}

#This will exclude plots which returned zeros for both HH and HV
#NOTE: this circumstance corresponds to a subset of the plots falling outside of the palsar scene
#      This can be verified by examining WLEF_SceneExtent_with_plot_overlay.pdf in data.remote/output/data
dat48<-dat48[dat48$HH.sigma.48 !=0 & dat48$HV.sigma.48 !=0,]


#Generate column of DOYs
dats<-as.character(dat48$scndate) #reformat as character
data.doy.07<-chron(dates = as.character(dat48$scndate[grep("2007",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2007))
data.doy.07<-as.numeric(data.doy.07)
data.year.07<-substr(dat48$scndate[grep("2007",dates)],1,4)
data.dates.07<-cbind(data.year.07,data.doy.07)

data.doy.08<-chron(dates = as.character(dat48$scndate[grep("2008",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2008))
data.doy.08<-as.numeric(data.doy.08)
data.year.08<-substr(dat48$scndate[grep("2008",dates)],1,4)
data.dates.08<-cbind(data.year.08,data.doy.08)

data.doy.09<-chron(dates = as.character(dat48$scndate[grep("2009",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2009))
data.doy.09<-as.numeric(data.doy.09)
data.year.09<-substr(dat48$scndate[grep("2009",dates)],1,4)
data.dates.09<-cbind(data.year.09,data.doy.09)

data.doy.10<-chron(dates = as.character(dat48$scndate[grep("2010",dats)]), format = (dates = "Y-m-d"), origin = c(day = 1, month = 0, year = 2010))
data.doy.10<-as.numeric(data.doy.10)
data.year.10<-substr(dat48$scndate[grep("2010",dates)],1,4)
data.dates.10<-cbind(data.year.10,data.doy.10)

dat48$doy<-c(data.dates.07,data.dates.08,data.dates.09,data.dates.10)

#Save extracted data
write.table(dat48,file=paste(outpath,"/",coord.set[fia+1],"_dat48.csv",sep=""),sep=",",quote=FALSE,col.names = TRUE, row.names=F)

#Switch to working from saved data 
dat48<-read.csv(paste(outpath,"/",coord.set[fia+1],"_dat48.csv",sep=""),header = TRUE)

#Correctly format data (again...sigh...)
dat48$scnid<-as.character(dat48$scnid)
dat48$scndate<-as.Date(dat48$scndate,"%Y-%m-%d")
dat48$plot<-as.numeric(dat48$plot)
dat48$UTM.lat<- as.numeric(as.character(dat48$UTM.lat))
dat48$UTM.lon<- as.numeric(as.character(dat48$UTM.lon))
dat48$biomass<- as.numeric(as.character(dat48$biomass))
dat48$HH.sigma.48<- as.numeric(as.character(dat48$HH.sigma.48))
dat48$HV.sigma.48<- as.numeric(as.character(dat48$HV.sigma.48))
dat48$year<-as.numeric(format(dat48$scndate,"%Y"))
dat48$month<-as.numeric(format(dat48$scndate,"%m"))
dat48$HHse_data_48m<- as.numeric(as.character(dat48$HHse_data_48m))
dat48$HVse_data_48m<- as.numeric(as.character(dat48$HVse_data_48m))

#This generates a figure showing the HH values for 2 WLEF plots which are anomalously high
#NOTE: These points are removed below
if(fia==0){
  plot(dat48$biomass, dat48$HH.sigma.48,xlab="Biomass",ylab="HH (sigma)",main="Anomalous Plots")
  points(dat48$biomass[dat48$plot=="W47"],dat48$HH.sigma.48[dat48$plot=="W47"],pch=19,col="red")
  points(dat48$biomass[dat48$plot=="W52"],dat48$HH.sigma.48[dat48$plot=="W52"],pch=19,col="blue")
  legend("topright",legend=c("W47","W52"),pch=19,col=c("red","blue"),bty="n")
  
  dat48<-dat48[dat48$plot !="W47" & dat48$plot !="W52",]
}

  
#Generate PDF of raw data exploration
#NOTE: Some of these figures will not be relevant for the FIA dataset
pdf(paste(outpath,"/",coord.set[fia+1], "_ExtractionQCplots.pdf",sep=""),width = 6, height = 6, paper='special')

#Plot HH values for each year-month combo
par(mfrow=c(3,6))
for(y in unique(dat48$year)){
  for(m in unique(dat48$month)){
    if(length(dat48$biomass[dat48$month==m  & dat48$year==y])<1){ #Skips Year-month combos with no data
      next
    }else{ 
      plot(dat48$biomass[dat48$month==m  & dat48$year==y],dat48$HH.sigma.48[dat48$month==m & dat48$year==y],
           xlim=c(min(dat48$biomass),max(dat48$biomass)),ylim=c(min(dat48$HH.sigma.48),max(dat48$HH.sigma.48)),
           xlab="biomass",ylab='HH',main=paste(month.abb[m],y,sep=" ") )
    }#if
  }#for m
}#for y

#Plot HV values for each year-month combo
par(mfrow=c(3,6))
for(y in unique(dat48$year)){
  for(m in unique(dat48$month)){
    if(length(dat48$biomass[dat48$month==m  & dat48$year==y])<1){ #Skips Year-month combos with no data
      next
    }else{ 
      plot(dat48$biomass[dat48$month==m  & dat48$year==y],dat48$HV.sigma.48[dat48$month==m & dat48$year==y],
           xlim=c(min(dat48$biomass),max(dat48$biomass)),ylim=c(min(dat48$HV.sigma.48),max(dat48$HV.sigma.48)),
           xlab="biomass",ylab='HV',main=paste(month.abb[m],y,sep=" ") )
    }#if
  }#for m
}#for y

#Plot boxplots of each scndate by year (HH)
par(mfrow=c(2,2))
boxplot(dat48$HH.sigma.48[dat48$year==2007] ~ dat48$month[dat48$year==2007],xlab="month",main="2007 HH")
boxplot(dat48$HH.sigma.48[dat48$year==2008] ~ dat48$month[dat48$year==2008],xlab="month",main="2008 HH")
boxplot(dat48$HH.sigma.48[dat48$year==2009] ~ dat48$month[dat48$year==2009],xlab="month",main="2009 HH")
boxplot(dat48$HH.sigma.48[dat48$year==2010] ~ dat48$month[dat48$year==2010],xlab="month",main="2010 HH")

#Plot boxplots of each scndate by year (HV)
par(mfrow=c(2,2))
boxplot(dat48$HV.sigma.48[dat48$year==2007] ~ dat48$month[dat48$year==2007],xlab="month",main="2007 HV")
boxplot(dat48$HV.sigma.48[dat48$year==2008] ~ dat48$month[dat48$year==2008],xlab="month",main="2008 HV")
boxplot(dat48$HV.sigma.48[dat48$year==2009] ~ dat48$month[dat48$year==2009],xlab="month",main="2009 HV")
boxplot(dat48$HV.sigma.48[dat48$year==2010] ~ dat48$month[dat48$year==2010],xlab="month",main="2010 HV")

#Plot comparing HH values of May, June, August 2007
par(mfrow=c(1,3))
plot(dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HH.sigma.48[dat48$month==6 & dat48$year==2007],
     xlab="05 HH",ylab='06 HH',main="may 2007 vs jun 2007")
    fit1<-lm(dat48$HH.sigma.48[dat48$month==6 & dat48$year==2007] ~ dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007])
    abline(0,1,lwd=2,lty=2,col="grey")
    abline(fit1,lwd=2,lty=1,col="red")
plot(dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007],
     xlab="05 HH",ylab='08 HH',main="may 2007 vs aug 2007")
    fit2<-lm(dat48$HH.sigma.48[dat48$month==5  & dat48$year==2007] ~ dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007])
    abline(0,1,lwd=2,lty=2,col="grey")
    abline(fit1,lwd=2,lty=1,col="red")
plot(dat48$HH.sigma.48[dat48$month==6  & dat48$year==2007],dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007],
     xlab="06 HH",ylab='08 HH',main="jun 2007 vs aug 2007")
    fit3<-lm(dat48$HH.sigma.48[dat48$month==6  & dat48$year==2007] ~ dat48$HH.sigma.48[dat48$month==8 & dat48$year==2007])
    abline(0,1,lwd=2,lty=2,col="grey")
    abline(fit1,lwd=2,lty=1,col="red")

#Plot comparing HV values of May, June, August 2007
par(mfrow=c(1,3))
plot(dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HV.sigma.48[dat48$month==6 & dat48$year==2007],
     xlab="05 HV",ylab='06 HV',main="may 2007 vs jun 2007")
fit1<-lm(dat48$HV.sigma.48[dat48$month==6 & dat48$year==2007] ~ dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007])
abline(0,1,lwd=2,lty=2,col="grey")
abline(fit1,lwd=2,lty=1,col="red")
plot(dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007],dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007],
     xlab="05 HV",ylab='08 HV',main="may 2007 vs aug 2007")
fit2<-lm(dat48$HV.sigma.48[dat48$month==5  & dat48$year==2007] ~ dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007])
abline(0,1,lwd=2,lty=2,col="grey")
abline(fit1,lwd=2,lty=1,col="red")
plot(dat48$HV.sigma.48[dat48$month==6  & dat48$year==2007],dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007],
     xlab="06 HV",ylab='08 HV',main="jun 2007 vs aug 2007")
fit3<-lm(dat48$HV.sigma.48[dat48$month==6  & dat48$year==2007] ~ dat48$HV.sigma.48[dat48$month==8 & dat48$year==2007])
abline(0,1,lwd=2,lty=2,col="grey")
abline(fit1,lwd=2,lty=1,col="red")

#Plot scene frequency by year, month
par(mfrow=c(1,2))
hist(dat48$year,freq=TRUE,main="By year")
hist(dat48$month,freq=TRUE,main="By month")

# par(mfrow=c(1,1))
# hist(dat48$scndate,freq=T,100,xaxt="n")
# axis(1, dat48$scndate, format(dat48$scndate, "%b %Y"), cex.axis = .7)

par(mfrow=c(1,3))
hist(dat48$biomass,main=paste(coord.set[fia+1],"biomass",sep=" "))
hist(dat48$HH.sigma.48,main=paste(coord.set[fia+1],"HH",sep=" "))
hist(dat48$HV.sigma.48,main=paste(coord.set[fia+1],"HV",sep=" "))

Lab.palette <- colorRampPalette(c("white","violet","blue","green","yellow","orange", "red"), space = "Lab")
par(mfrow=c(1,3))
smoothScatter(dat48$HV.sigma.48,dat48$HH.sigma.48,nbin=256,colramp = Lab.palette,xlab="HV",ylab="HH")
smoothScatter(dat48$biomass,dat48$HH.sigma.48,nbin=256,colramp = Lab.palette,xlab="biomass",ylab="HH",main="Density")
smoothScatter(dat48$biomass,dat48$HV.sigma.48,nbin=256,colramp = Lab.palette,ylim=c(0,max(dat48$HH.sigma.48)),xlab="biomass",ylab="HV")

par(mfrow=c(1,2))
scatter.smooth(dat48$biomass,dat48$HH.sigma.48,cex=0,xlab="biomass",ylab="HH",main="48m",col="grey")
  points(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2007],col=1,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2008],col=2,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2009],col=3,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col=4,cex=0.5)
  legend("topright",pch=1,legend=c(2007,2008,2009,2010), cex=0.7,pt.cex=0.5,col=1:4,bty="n",xjust=1)
scatter.smooth(dat48$biomass,dat48$HV.sigma.48,cex=0,xlab="biomass",ylab="HV",main="48m",col="grey")
  points(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2007],col=1,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2008],col=2,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2009],col=3,cex=0.5)
  points(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010],col=4,cex=0.5)
  legend("topright",pch=1,legend=c(2007,2008,2009,2010), cex=0.7,pt.cex=0.5,col=1:4,bty="n",xjust=1)
# scatter.smooth(dat60$biomass,dat60$HV.sigma.60,xlab="biomass",ylab="HV",main="60m",col="grey")
# scatter.smooth(dat60$biomass,dat60$HV.sigma.60,xlab="biomass",ylab="HV",main="60m",col="grey")

par(mfrow=c(1,2))
scatter.smooth(dat48$biomass,dat48$HV.sigma.48/dat48$HH.sigma.48,xlab="biomass",ylab="HV/HH",main="48m",col="grey")
# scatter.smooth(dat60$biomass,dat60$HV.sigma.60/dat60$HV.sigma.60,xlab="biomass",ylab="HV/HV",main="60m",col="grey")
scatter.smooth(dat48$biomass,dat48$HH.sigma.48*dat48$HV.sigma.48,xlab="biomass",ylab="HHxHV",main="48m",col="grey")
# scatter.smooth(dat60$biomass,dat60$HV.sigma.60*dat60$HV.sigma.60,xlab="biomass",ylab="HVxHV",main="60m",col="grey")

par(mfrow=c(1,1))
scatter.smooth(dat48$biomass,(dat48$HH.sigma.48-dat48$HV.sigma.48)/(dat48$HH.sigma.48+dat48$HV.sigma.48),xlab="biomass",ylab="(HH-HV)/(HH+HV)",main="48m", col="gray")
# scatter.smooth(dat60$biomass,(dat60$HV.sigma.60-dat60$HV.sigma.60)/(dat60$HV.sigma.60+dat60$HV.sigma.60),xlab="biomass",ylab="(HV-HV)/(HV+HV)",main="60m", col="gray")

par(mfrow=c(4,2),mar=c(4,4,2,2))
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2007],col="grey",xlab="biomass",ylab="HH",main="2007")
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2007],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2007],col="grey",xlab="biomass",ylab="HV",main="2007")
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2008],col="grey",xlab="biomass",ylab="HH",main="2008")
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2008],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2008],col="grey",xlab="biomass",ylab="HV",main="2008")
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2009],col="grey",xlab="biomass",ylab="HH",main="2009")
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2009],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2009],col="grey",xlab="biomass",ylab="HV",main="2009")
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HH",main="2010")
  points(dat48$biomass[format(dat48$scndate,"%m")>10],dat48$HH.sigma.48[format(dat48$scndate,"%m")>10],col="red",xlab="biomass",ylab="HV",main="2010")
  legend("topright",pch=1,legend=c("!Dec","Dec"), cex=0.7,pt.cex=0.5,col=c("grey","red"),bty="n",xjust=1)
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HV",main="2010")
  points(dat48$biomass[format(dat48$scndate,"%m")>10],dat48$HV.sigma.48[format(dat48$scndate,"%m")>10],col="red",xlab="biomass",ylab="HV",main="2010")
  legend("topright",pch=1,legend=c("!Dec","Dec"), cex=0.7,pt.cex=0.5,col=c("grey","red"),bty="n",xjust=1)

par(mfrow=c(1,2))
plot(dat48$scndate,dat48$HH.sigma.48,ylim=c(0,max(dat48$HH.sigma.48)),xlab="Date",ylab="HH")
plot(dat48$scndate,dat48$HV.sigma.48,ylim=c(0,max(dat48$HH.sigma.48)),xlab="Date",ylab="HV")
mtext("On same scale", side=3, line=-2, outer=TRUE, cex=1, font=2)

par(mfrow=c(1,2))
plot(dat48$scndate,dat48$HH.sigma.48,ylim=c(0,max(dat48$HH.sigma.48)),xlab="Date",ylab="HH")
plot(dat48$scndate,dat48$HV.sigma.48,ylim=c(0,max(dat48$HV.sigma.48)),xlab="Date",ylab="HV")
mtext("By Date", side=3, line=-2, outer=TRUE, cex=1, font=2)

par(mfrow=c(1,2))
plot(dat48$scndate[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],xlab="2010",ylab="HH")
plot(dat48$scndate[format(dat48$scndate,"%Y")==2010],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010],xlab="2010",ylab="HV")
mtext("2010 only", side=3, line=-3, outer=TRUE, cex=1, font=2)

#Plots demonstrating the effects of including DEC palsar scndate
if(leaf.off==1){ #Only plots if leaf off period is included
par(mfrow=c(2,2))
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HH",main="2010 only")
points(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10])
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010],col="grey",xlab="biomass",ylab="HH",main="2010 only")
points(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")>10])
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],dat48$HH.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],col="grey",xlab="biomass",ylab="HH",main="2010 only,Dec. removed")
scatter.smooth(dat48$biomass[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],dat48$HV.sigma.48[format(dat48$scndate,"%Y")==2010 & format(dat48$scndate,"%m")<11],col="grey",xlab="biomass",ylab="HV",main="2010 only,Dec. removed")
}

# #Plot individual time series of HH for each coordinate set
# par(new=FALSE, mfrow=c(1,1))
# plot(format(dat48$scndate,"%Y"),dat48$HH.sigma.48,col="grey",cex=0.5,
#      xlab="Year",ylab="HH",main="Average backscatter/plot/year")
# date.plot.HHmean<-tapply(dat48$HH.sigma.48,list(dat48$plot,format(dat48$scndate,"%Y")),mean,na.rm=TRUE) #mean HH for each plot in each year
# for(i in dat48$plot){
#   lines(cbind(c(2007,2008,2009,2010),date.plot.HHmean[dat48$plot[i],]),col=i)
#   par(new=T)
# }
# #Plot individual time series of HV for each coordinate set
# par(new=FALSE, mfrow=c(1,1))
# plot(format(dat48$scndate,"%Y"),dat48$HV.sigma.48,col="grey",cex=0.5,
#      xlab="Year",ylab="HV",main="Average backscatter/plot/year")
# date.plot.HVmean<-tapply(dat48$HV.sigma.48,list(dat48$plot,format(dat48$scndate,"%Y")),mean,na.rm=TRUE) #mean HV for each plot in each year
# date.plot.HVmean<-na.exclude(date.plot.HVmean)
# for(i in dat48$plot){
#   lines(cbind(c(2007,2008,2009,2010),date.plot.HVmean[i,]),col=i)
#   par(new=T)
# }

#breaks data into quantiles each containing ~5% of the data
bind.bio<-tapply(dat48$biomass,cut(dat48$biomass,breaks=round(quantile(dat48$biomass,probs = seq(0, 1, 0.05))) ),mean)
bind.HH<-tapply(dat48$HH.sigma.48,cut(dat48$HH.sigma.48,breaks=quantile(dat48$HH.sigma.48,probs = seq(0, 1, 0.05)) ),mean)
bind.HV<-tapply(dat48$HV.sigma.48,cut(dat48$HV.sigma.48,breaks=quantile(dat48$HV.sigma.48,probs = seq(0, 1, 0.05)) ),mean)
par(new=FALSE, mfrow=c(1,2))
plot(dat48$biomass,dat48$HH.sigma.48,col="grey",pch=".",xlab="Binned Biomass",ylab="Binned HH")
  points(bind.bio,bind.HH)
plot(dat48$biomass,dat48$HV.sigma.48,col="grey",,pch=".",xlab="Binned Biomass",ylab="Binned HV")
  points(bind.bio,bind.HV)
mtext("Bins each contain 5% of the data points", side=3, line=-3, outer=TRUE, cex=1, font=2)

#breaks data into even-length bins
bind.bio<-tapply(dat48$biomass,   cut(dat48$biomass,    breaks=seq(0, max(dat48$biomass),     0.05*max(dat48$biomass))),mean)
bind.HH<-tapply(dat48$HH.sigma.48,cut(dat48$HH.sigma.48,breaks=seq(0, max(dat48$HH.sigma.48), 0.05*max(dat48$HH.sigma.48))),mean)
bind.HV<-tapply(dat48$HV.sigma.48,cut(dat48$HV.sigma.48,breaks=seq(0, max(dat48$HV.sigma.48), 0.05*max(dat48$HV.sigma.48))),mean)
par(mfrow=c(1,2))
plot(dat48$biomass,dat48$HH.sigma.48,col="grey",pch=".",xlab="Binned Biomass",ylab="Binned HH")
  points(bind.bio,bind.HH)
plot(dat48$biomass,dat48$HV.sigma.48,col="grey",,pch=".",xlab="Binned Biomass",ylab="Binned HV")
  points(bind.bio,bind.HV)
mtext("Bins each contain 5% of data range", side=3, line=-3, outer=TRUE, cex=1, font=2)

par(mfrow=c(1,2))
bplot.xy(dat48$biomass,dat48$HH.sigma.48,N=15,xlab="biomass",ylab="HH (simga naught)")
bplot.xy(dat48$biomass,dat48$HV.sigma.48,N=15,xlab="biomass",ylab="HV (simga naught)")

dev.off()

#Run curve fitting function
n.reps<- 1000 #sets value for n.adapt and n.iter
n.chain<-3 #number of MCMC chains to run
bayes.curve.fit(outpath,coord.set,fia,n.reps,n.chain)






#########################################################
## HERE THERE BE DRAGONS! (old/obselete/deprecated)
#########################################################



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
# #######################
# ##NOTE: backscatter=((alpha*beta*biomass)/(beta + alpha*biomass))
# buff<-c("48", "60")
# 
# # col.names<-c("pol_band", 
# #              "buffer_radius(m)",
# #              "biomass_R2", 
# #              "mod.alpha", 
# #              "pval.alpha", 
# #              "alpha.ci.lower", 
# #              "alpha.ci.upper",
# #              "mod.beta", 
# #              "pval.b", 
# #              "beta.ci.upper",
# #              "beta.ci.upper",
# #              "num.iters", 
# #              "convergence")
# # mod.params<-matrix(nrow=1,ncol=length(col.names))
# # colnames(mod.params)<-col.names
# # 
# # par(mfrow=c(length(pol_bands),length(buff)))
# # for(i in 1:length(pol_bands)){
# #   for(j in 1:length(buff)){
# #     
# #     y<-eval(parse(text=paste(paste("dat",buff[j],sep=''),paste("$",sep=''),paste(pol_bands[i],'.sigma.',buff[j],sep=''))))
# #     x<-(eval(parse(text=paste(paste("dat",buff[j],sep=''),paste("$biomass",sep='')))))^0.5
# #     
# #     # Plot backscatter v biomass
# #     plot(x, y,
# #          xlab=expression(sqrt(biomass)),
# #          ylab=pol_bands[i],
# #          main=buff[j],
# #          col=51,pch=19, cex=0.6,
# #          xlim=c(min(x),max(x)), 
# #          ylim=c(min(y),max(y)),
# #          las=1, cex.axis=1.2)
# #     
# #     # Calculate rectangular hyperbola between backscatter and biomass
# #     biomass_curve <- nls(formula=y ~ ((alpha*beta*x)/(beta + alpha*x)), # also in Gu 2002
# #                          data=list(y = y, x = x), 
# #                          start=list(alpha = .75, beta = mean(y[x > quantile(x)[4]])), 
# #                          na.action="na.exclude", trace=F)
# #     biomass_R2 <- 1 - var(residuals(biomass_curve)) / var(y)  # R2
# #     
# #     # Plot rectangular hyperbola model fit
# #     mod.alpha <- summary(biomass_curve)$parameters[1]  	# alpha value
# #     mod.beta  <- summary(biomass_curve)$parameters[2]		# Beta value
# #     mod.biomass <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), by=0.01)	
# #     mod.HH <- (mod.alpha*mod.beta*mod.biomass)/(mod.beta + mod.alpha*mod.biomass) 
# #     lines(x=mod.biomass, y=mod.HH, col="black", lty=1, lwd=2.5)
# #     
# #     legend("topright", legend=c(paste("R^2=", format(biomass_R2, digits=2)),
# #                                 paste("alpha=",format(mod.alpha,digits=2)),
# #                                 paste("beta=",format(mod.beta,digits=2))), bty="n",cex=1.2)
# #     
# #     # Write model parameters to output file
# #     num.iters  <- as.numeric(biomass_curve$convInfo[2])
# #     conv 	     <- as.numeric(biomass_curve$convInfo[1])
# #     pval.a	   <- as.numeric(summary(biomass_curve)$parameters[7])
# #     #     if(i==1 & j==2){alpha.ci   <- as.vector(confint(lgt_curve,summary(lgt_curve)$parameters, level = 0.95))
# #     #                         alpha.ci.lower   <- as.numeric(alpha.ci[1])
# #     #                         alpha.ci.upper   <- as.numeric(alpha.ci[2])
# #     #     }else if(i==2 & j==1){alpha.ci   <- as.vector(confint(lgt_curve,summary(lgt_curve)$parameters[1,], level = 0.95))
# #     #                               alpha.ci.lower   <- as.numeric(alpha.ci[1])
# #     #                               alpha.ci.upper   <- as.numeric(alpha.ci[2])
# #     #     }else
# #     ci               <- as.matrix(confint(biomass_curve))
# #     alpha.ci.lower   <- as.numeric(ci[1,1])
# #     alpha.ci.upper   <- as.numeric(ci[1,2])
# #     beta.ci.lower    <- as.numeric(ci[2,1])
# #     beta.ci.upper    <- as.numeric(ci[2,2])
# #     pval.b	   <- as.numeric(summary(biomass_curve)$parameters[8])
# #     mod.params <- rbind(mod.params,as.vector(c(pol_bands[i], 
# #                                                buff[j], 
# #                                                biomass_R2, 
# #                                                mod.alpha, 
# #                                                pval.a, 
# #                                                alpha.ci.lower, 
# #                                                alpha.ci.upper,
# #                                                mod.beta, 
# #                                                pval.b,
# #                                                beta.ci.lower, 
# #                                                beta.ci.upper,
# #                                                num.iters, 
# #                                                conv)))
# #     print(paste(pol_bands[i],buff[j]))
# #   }}
# # mod.params<-mod.params[2:nrow(mod.params),]
# # 
# # xs<-seq(from=1, to=4,by=1)
# # ys<-as.numeric(mod.params[,4])
# # upper<-as.numeric(mod.params[,7])
# # lower<-as.numeric(mod.params[,6])
# # par(mfrow=c(1,1))
# # plot(xs,ys,ylim=c(0,max(upper)+0.1*max(upper)),ylab="Alpha estimate",xlab="pol.band/buffer.radius")
# # segments(xs,lower,xs,upper,col="black",lwd=2)
# # legend("topright",legend=c("1=48,HH", "2=60,HH","3=48,HV", "4=60,HV"))
# # 
# # rhyp<-mod.params
# 
# 
# #######################
# ##Use Maximum likelihood to fit curves
# #######################
# data<- read.csv(file.path(outpath, "WLEF_dat48.csv"), sep=",", header=T) ##location of PALSAR metadata table
# 
# # model<-c("Holl4", "RecHyp", "Logistic")
# 
# 
# # for(k in 1:length(model)){ #loop over different functional forms
#   for(i in 1:length(pol_bands)){
#     for(j in 1 ){
#       
#       y<-eval(parse(text=paste(paste("data$",pol_bands[i],'.sigma.',buff[j],sep='')))) #backscatter values
#       #     x<-(eval(parse(text=paste(paste("dat",buff[j],sep=''),paste("$biomass",sep='')))))^0.5
#       x<-(data$biomass) #biomass
#       #     max.y<-mean(y[x>=quantile(x)[4]]) #starting est. of max backscatter is taken as the mean backscatter value when biomass > the 75th quantile
#      
# #######################
# ##Use Maximum likelihood to fit Holling Type 4
# #######################
#       model<-"Holl4"
#       param_est<-matrix(nrow=0,ncol=8) 
#       par(mfrow=c(1,length(pol_bands)))
#       pdf(paste(outpath,"/",model,"_curvefits.pdf",sep=""),width = 6, height = 6, paper='special')
#       
#       a<- mean(y[x>=quantile(x,na.rm=TRUE)[4]],na.rm=TRUE) #starting est. of max backscatter is taken as the mean backscatter value when biomass > the 75th quantile
#       b<-quantile(x,na.rm=TRUE)[4]
#       c<--1
#       sd<-sd(y,na.rm=TRUE) #stdev of backscatter values 
#       params<-c(a,b,c,sd)
#       
#       fit <- function(params,x,y){
#         a <-params[1]
#         b <-params[2]
#         c <-params[3]
#         sd<-params[4]
#         #       y.pred<-(max.y*x)/(ki+x)
#         y.pred<-(a*x^2)/(b+(c*x)+x^2)
#         
#         LL<- -sum(dnorm(y,y.pred,sd,log=TRUE))
#         return(LL)
#       } #function
#       
#       fit.mod = optim(par=params,fit,x=x,y=y)
#       fit.mod
#       aic.mod<- -2*fit.mod$value + 2*length(params)
#       
#       params <- c(pol_bands[i],buff[j],fit.mod$par[1:3],2*fit.mod$par[2]/fit.mod$par[3],fit.mod$par[4],aic.mod) #par means parameter estimates
#       param_est<-rbind(param_est, params)
#       xseq = seq(0,max(x),length=1000)
#       
#       plot(x,y, xlab="biomass",ylab=paste(pol_bands[i],buff[j],sep="_"),main=model, #something wrong here with main title
#            xlim=c(min(x),max(x)), 
#            ylim=c(min(y),max(y)),
#            pch=16,col="#CCCCCC")
#       abline(a=0,b=0)
#       #     lines(cbind(xseq,(as.numeric(params[3])*xseq)/(as.numeric(params[4])+xseq)),lwd=3) #closed circles and solid line for CTRL
#       lines(cbind(xseq,(fit.mod$par[1]*xseq^2)/(fit.mod$par[2]+fit.mod$par[3]*xseq+xseq^2)),lwd=3,col="green")
#       legend('topright', legend=c(paste("a=", format(fit.mod$par[1], digits=2)),
#                                   paste("b=",format(fit.mod$par[2],digits=2)),
#                                   paste("c=",format(fit.mod$par[3],digits=2)),
#                                   paste("aic=", format(aic.mod, digits=4))), bty="n",cex=0.85)
# 
#       dev.off()
#       colnames(param_est)<-c("pol_bands","buff","a","b","c","bio.at.peak.backscatter", "sd","AIC")
#       param_est
#       write.table(param_est,file=paste(outpath, "/", model, "_param_estimates.csv",sep=""),quote=FALSE,sep=",",row.names=F)
#       
# #######################
# ##Use Maximum likelihood to fit Logistic
# #######################
#       model<-"Logistic"
#       param_est<-matrix(nrow=0,ncol=7) 
#       par(mfrow=c(1,length(pol_bands)))
#       pdf(paste(outpath,"/",model,"_curvefits.pdf",sep=""),width = 6, height = 6, paper='special')
#       
#       a<- max(y) #starting est. of max backscatter is taken as the mean backscatter value when biomass > the 75th quantile
#       b<-2 #slope of initial portion of the curve
#       c<-mean(x[y>=quantile(y,0.9,na.rm=TRUE)],na.rm=TRUE)
#       sd<-sd(y,na.rm=TRUE) #stdev of backscatter values 
#       params<-c(a,b,c,sd)
#       
#       fit <- function(params,x,y){
#         a <-params[1]
#         b <-params[2]
#         c <-params[3]
#         sd<-params[4]
#         y.pred<- a/(1+b*exp(-c*x))
#         
#         LL<- -sum(dnorm(y,y.pred,sd,log=TRUE))
#         return(LL)
#       } #function
#       
#       fit.mod = optim(par=params,fit,x=x,y=y)
#       fit.mod
#       aic.mod<- -2*fit.mod$value + 2*length(params)
#       
#       params <- c(pol_bands[i],buff[j],fit.mod$par[1:4],aic.mod) #par means parameter estimates
#       param_est<-rbind(param_est, params)
#       xseq = seq(0,max(x),length=1000)
#       
#       plot(x,y, xlab="biomass",ylab=paste(pol_bands[i],buff[j],sep="_"),main=model, #something wrong here with main title
#            xlim=c(min(x),max(x)), 
#            ylim=c(min(y),max(y)),
#            pch=16,col="#CCCCCC")
#       abline(a=0,b=0)
#       lines(cbind(xseq,fit.mod$par[1]/(1+fit.mod$par[2]*exp(-fit.mod$par[3]*xseq))),lwd=3,col="green")
#       legend('topright', legend=c(paste("a=", format(fit.mod$par[1], digits=2)),
#                                   paste("b=",format(fit.mod$par[2],digits=2)),
#                                   paste("c=",format(fit.mod$par[3],digits=2)),
#                                   paste("aic=", format(aic.mod, digits=4))), bty="n",cex=0.85)
#       
#       dev.off()
#       colnames(param_est)<-c("pol_bands","buff","a","b","c","sd","AIC")
#       param_est
#       write.table(param_est,file=paste(outpath, "/", model, "_param_estimates.csv",sep=""),quote=FALSE,sep=",",row.names=F)
#       
#       
#       
#     }#for j looping over pol_bands
#   }#for i looping over buff
# 
# }#for k looping over models
# 
# ##################################
# ##################################
# 
# 
# #################
# ##diagnotics?
# #################
# 
# 
