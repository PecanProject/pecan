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
calib_inpath <-"/home/bhardima/git/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords"

calib_infile <-read.csv(file.path(calib_inpath,"wi-biomass-fuzzed.csv"), sep=",", header=T) #Wisconsin FIA plots

coords<-data.frame(calib_infile$FUZZED_LON,calib_infile$FUZZED_LAT) #lat and lon (ChEAS: UTM Zone 15N NAD83)

Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

wi.fia<-data.frame(calib_infile$FUZZED_LAT,calib_infile$FUZZED_LON)

FIA.points <- SpatialPointsDataFrame(Sr1, wi.fia) #convert to class="SpatialPointsDataFrame" for export as kml

# writeOGR(FIA.points, layer=1, "WI_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 

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

cheasFIA<-Sr1@coords[FIA.in.cheas==1,] 
spcheasFIA <- SpatialPoints(cheasFIA,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

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

# lake_extracted<-matrix(NA, nrow(lake_coords),length(pol_bands)*numfiles)
# disturbance_extracted_40m<-matrix(NA, nrow(disturbance_coords),length(pol_bands)*numfiles)
# 
# colnames(lake_extracted)<-col_names
# colnames(disturbance_extracted)<-col_names
# colnames(disturbance_extracted_40m)<-col_names

# extracted_48m<-matrix(NA, nrow=length(spcheasFIA)*numfiles, ncol=length(pol_bands)) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
# extracted_60m<-matrix(NA, nrow=length(spcheasFIA)*numfiles, ncol=length(pol_bands)) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted_48m<-matrix(NA, ncol=6) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands
extracted_60m<-matrix(NA, ncol=6) #df to store extracted palsar values. nrow=number of coordinates being extracted. ncol=# of pol_bands

# colnames(extracted_48m)<-pol_bands
# colnames(extracted_60m)<-pol_bands

for(i in 1:numfiles){
#   for(j in 1:length(pol_bands)){        

#     filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[j]), pattern=".tif" ,recursive=F))
#     inpath<-file.path(palsar_inpath,pol_bands[j],filelist[i])
#     rast<-raster(inpath)
        HH_filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[1]), pattern=".tif" ,recursive=F))
        HH_inpath<-file.path(palsar_inpath, pol_bands[1],HH_filelist[i])
        HH_rast<-raster(HH_inpath)
        
        HV_filelist<-as.vector(list.files(file.path(palsar_inpath, pol_bands[2]), pattern=".tif" ,recursive=F))
        HV_inpath<-file.path(palsar_inpath, pol_bands[2],HV_filelist[i])
        HV_rast<-raster(HV_inpath)
    
        ################################
        ##calibration data from FIA plots 48m BUFFER MEAN
        ################################
        HH_data_48m<-extract(HH_rast, spcheasFIA, method="simple",buffer=48, small=T, fun=mean)
        HV_data_48m<-extract(HV_rast, spcheasFIA, method="simple",buffer=48, small=T, fun=mean)
#           cols<-seq(j,ncol(extracted_48m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
#           extracted_48m[,cols[i]]<-data_48m
        cleaned_HH_data_48m<-as.vector(na.omit(HH_data_48m))
        cleaned_HV_data_48m<-as.vector(na.omit(HV_data_48m))
        
        filename<-matrix(substr(as.character(HV_filelist[i]),1,15),nrow=length(cleaned_HH_data_48m),ncol=1)
        colnames(filename)<-"scnid"
        palsar_date<-matrix(as.character(as.Date(substr(as.character(metadata$scndate[metadata$scnid==filename[1]]),1,8),"%Y%m%d")),nrow=length(cleaned_HH_data_48m),ncol=1)
        colnames(palsar_date)<-"scndate"
        extracted_coords<-spcheasFIA@coords[!is.na(HH_data_48m),]
                
        both_48<-cbind(filename,palsar_date,extracted_coords,cleaned_HH_data_48m,cleaned_HV_data_48m)
        extracted_48m<-rbind(extracted_48m,both_48)
        
        ###############################
        #calibration data from FIA plots 60m BUFFER MEAN
        ###############################
        HH_data_60m<-extract(HH_rast, spcheasFIA, method="simple",buffer=60, small=T, fun=mean)
        HV_data_60m<-extract(HV_rast, spcheasFIA, method="simple",buffer=60, small=T, fun=mean)
        #           cols<-seq(j,ncol(extracted_60m),by=2) #columns to be filled with palsar data (if j is odd=HH, if j is even=HV)
        #           extracted_60m[,cols[i]]<-data_60m
        cleaned_HH_data_60m<-as.vector(na.omit(HH_data_60m))
        cleaned_HV_data_60m<-as.vector(na.omit(HV_data_60m))
        
        filename<-matrix(substr(as.character(HV_filelist[i]),1,15),nrow=length(cleaned_HH_data_60m),ncol=1)
        colnames(filename)<-"scnid"
        palsar_date<-matrix(as.character(as.Date(substr(as.character(metadata$scndate[metadata$scnid==filename[1]]),1,8),"%Y%m%d")),nrow=length(cleaned_HH_data_60m),ncol=1)
        colnames(palsar_date)<-"scndate"
        extracted_coords<-spcheasFIA@coords[!is.na(HH_data_60m),]
        
        both_60<-cbind(filename,palsar_date,extracted_coords,cleaned_HH_data_60m,cleaned_HV_data_60m)
        extracted_60m<-rbind(extracted_60m,both_60)
      
    print(paste("i=",i,sep=""))
#     print(paste("j=",j,sep=""))
#   }
}
extracted_48m<-extracted_48m[2:dim(extracted_48m)[1],]
extracted_60m<-extracted_60m[2:dim(extracted_60m)[1],]




