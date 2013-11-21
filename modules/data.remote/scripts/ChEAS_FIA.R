##Author Brady S. Hardiman 11/12/2013

##Read in fuzzed FIA coordinates from Andy Finley (MSU)
require(sp)

calib_inpath <-"/home/bhardima/git/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords"

calib_infile <-read.csv(file.path(calib_inpath,"wi-biomass-fuzzed.csv"), sep=",", header=T) #Wisconsin FIA plots

coords<-data.frame(calib_infile$FUZZED_LON,calib_infile$FUZZED_LAT) #lat and lon (ChEAS: UTM Zone 15N NAD83)

Sr1<- SpatialPoints(coords,proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

wi.fia<-data.frame(calib_infile$FUZZED_LAT,calib_infile$FUZZED_LON)

FIA.points <- SpatialPointsDataFrame(Sr1, wi.fia) #convert to class="SpatialPointsDataFrame" for export as kml

# writeOGR(FIA.points, layer=1, "WI_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 

ChEAS_PLASAR_extent<-rbind(c(-88.446974,46.936532), 
c(-88.023077,45.343856),
c(-91.057452,45.032895),
c(-91.454983,46.725886),
c(-88.446974,46.936532)) #corner coords for palsar data in cheas domain. DEVELOP METHOD TO BUILD THIS AUTOMATICALLY--METADATA TABLE???

ChEAS_PLASAR_extent<- Polygon(ChEAS_PLASAR_extent) #spatial polygon from cheas-palsar extent
Srs1<- Polygons(list(ChEAS_PLASAR_extent),"ChEAS_PLASAR_extent") #spatial polygons (plural)
ChEAS_PLASAR_extent<-SpatialPolygons(list(Srs1),proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 

FIA.in.cheas<-as.vector(over(FIA.points,ChEAS_PLASAR_extent)) #subset of FIA plots that falls within Cheas-PALSAR extent

FIA.in.cheas[is.na(FIA.in.cheas)]<-0 #replace na's with 0's for indexing

cheasFIA<-FIA.points[FIA.in.cheas>0,]

# writeOGR(cheasFIA, layer=1, "cheas_FIA.kml", driver="KML") #export as kml (this puts in in the Home folder) 

## Use "over" function to ID palsar scenes that intersect with ChEAS bounding box




