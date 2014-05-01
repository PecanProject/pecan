palsar.extractor<-function(kml,fia,leaf.off,plot_ext){
  
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
  
  coord.set<-c("WLEF", "FIA")
  
  if(machine==2){ #Brady's Linux paths
    metadata<- read.csv("/home/bhardima/pecan/modules/data.remote/output/metadata/output_metadata.csv", sep="\t", header=T) ##for Brady's Linux
    palsar_inpath <- file.path("/home/bhardima/Desktop/cheas/geo_corrected_single_sigma") ##location of PALSAR raw files
    calib_inpath <-"/home/bhardima/pecan/modules/data.remote/biometry/Link_to_Forest_Biomass_Calibration_Coords" ##location of file containing (FIA) plot coords and biomass values for calibrating PALSAR backscatter 
    outpath <- file.path("/home/bhardima/pecan/modules/data.remote/output/data") ##For saving
  } 
  if(machine==1){ #Brady's Mac paths
    metadata<- read.csv("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/output/metadata/output_metadata.csv", sep="\t", header=T) ##location of PALSAR metadata table
    palsar_inpath <- file.path("/Users/hardimanb/Desktop/data.remote(Andys_Copy)/palsar_scenes/geo_corrected_single_sigma") ##location of PALSAR raw files
    calib_inpath <-"/Users/hardimanb/Desktop/data.remote(Andys_Copy)/biometry" ##location of file containing (FIA) plot coords and biomass values for calibrating PALSAR backscatter 
    outpath <- file.path("/Users/hardimanb/Dropbox/PALSAR_Biomass_Study/data") ##For saving  
  }
  
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
    calib_infile <-read.csv(file.path(calib_inpath,"biometry_trimmed.csv"), sep=",", header=T) #WLEF plots
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
  return(outpath,coord.set,dat48)
}#function
