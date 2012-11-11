#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##'
##' Convert ESRI shapefile format to keyhole markup language (KML) file format
##' 
##' @title Convert shapefile to KML
##' 
##' @param dir Directory of GIS shapefiles to convert to kml/kmz
##' @param ext File extension for files to convert to kml/kmz.  Defaults to ESRI shapefile,
##' '.shp'.  [Place holder for other potential vector files to conver to kml]
##' @param kmz TRUE/FALSE. Option to write out file as a compressed kml
##' @param proj4 OPTIONAL. Define output proj4 projection string.  If set, input vector will be 
##' reprojected to desired projection.  Not yet implemented.
##' @param color OPTIONAL. Fill color for output kml/kmz file
##' @param NameField OPTIONAL. Define names for layers in KML file
##' @param out.dir OPTIONAL. Output directory for converted files
##'
##' @import rgdal
##' @import plotKML
##'
##' @export
##'
##' @author Shawn P. Serbin
##'
shp2kml <- function(dir,ext,kmz=TRUE,proj4=NULL,color=NULL,NameField=NULL,out.dir=NULL){
  
  # TODO: Allow assignment of output projection info by entering proj4 string
  # TODO: Allow for customization of output fill colors and line size
  # TODO: Allow for selection of taget attribute in output kml/kmz file(s)
  # TODO: Allow for setting out labels
  
  # Temp Hack
  #NameField="STATE"
  
  if (! is.null(out.dir)){
    if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
    output <- out.dir
  } else {
    output <- dir
  }
  
  # Get list of shapefiles in directory
  files <- list.files(path=dir,pattern='*.shp',full.names = FALSE)
  remove <- grep('*xml',files)
  if (length(remove)>0){
    files <- files[-remove]
  }
  
  # loop here
  for (i in files) {
    print("")
    print(paste('Converting : ** ',i,' ** to KML/KMZ file',sep=""))
    print("")
    print("")
    
    # Read in shapefile(s) & get coordinates/projection info
    #shp.file <- readShapeSpatial(paste(dir,i,sep="/"),verbose=TRUE)
    #coordinates(test) <- ~X+Y
    
    layers <- ogrListLayers(paste(dir,i,sep="/"))
    info <- ogrInfo(paste(dir,i,sep="/"),layers)
    #shp.file <- readOGR(paste(dir,i,sep="/"),layer=layers) # no need to read in file
    
    # Display vector info to the console
    print("")
    print(paste('Input layers: ',layers,sep=""))
    print(paste('Input projection info: ',info$p4s,sep=""))
    print("")
    
    # Write out kml/kmz using plotKML package
    #if (is.null(color)){
    #  color <- "grey70"
    #}
    
    if (kmz==TRUE){
       #kml(shp.file["STATE"],file=paste(output, "test.kmz"),colour = "grey70", alpha = 0.75, width=2,
       #    balloon=FALSE,kmz=TRUE)
      
    } else {
       #kml(shp.file,file=paste(output,"test.kml"),colour = "grey70", alpha = 0.75, width=2,
       #    balloon=FALSE)
      #writeOGR(shp.file["STATE"],'test2.kml',layer='statep010',NameField='STATE',driver="KML")
      
      # Required to get all fields in output kml/kmz file
      in.file <- file.path(dir,i,fsep = .Platform$file.sep)
      out.file <- file.path(output,"ogr2ogr.test.kml",fsep = .Platform$file.sep)
      
      OGRstring <- paste("ogr2ogr -progress -f KML"," ",
                         out.file, " ", in.file, " ", "-dsco NameField=",NameField, sep = "")
      system(OGRstring)
      
    }
    
  } # End of loop
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################