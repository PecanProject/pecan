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
##' Convert ESRI shapefile (*.shp) to keyhole markup language (KML) file format
##' 
##' @title Convert shapefile to KML
##' 
##' @param dir Directory of GIS shapefiles to convert to kml/kmz
##' @param ext File extension for files to convert to kml/kmz.  Defaults to ESRI shapefile,
##' '.shp'.  [Place holder for other potential vector files to conver to kml]
##' @param kmz TRUE/FALSE. Option to write out file as a compressed kml. Requires zip utility
##' @param proj4 OPTIONAL. Define output proj4 projection string.  If set, input vector will be 
##' reprojected to desired projection.  Not yet implemented.
##' @param color OPTIONAL. Fill color for output kml/kmz file
##' @param NameField OPTIONAL. Define names for individual features in KML/KMZ file
##' @param out.dir OPTIONAL. Output directory for converted files
##'
##' @import rgdal
##'
##' @export
##' 
##' @examples
##' \dontrun{
##' dir <- Sys.glob(file.path(R.home(), "library", "PEcAn.data.land","data"))
##' out.dir <- path.expand("~/temp")
##' shp2kml(dir,'.shp',kmz=FALSE,NameField="STATE",out.dir=out.dir)
##' system(paste("rm -r ",out.dir))
##'}
##' 
##' @author Shawn P. Serbin
##'
shp2kml <- function(dir,ext,kmz=FALSE,proj4=NULL,color=NULL,NameField=NULL,out.dir=NULL){
  
  # TODO: Enable compression of KML files using zip/gzip utility.  Not quite figured this out yet
  # TODO: Allow assignment of output projection info by entering proj4 string
  # TODO: Allow for customization of output fill colors and line size
  # TODO: Allow for selection of taget attribute in output kml/kmz file(s)
  # TODO: Allow for setting out labels
  
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
    #shp.file <- readShapeSpatial(file.path(dir,i),verbose=TRUE)
    #coordinates(test) <- ~X+Y
    
    layers <- ogrListLayers(file.path(dir,i))
    info <- ogrInfo(file.path(dir,i),layers)
    #shp.file <- readOGR(file.path(dir,i),layer=layers) # no need to read in file
    
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
      # NOT YET FULLY IMPLEMENTED
      in.file <- file.path(dir,i,fsep = .Platform$file.sep)
      out.file <- file.path(output,unlist(strsplit(i,'\\.')),fsep = .Platform$file.sep)
      OGRstring <- paste("ogr2ogr -progress -f KML"," ",
                         paste(out.file,".kmz",sep=""), " ", in.file, " ", 
                         "-dsco NameField=",NameField, sep = "")
      system(OGRstring) # Run KML conversion
      
      # ADD COMPRESSION STEP HERE!!!
      
    } else {
      #kml(shp.file,file=paste(output,"test.kml"),colour = "grey70", alpha = 0.75, width=2,
      #    balloon=FALSE)
      #writeOGR(shp.file["STATE"],'test2.kml',layer='statep010',NameField='STATE',driver="KML")
      
      # Using ogr2ogr external system utility.  Works much better than R packages.
      in.file <- file.path(dir,i,fsep = .Platform$file.sep)
      out.file <- file.path(output,unlist(strsplit(i,'\\.')),fsep = .Platform$file.sep)   
      OGRstring <- paste("ogr2ogr -progress -f KML"," ",
                         paste(out.file,".kml",sep=""), " ", in.file, " ", 
                         "-dsco NameField=",NameField,sep = "")
      system(OGRstring) # Run KML conversion
      
    }
    
  } # End of loop
} ### End of function
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' Function to extract attribute information from vector or raster data layer.
##' 
##' @title Retrieve attribute information from a vector or raster layer
##' @name get.attributes
##' 
##' @param file vector or raster layer
##' @param coords vector containin xmin,ymin,xmax,ymax defing the bounding box for subset
##' 
##' @import rgdal
##' @import fields
##' 
##' @export
##'
##' @examples
##' \dontrun{
##' file <- Sys.glob(file.path(R.home(), "library", "PEcAn.data.land","data","*.kml"))
##' out <- get.attributes(file=file,coords=c(-95,42,-84,47))
##' print(out)
##' }
##'
##' @author Shawn P. Serbin
##' 
get.attributes <- function(file,coords) {
  # ogr tools do not seem to function properly in R.  Need to figure out a work around
  # reading in kml files drops important fields inside the layers.
  
  #print("NOT IMPLEMENTED YET")
  #subset.layer(file,coords)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' Function to subset and clip a GIS vector or raster layer by a bounding box
##' or clip/subset layer (e.g. shapefile/KML)
##' 
##' @param file input file to be subset
##' @param coords vector with xmin,ymin,xmax,ymax defing the bounding box for subset
##' @param sub.layer Vector layer defining the subset region
##' @param clip clip geometries to bounding box/subset layer? TRUE/FALSE
##' @param out.dir output directory for subset layer. Defaults to location of
##' input file.  Can also set to "pwd"
##' @param out.name filename for subset layer.  Defaults to original filename with the suffix
##' *.sub
##' 
##' @import rgdal
##' @export
##' 
##' @examples
##' \dontrun{
##' # Test dataset
##' file <- Sys.glob(file.path(R.home(), "library", "PEcAn.data.land","data","*.shp"))
##' out.dir <- path.expand("~/temp")
##' # with clipping enabled
##' subset.layer(file=file,coords=c(-95,42,-84,47),clip=TRUE,out.dir=out.dir)
##' # without clipping enables
##' subset.layer(file=file,coords=c(-95,42,-84,47),out.dir=out.dir)
##' system(paste("rm -r",out.dir,sep=""))
##' }
##' 
##' @export
##' 
##' @author Shawn P. Serbin
##' 
subset.layer <- function(file,coords=NULL,sub.layer=NULL,clip=FALSE,out.dir=NULL,
                         out.name=NULL){
  
  # Setup output directory for subset layer
  if (is.null(out.dir)){
    out.dir <- dirname(file)
  } else if (out.dir=="pwd"){
    out.dir <- getwd()
  } else {
    out.dir <- out.dir
  }
  if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
  
  # Setup output file name for subset layer
  if (is.null(out.name)){
    out.name <- paste(unlist(strsplit(basename(file),'\\.'))[1],".sub.",
                      unlist(strsplit(basename(file),'\\.'))[2],sep="")
  } else {
    out.name <- out.name
  }
  
  print(paste("Subsetting layer: ",out.name,sep=""))
  output <- file.path(out.dir,out.name,fsep = .Platform$file.sep)
  
  if (unlist(strsplit(basename(file),'\\.'))[2]=="kml"){
    format <- "-f KML"
  } else {
    format <- paste("-f ","'ESRI Shapefile'",sep="")
  }
  
  if (clip){
    OGRstring <- paste("ogr2ogr -spat"," ",coords[1]," ",coords[2]," ",coords[3]," ",
                       coords[4]," ",format," ",output," ",file," ",
                       "-clipsrc"," ","spat_extent",sep="")
  } else {
    OGRstring <- paste("ogr2ogr -spat"," ",coords[1]," ",coords[2]," ",coords[3]," ",
                       coords[4]," ",format," ",output," ",file,sep="")
  }

  # Run subset command
  system(OGRstring)

}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
