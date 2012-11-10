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
##' @param out.dir OPTIONAL. Output directory for converted files
##'
##' @import maptools
##'
##' @export
##'
##' @author Shawn P. Serbin
##'
shp2kml <- function(dir,ext,kmz=TRUE,out.dir=NULL){
  
  if (! is.null(out.dir)){
    if (! file.exists(out.dir)) dir.create(out.dir,recursive=TRUE)
  }
  
  
  # Get list of shapefiles in directory
  files <- list.files(path=dir,pattern='*.shp',full.names = TRUE)
  remove <- grep('*xml',files)
  files <- files[-remove]
  
  print(files)
  
  # loop here
  for (i in files) {
    print("")
    print(paste('Converting : **',i,' ** to KML/KMZ file',sep=""))
    print("")
    print("")
    # Read in shapefile(s)
    shp.file <- readShapeSpatial(i,verbose=TRUE)
  }

  
  # read file here
  #readShapeSpatial()
  
  # Write out kml/kmz using plotKML package

  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################