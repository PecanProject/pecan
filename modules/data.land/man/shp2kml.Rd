\name{shp2kml}
\alias{shp2kml}
\title{Convert shapefile to KML}
\usage{
  shp2kml(dir, ext, kmz = TRUE, out.dir = NULL)
}
\arguments{
  \item{dir}{Directory of GIS shapefiles to convert to
  kml/kmz}

  \item{ext}{File extension for files to convert to
  kml/kmz.  Defaults to ESRI shapefile, '.shp'.  [Place
  holder for other potential vector files to conver to
  kml]}

  \item{kmz}{TRUE/FALSE. Option to write out file as a
  compressed kml}

  \item{out.dir}{OPTIONAL. Output directory for converted
  files}
}
\description{
  Convert ESRI shapefile format to keyhole markup language
  (KML) file format
}
\author{
  Shawn P. Serbin
}

