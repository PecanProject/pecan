\name{shp2kml}
\alias{shp2kml}
\title{Convert shapefile to KML}
\usage{
shp2kml(dir, ext, kmz = FALSE, proj4 = NULL, color = NULL,
  NameField = NULL, out.dir = NULL)
}
\arguments{
  \item{dir}{Directory of GIS shapefiles to convert to
  kml/kmz}

  \item{ext}{File extension for files to convert to
  kml/kmz.  Defaults to ESRI shapefile, '.shp'.  [Place
  holder for other potential vector files to conver to
  kml]}

  \item{kmz}{TRUE/FALSE. Option to write out file as a
  compressed kml. Requires zip utility}

  \item{proj4}{OPTIONAL. Define output proj4 projection
  string.  If set, input vector will be reprojected to
  desired projection.  Not yet implemented.}

  \item{color}{OPTIONAL. Fill color for output kml/kmz
  file}

  \item{NameField}{OPTIONAL. Define names for individual
  features in KML/KMZ file}

  \item{out.dir}{OPTIONAL. Output directory for converted
  files}
}
\description{
Convert ESRI shapefile (*.shp) to keyhole markup language
(KML) file format
}
\examples{
\dontrun{
dir <- Sys.glob(file.path(R.home(), "library", "PEcAn.data.land","data"))
out.dir <- path.expand("~/temp")
shp2kml(dir,'.shp',kmz=FALSE,NameField="STATE",out.dir=out.dir)
system(paste("rm -r ",out.dir))
}
}
\author{
Shawn P. Serbin
}

