\name{get.attributes}
\alias{get.attributes}
\title{Retrieve attribute information from a vector or raster layer}
\usage{
get.attributes(file, coords)
}
\arguments{
  \item{file}{vector or raster layer}

  \item{coords}{vector containin xmin,ymin,xmax,ymax defing
  the bounding box for subset}
}
\description{
Function to extract attribute information from vector or
raster data layer.
}
\examples{
\dontrun{
file <- Sys.glob(file.path(R.home(), "library", "PEcAn.data.land","data","*.kml"))
out <- get.attributes(file=file,coords=c(-95,42,-84,47))
print(out)
}
}
\author{
Shawn P. Serbin
}

