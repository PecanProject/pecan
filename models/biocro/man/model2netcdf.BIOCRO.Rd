\name{model2netcdf.BIOCRO}
\alias{model2netcdf.BIOCRO}
\title{Function to convert biocro model output to standard netCDF format}
\usage{
  model2netcdf.BIOCRO(outdir)
}
\arguments{
  \item{outdir}{Location of biocro model output}

  \item{run.id}{Name of biocro model output file.}
}
\description{
  Convert BioCro output to netCDF
}
\details{
  Converts all output contained in a folder to netCDF.
  Modified from on model2netcdf.sipnet and model2netcdf.ED2
  by Shawn Serbin and Mike Dietze
}
\author{
  David LeBauer, Deepak Jaiswal
}

