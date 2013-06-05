\name{model2netcdf}
\alias{model2netcdf}
\title{Convert model output to NetCDF}
\usage{
  model2netcdf(runid, outdir, model)
}
\arguments{
  \item{runid}{}

  \item{outdir}{}

  \item{model}{name of simulation model currently accepts
  ("ED2", "SIPNET", "BIOCRO")}
}
\value{
  vector of filenames created, converts model output to
  netcdf as a side effect
}
\description{
  Convert output for a single model run to NetCDF
}
\details{
  This function is a wrapper for model-specific conversion
  functions, e.g. \code{model2netcdf.ED2},
  \code{model2netcdf.BIOCRO}.
}
\author{
  Mike Dietze, David LeBauer
}

