\name{model2netcdf.BIOCRO}
\alias{model2netcdf.BIOCRO}
\title{Function to convert biocro model output to standard netCDF format}
\usage{
  model2netcdf.BIOCRO(outdir, sitelat, sitelon, start_date,
    end_date)
}
\arguments{
  \item{outdir}{Location of ED model output}

  \item{sitelat}{Latitude of the site}

  \item{sitelon}{Longitude of the site}

  \item{start_date}{Start time of the simulation}

  \item{end_date}{End time of the simulation}
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

