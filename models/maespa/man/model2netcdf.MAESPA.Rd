\name{model2netcdf.MAESPA}
\alias{model2netcdf.MAESPA}
\title{Code to convert MAESPA's output into netCDF format}
\usage{
model2netcdf.MAESPA(outdir, sitelat, sitelon, start_date, end_date)
}
\arguments{
  \item{outdir}{Location of MAESPA output}

  \item{sitelat}{Latitude of the site}

  \item{sitelon}{Longitude of the site}

  \item{start_date}{Start time of the simulation}

  \item{end_date}{End time of the simulation}
}
\description{
Convert MAESPA output into the NACP Intercomparison format
(ALMA using netCDF)
}
\author{
Tony Gardella
}

