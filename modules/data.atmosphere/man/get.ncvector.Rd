\name{get.ncvector}
\alias{get.ncvector}
\title{Get time series vector from netCDF file}
\usage{
  get.ncvector(var, lati = lati, loni = loni,
    run.dates = run.dates, met.nc)
}
\arguments{
  \item{var}{}

  \item{lati}{}

  \item{loni}{}

  \item{run.dates}{}

  \item{met.nc}{netcdf file with CF variable names}
}
\value{
  numeric vector
}
\description{
  Get time series vector from netCDF file
}
\details{
  internal convenience function for streamlining extraction
  of data from netCDF files with CF-compliant variable
  names
}
\author{
  David Shaner LeBauer
}

