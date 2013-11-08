\name{qair2rh}
\alias{qair2rh}
\title{qair2rh}
\usage{
  qair2rh(qair, temp, press = 1013.25)
}
\arguments{
  \item{qair}{specific humidity, dimensionless (e.g. kg/kg)
  ratio of water mass / total air mass}

  \item{temp}{degrees C}

  \item{press}{pressure in mb}
}
\value{
  rh relative humidity, ratio of actual water mixing ratio
  to saturation mixing ratio
}
\description{
  Convert specific humidity to relative humidity
}
\details{
  converting specific humidity into relative humidity NCEP
  surface flux data does not have RH from Bolton 1980 Teh
  computation of Equivalent Potential Temperature
  \url{http://www.eol.ucar.edu/projects/ceop/dm/documents/refdata_report/eqns.html}
}
\author{
  David LeBauer
}

