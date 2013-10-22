\name{get.es}
\alias{get.es}
\title{get es}
\usage{
  get.es(temp)
}
\arguments{
  \item{temp}{temperature in degrees C}
}
\value{
  saturation vapor pressure in mb
}
\description{
  Calculate saturation vapor pressure
}
\examples{
temp <- -30:30
plot(temp, get.es(0, temp))
}
\author{
  David LeBauer
}

