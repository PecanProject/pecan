\name{ncep.gather2}
\alias{ncep.gather2}
\title{ncep.gather2}
\usage{
ncep.gather2(variable, level = "gaussian", months.minmax = c(1, 12), inputs,
  reanalysis2 = TRUE, return.units = FALSE)
}
\arguments{
  \item{variable}{variable to be extracted from NCEP data
  (see ?NCEP.gather)}

  \item{level}{passed to NCEP.gather, either "gaussian"
  (default), "surface" or numeric value of pressure}

  \item{inputs}{list of parameters passed to InputForWeach}
}
\value{
data from NCEP
}
\description{
Wrapper for \code{\link{RNCEP::NCEP.gather}} function to
specify custom defaults
}
\author{
David LeBauer
}

