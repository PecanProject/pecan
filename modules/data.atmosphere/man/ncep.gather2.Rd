\name{ncep.gather2}
\alias{ncep.gather2}
\title{ncep.gather2}
\usage{
  ncep.gather2(variable, level = "gaussian", inputs)
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
\examples{
Uwind <- ncep.gather2(variable = "uwnd.10m", inputs = list(lat = 40, lon = 40, start.year = 2000, end.year = 2001))
}
\author{
  David LeBauer
}

