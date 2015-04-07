\name{calcSpatialCov}
\alias{calcSpatialCov}
\alias{calcSpatialCov.list}
\alias{calcSpatialCov.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate Spatial Covariance }
\description{
}
\usage{
calcSpatialCov(x, ...)
calcSpatialCov.list(d, psi, tau)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{ either square distance matrix or list of component distance matrices }
  \item{psi}{ spatial correlation coefficient }
  \item{tau}{ spatial variance }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  returns covariance matrix
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{
Currently assumes an exponential spatial dependency can make gaussian by passing squared distance matrix
}
\seealso{}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

