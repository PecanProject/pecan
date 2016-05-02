\name{Licor.QC}
\alias{Licor.QC}
\title{Licor.QC}
\usage{
Licor.QC(dat, curve = c("ACi", "AQ"), tol = 0.05)
}
\arguments{
  \item{dat}{data frame}

  \item{curve}{Whether to do Quality Control by examining
  the "ACi" curve, the "AQ" curve, or both}

  \item{tol}{Code automatically tries to separate ACi and
  AQ curves in the same dataset by detecting the
  'reference' condition for light and CO2 respectively.
  This is the relative error around the mode in that
  detection.}
}
\description{
Licor.QC
}
\author{
Mike Dietze
}

