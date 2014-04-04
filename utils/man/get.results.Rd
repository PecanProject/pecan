\name{get.results}
\alias{get.results}
\title{Generate model output for PEcAn analyses}
\usage{
get.results(settings)
}
\arguments{
  \item{settings}{list, read from settings file (xml) using
  \code{\link{read.settings}}}
}
\description{
Reads model output and runs sensitivity and ensemble
analyses
}
\details{
Output is placed in model output directory
(settings$modeloutdir).
}
\author{
David LeBauer, Shawn Serbin, Mike Dietze
}

