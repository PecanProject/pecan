\name{get.results}
\alias{get.results}
\title{Generate model output for PEcAn analyses}
\usage{
  get.results(model)
}
\arguments{
  \item{model}{name of model being used}

  \item{settings}{list, read from settings file (xml) using
  \code{\link{read.settings}}}
}
\description{
  Reads model output and runs sensitivity and ensemble
  analyses
}
\details{
  Output is placed in model output directory
  (settings$run$host$outdir).
}
\author{
  Shawn Serbin, David LeBauer, Mike Dietze

  David LeBauer, Shawn Serbin, Mike Dietze
}

