\name{read.outputs}
\alias{read.outputs}
\title{Read outputs}
\usage{
  read.outputs(model, settings, ...)
}
\arguments{
  \item{model}{name of simulation model currently accepts
  ("ED", "SIPNET", "BIOCRO")}

  \item{settings}{settings loaded from pecan.xml}

  \item{...}{arguments passed to \code{\link{read.output}},
  e.g. \code{variables}, \code{start.year},
  \code{end.year}}
}
\description{
  Reads the output of all model runs
}
\author{
  Rob Kooper
}

