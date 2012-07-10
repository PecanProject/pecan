\name{read.ensemble.output}
\alias{read.ensemble.output}
\title{Read Ensemble Output}
\usage{
  read.ensemble.output(ensemble.size, outdir, start.year,
  end.year, read.output = read.output.ed)

  read.ensemble.output(ensemble.size, outdir, start.year,
  end.year, read.output = read.output.ed)
}
\arguments{
  \item{ensemble.size}{}

  \item{outdir}{}

  \item{pft.name}{}

  \item{start.year}{}

  \item{end.year}{}

  \item{read.output}{model specific read output function,
  \cite{\link{read.output.ed}} by default.}
}
\value{
  list of ensemble output

  a list of ensemble output
}
\description{
  Returns list of ensemble output

  Reads output from model ensemble
}
\details{
  Reads output for an ensemble of length specified by
  \code{ensemble.size} and bounded by \code{start.year} and
  \code{end.year}
}

