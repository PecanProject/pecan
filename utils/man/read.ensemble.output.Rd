\name{read.ensemble.output}
\alias{read.ensemble.output}
\title{Read ensemble output}
\usage{
  read.ensemble.output(ensemble.size, outdir, start.year,
    end.year, variables, model)
}
\arguments{
  \item{ensemble.size}{}

  \item{outdir}{}

  \item{pft.name}{}

  \item{start.year}{}

  \item{end.year}{}

  \item{variable}{}

  \item{model}{}
}
\value{
  a list of ensemble output
}
\description{
  Reads output from model ensemble
}
\details{
  Reads output for an ensemble of length specified by
  \code{ensemble.size} and bounded by \code{start.year} and
  \code{end.year}
}

