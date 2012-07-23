\name{read.ensemble.output}
\alias{read.ensemble.output}
\title{Read ensemble output}
\usage{
  read.ensemble.output(ensemble.size, outdir, start.year,
    end.year, variables, model)
}
\arguments{
  \item{ensemble.size}{the number of ensemble members run}

  \item{outdir}{directory with model output to use in
  ensemble analysis}

  \item{start.year}{first year to include in ensemble
  analysis}

  \item{end.year}{last year to include in ensemble
  analysis}

  \item{variables}{targe variables for ensemble analysis}

  \item{model}{ecosystem model run}
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

