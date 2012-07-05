\name{read.output.generic}
\alias{read.output.generic}
\title{Read output}
\usage{
  read.output.generic(run.id, outdir, start.year = NA,
    end.year = NA)
}
\arguments{
  \item{run.id}{the id distiguishing the model run}

  \item{outdir}{the directory that the model's output was
  sent to}

  \item{start.year}{}

  \item{end.year}{}
}
\value{
  vector of output variable
}
\description{
  Reads the output of a single model run
}

