\name{read.output.SIPNET}
\alias{read.output.SIPNET}
\title{Read output}
\usage{
  read.output.SIPNET(run.id, outdir, start.year = NA,
    end.year = NA, variables = "NPP")
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
\author{
  Michael Dietze
}

