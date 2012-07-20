\name{read.output}
\alias{read.output}
\title{Read output}
\usage{
  read.output(run.id, outdir, start.year = NA,
    end.year = NA, variables = "GPP", model = model)
}
\arguments{
  \item{run.id}{the id distiguishing the model run}

  \item{outdir}{the directory that the model's output was
  sent to}

  \item{start.year}{}

  \item{end.year}{}

  \item{variables}{}

  \item{model}{}
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

