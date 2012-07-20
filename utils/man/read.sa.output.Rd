\name{read.sa.output}
\alias{read.sa.output}
\title{Read Sensitivity Analysis output}
\usage{
  read.sa.output(traits, quantiles, outdir, pft.name = "",
    start.year, end.year, variables, model)
}
\arguments{
  \item{traits}{}

  \item{quantiles}{}

  \item{outdir}{}

  \item{pft.name}{}

  \item{start.year}{}

  \item{end.year}{}

  \item{read.output}{model specific read.output function}
}
\value{
  dataframe with one col per quantile analysed and one row
  per trait, each cell is a list of AGB over time
}
\description{
  Reads output of sensitivity analysis runs
}

