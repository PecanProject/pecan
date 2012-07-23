\name{read.sa.output}
\alias{read.sa.output}
\title{Read Sensitivity Analysis output}
\usage{
  read.sa.output(traits, quantiles, outdir, pft.name = "",
    start.year, end.year, variables, model)
}
\arguments{
  \item{traits}{model parameters included in the
  sensitivity analysis}

  \item{quantiles}{quantiles selected for sensitivity
  analysis}

  \item{outdir}{directory with model output to use in
  sensitivity analysis}

  \item{pft.name}{name of PFT used in sensitivity analysis
  (Optional)}

  \item{start.year}{first year to include in sensitivity
  analysis}

  \item{end.year}{last year to include in sensitivity
  analysis}

  \item{read.output}{model specific read.output function}
}
\value{
  dataframe with one col per quantile analysed and one row
  per trait, each cell is a list of AGB over time
}
\description{
  Reads output of sensitivity analysis runs
}

