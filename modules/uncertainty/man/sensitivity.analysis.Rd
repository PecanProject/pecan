\name{sensitivity.analysis}
\alias{sensitivity.analysis}
\title{Sensitivity Analysis}
\usage{
  sensitivity.analysis(trait.samples, sa.samples,
    sa.output, outdir)
}
\arguments{
  \item{trait.samples}{list of vectors, one per trait,
  representing samples of the trait value, with length
  equal to the mcmc chain length. Samples are taken from
  either the prior distribution or meta-analysis results}

  \item{sa.samples}{data.frame with one column per trait
  and one row for the set of quantiles used in sensitivity
  analysis. Each cell contains the value of the trait at
  the given quantile.}

  \item{sa.output}{list of data.frames, similar to
  sa.samples, except cells contain the results of a model
  run with that trait x quantile combination and all other
  traits held at their median value}

  \item{outdir}{directory to which plots are written}
}
\value{
  results of sensitivity analysis
}
\description{
  Performs univariate sensitivity analysis and variance
  decomposition
}
\details{
  This function estimates the univariate responses of a
  model to a parameter for a set of traits, calculates the
  model sensitivity at the median, and performs a variance
  decomposition. This function results in a set of
  sensitivity plots (one per variable) and variance
  decomposition plot.
}
\examples{
\dontrun{
sensitivity.analysis(trait.samples[[pft$name]], sa.samples[[pft$name]], sa.agb[[pft$name]], pft$outdir)
}
}
\author{
  David LeBauer
}

