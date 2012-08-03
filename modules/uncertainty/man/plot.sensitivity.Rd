\name{plot.sensitivity}
\alias{plot.sensitivity}
\title{Sensitivity plot}
\usage{
  plot.sensitivity(sa.sample, sa.spline, trait,
    y.range = c(0, 50), median.i = 4,
    prior.sa.sample = NULL, prior.sa.spline = NULL,
    fontsize = list(title = 12, axis = 8), linesize = 1,
    dotsize = 2)
}
\arguments{
  \item{sa.sample}{trait quantiles used in sensitivity
  analysis}

  \item{sa.spline}{spline function estimated from
  sensitivity analysis}

  \item{trait}{trait name for title}

  \item{y.range}{}

  \item{median.i}{index of median value in sa.sample;
  \code{median.i == which(as.numeric(rownames(sa.sample))
  == 50) }}

  \item{prior.sa.sample}{similar to sa.sample, but for
  prior distribution. If given, plots sensitivity for prior
  run}

  \item{prior.sa.spline}{similar to sa.spline, but for
  prior trait distribution.}

  \item{fontsize}{(optional) list with three arguments that
  can be set to vary the fontsize of the title, axis
  labels, and axis title in the sensitivity plots}
}
\value{
  object of class ggplot
}
\description{
  Plot univariate response of model output to a trait
  parameter.
}
\details{
  Plots for a single trait; called by
  \code{\link{plot.sensitivities}} to plot sensitivity
  plots for multiple traits.
}

