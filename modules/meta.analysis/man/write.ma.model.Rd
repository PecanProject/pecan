% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta.analysis.write.model.R
\name{write.ma.model}
\alias{write.ma.model}
\title{write.ma.model}
\usage{
write.ma.model(
  modelfile,
  outfile,
  reg.model,
  pr.dist,
  pr.param.a,
  pr.param.b,
  n,
  trt.n,
  site.n,
  ghs.n,
  tauA,
  tauB
)
}
\arguments{
\item{modelfile}{model template file (ma.model.template.R)}

\item{outfile}{file name of model created}

\item{reg.model}{structure of regression model}

\item{pr.dist}{A string representing the root distribution name used by R, e.g. 'norm', 'lnorm', 'gamma', 'beta', etc.}

\item{pr.param.a}{first parameter value accepted by \code{pr.dist}}

\item{pr.param.b}{second parameter value accepted by \code{pr.dist}}

\item{n}{number of observations in data}

\item{trt.n}{number of distinct treatments in data}

\item{site.n}{number of distinct sites in data}

\item{ghs.n}{= 1 if only non-greenhouse or greenhouse studies included, 2 if both}

\item{tauA}{parameter a for gamma prior on precision}

\item{tauB}{parameter b for gamma prior on precision}
}
\value{
Nothing, but as a side effect, the model is written
}
\description{
Convert template ma.model.template.R to a JAGS model.
}
\details{
Writes a meta-analysis model based on available data and prior specification.
Inspired by the \code{R2WinBUGS::write.model} by Jouni Kerman and Uwe Ligges.
}
\author{
David LeBauer and Mike Dietze.
}
