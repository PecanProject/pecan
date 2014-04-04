\name{get.elasticity}
\alias{get.elasticity}
\title{Get Elasticity}
\usage{
get.elasticity(sensitivity, samples, outputs)
}
\arguments{
  \item{sensitivity}{univariate sensitivity of model to a
  parameter, can be calculated by
  \code{\link{get.sensitivity}}}

  \item{samples}{samples from trait distribution}

  \item{outputs}{model output from ensemble runs}
}
\value{
elasticity = normalized sensitivity
}
\description{
Generic function for the elasticity
}
\details{
Given the sensitivity, samples, and outputs for a single
trait, return elasticity
}

