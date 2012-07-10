\name{add.prior.density}
\alias{add.prior.density}
\title{Add Prior Density}
\usage{
  add.prior.density(prior.density, base.plot = NULL,
  prior.color = "black")
}
\arguments{
  \item{prior.density}{}

  \item{base.plot}{a ggplot object (grob), created by
  \code{\link{create.base.plot}} if none provided}

  \item{prior.color}{color of line to be plotted}
}
\value{
  plot with prior density added
}
\description{
  Plots a prior density from a parameterized probability
  distribution
}
\examples{
add.prior.density(pr.dens('norm', 0, 1))
}
\author{
  David LeBauer
}
\seealso{
  \code{\link{pr.dens}}
}

