\name{pr.dens}
\alias{pr.dens}
\title{Calculate densities}
\usage{
  pr.dens(distn, parama, paramb, n = 1000, alpha = 0.001)
}
\arguments{
  \item{distn}{distribution}

  \item{parama}{parameter}

  \item{paramb}{parameter}

  \item{n}{length of vector to be returned}

  \item{alpha}{sets range at which the distribution will be
  evaluated (e.g. from alpha to 1-alpha)}
}
\value{
  dataframe with equally spaced x values and the
  corresponding densities
}
\description{
  Calculates density at n points across the range of a
  parameter
}
\details{
  For a distribution and parameters, return the density for
  values ranging from alpha to 1-alpha
}
\author{
  David LeBauer
}

