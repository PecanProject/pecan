\name{pdf.stats}
\alias{pdf.stats}
\title{Probability Distirbution Function Statistics}
\usage{
pdf.stats(distn, A, B)
}
\arguments{
  \item{distn}{name of distribution used by R (beta, f,
  gamma, lnorm, norm, weibull)}

  \item{A}{first parameter}

  \item{B}{second parameter}
}
\value{
list with mean, variance, and 95 CI
}
\description{
Calculate mean, variance statistics, and CI from a known
distribution
}
\author{
David LeBauer
}

