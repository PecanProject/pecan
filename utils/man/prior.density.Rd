\name{prior.density}
\alias{prior.density}
\title{Prior Density}
\usage{
  prior.density(distribution = "norm", a = 0, b = 1,
    xlim = NA)
}
\arguments{
  \item{distribution}{one of R's supported distributions
  (character)}

  \item{a}{first parameter of distribution (numeric)}

  \item{b}{second parameter of distribution (numeric)}
}
\value{
  data frame with values of x, the density at each x and
  the probability at each x
}
\description{
  Calculate the density of a distribution for use in
  plotting
}
\author{
  David LeBauer
}

