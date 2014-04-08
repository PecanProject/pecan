\name{as.sequence}
\alias{as.sequence}
\title{as.sequence}
\usage{
as.sequence(x, na.rm = TRUE)
}
\arguments{
  \item{x}{categorical variable as vector}

  \item{na.rm}{logical: return NA's or replace with max(x)
  + 1}
}
\value{
sequence from 1:length(unique(x))
}
\description{
Convert categorical variable into sequential integers
}
\details{
Turns any categorical variable into a sequential integer.
This transformation is required for using data in BUGS/JAGS
}
\author{
David LeBauer
}

