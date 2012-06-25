\name{assign.treatments}
\alias{assign.treatments}
\title{assign.treatments}
\usage{
  assign.treatments(data)
}
\arguments{
  \item{data}{input data}
}
\value{
  dataframe with sequential treatments
}
\description{
  Change treatments to sequential integers
}
\details{
  Assigns all control treatments the same value, then
  assigns unique treatments within each site. Each site is
  required to have a control treatment. The algorithm
  (incorrectly) assumes that each site has a unique set of
  experimental treatments.
}
\author{
  David LeBauer, Carl Davidson
}

