\name{get.sample}
\alias{get.sample}
\title{Get Samples}
\usage{
  get.sample(prior, n)
}
\arguments{
  \item{prior}{data.frame with distn, parama, paramb}

  \item{n}{number of samples to return}
}
\value{
  vector with n random samples from prior
}
\description{
  Take n random samples from prior
}
\details{
  Like pr.samp, with prior as a single input
}
\seealso{
  \link{pr.samp}
}

