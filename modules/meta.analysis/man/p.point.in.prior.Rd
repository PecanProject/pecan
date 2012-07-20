\name{p.point.in.prior}
\alias{p.point.in.prior}
\title{find quantile of point within prior distribution}
\usage{
  p.point.in.prior(point, prior)
}
\arguments{
  \item{point}{}

  \item{prior}{list of distn, parama, paramb}
}
\value{
  result of p<distn>(point, parama, paramb)
}
\description{
  compare point to prior distribution
}
\details{
  used to compare data to prior, meta analysis posterior to
  prior
}
\author{
  David LeBauer
}

