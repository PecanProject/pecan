% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run.meta.analysis.R
\name{p.point.in.prior}
\alias{p.point.in.prior}
\title{find quantile of point within prior distribution}
\usage{
p.point.in.prior(point, prior)
}
\arguments{
\item{point}{quantile of given prior to return}

\item{prior}{list of distn, parama, paramb}
}
\value{
result of \verb{p<distn>(point, parama, paramb)}
}
\description{
compare point to prior distribution
}
\details{
used to compare data to prior, meta analysis posterior to prior
}
\author{
David LeBauer
}
