\name{fqdn}
\alias{fqdn}
\title{Returns the fully qualified hostname.}
\usage{
  fqdn()
}
\value{
  fully qualified hostname
}
\description{
  Returns the fully qualified hostname. This is potentially
  different from Sys.info()['nodename'] which can return
  just the hostname part and not the domain as well. For
  example the machine pecan.ncsa.illinois.edu will return
  just that as fqdn but only pecan for hostname.
}
\examples{
fqdn()
}
\author{
  Rob Kooper
}

