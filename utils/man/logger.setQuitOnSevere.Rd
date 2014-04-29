\name{logger.setQuitOnSevere}
\alias{logger.setQuitOnSevere}
\title{Configure wheter severe should quit.}
\usage{
  logger.setQuitOnSevere(severeQuits)
}
\arguments{
  \item{severeQuits}{should R quit on a severe error.}
}
\description{
  The default is for a non-interactive session to quit.
  Setting this to false is especially useful for running
  tests when placed in \codePinst/tests/test.<fn>.R}, but
  is not passed from \code{tests/run.all.R}.
}
\examples{
\dontrun{
logger.setQuitOnSevere(FALSE)
}
}
\author{
  Rob Kooper
}

