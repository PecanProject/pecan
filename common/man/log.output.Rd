\name{log.output}
\alias{log.output}
\title{Configure logging output.}
\usage{
  log.output(filename = log.variables$filename,
    console = log.variables$console)
}
\arguments{
  \item{filename}{the file to send the log messages to.}

  \item{console}{set to true if the logger should write to
  the console}
}
\description{
  This will configure the logger.
}
\examples{
\dontrun{
log.output(console=FALSE)
}
}
\author{
  Rob Kooper
}

