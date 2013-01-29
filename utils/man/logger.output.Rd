\name{logger.output}
\alias{logger.output}
\title{Configure logging output.}
\usage{
  logger.output(filename = .local$filename,
    console = .local$console)
}
\arguments{
  \item{filename}{the file to send the log messages to (or
  NA to not write to file)}

  \item{console}{set to true if the logger should write to
  the console}
}
\description{
  This will configure the logger.
}
\examples{
\dontrun{
logger.output(console=FALSE)
}
}
\author{
  Rob Kooper
}

