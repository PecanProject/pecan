\name{logger.enable}
\alias{logger.enable}
\title{Configure logging level.}
\usage{
  logger.enable(level, enable = TRUE)
}
\arguments{
  \item{level}{the level of the message (DEBUG, INFO, WARN,
  ERROR)}

  \item{enable}{wheter or not the messages should be
  printed at this level.}
}
\description{
  This will configure the logger level. This allows to turn
  DEBUG, INFO, WARN and ERROR messages on and off.
}
\examples{
\dontrun{
logger.enable("DEBUG", TRUE)
}
}
\author{
  Rob Kooper
}

