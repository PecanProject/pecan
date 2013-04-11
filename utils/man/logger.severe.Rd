\name{logger.severe}
\alias{logger.severe}
\title{Prints an severe message and stops execution.}
\usage{
  logger.severe(msg, errorcode = 1, ...)
}
\arguments{
  \item{msg}{the message that should be printed.}

  \item{errorcode}{the error code to return when the
  session quits.}

  \item{...}{any additional text that should be printed.}
}
\description{
  This function will print a message and stop execution of
  the code. This should only be used if the application
  should terminate. If the session is non-interactive the
  error code can be specified which is returned to the
  shell.
}
\examples{
\dontrun{
logger.severe("missing parameters")
}
}
\author{
  Rob Kooper
}

