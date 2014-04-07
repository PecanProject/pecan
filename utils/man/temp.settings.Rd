\name{temp.settings}
\alias{temp.settings}
\title{temp.settings}
\usage{
temp.settings(settings.txt)
}
\arguments{
  \item{settings.txt}{}
}
\value{
character vector written to and read from a temporary file
}
\description{
Create a temporary settings file
}
\details{
Uses \code{\link{tempfile}} function to provide a valid
temporary file (OS independent) Useful for testing
functions that depend on settings file Reference:
http://stackoverflow.com/a/12940705/199217
}
\author{
David LeBauer
}

