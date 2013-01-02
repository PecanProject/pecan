\name{check.settings}
\alias{check.settings}
\title{Check Settings}
\usage{
  check.settings(settings)
}
\arguments{
  \item{settings}{settings file}
}
\value{
  error if minimum fields are not provided
}
\description{
  Sanity checks. Checks the settings file to make sure
  expected fields exist.
}
\details{
  Expected fields in settings file are: - pfts with at
  least one pft defined - database - model - run with the
  following fields -- site with id -- host with name
}
\author{
  Rob Kooper
}

