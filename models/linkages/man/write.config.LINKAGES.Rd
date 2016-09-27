\name{write.config.LINKAGES}
\alias{write.config.LINKAGES}
\title{Write LINKAGES configuration files}
\usage{
write.config.LINKAGES(defaults = NULL, trait.values, settings, run.id,
  restart = NULL, spinup = NULL)
}
\arguments{
  \item{defaults}{list of defaults to process}

  \item{trait.samples}{vector of samples for a given trait}

  \item{settings}{list of settings from pecan settings
  file}

  \item{run.id}{id of run}
}
\value{
configuration file for LINKAGES for given run
}
\description{
Writes a LINKAGES config file.
}
\details{
Requires a pft xml object, a list of trait values for a
single model run, and the name of the file to create
}
\author{
Ann Raiho, Betsy Cowdery
}

