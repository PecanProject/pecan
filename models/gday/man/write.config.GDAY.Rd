\name{write.config.GDAY}
\alias{write.config.GDAY}
\title{Write GDAY configuration files}
\usage{
write.config.GDAY(defaults, trait.values, settings, run.id)
}
\arguments{
  \item{defaults}{list of defaults to process}

  \item{trait.samples}{vector of samples for a given trait}

  \item{settings}{list of settings from pecan settings
  file}

  \item{run.id}{id of run}
}
\value{
configuration file for GDAY for given run
}
\description{
Writes a config file for GDAY
}
\details{
Requires a pft xml object, a list of trait values for a
single model run, and the name of the file to create
}
\author{
Martin De Kauwe
}

