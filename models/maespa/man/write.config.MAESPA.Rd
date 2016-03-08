\name{write.config.MAESPA}
\alias{write.config.MAESPA}
\title{Write MAESPA configuration files}
\usage{
write.config.MAESPA(settings, run.id)
}
\arguments{
  \item{settings}{list of settings from pecan settings
  file}

  \item{run.id}{id of run}
}
\value{
configuration file for MODEL for given run
}
\description{
Writes a config file for Maespa
}
\details{
Requires a pft xml object, a list of trait values for a
single model run, and the name of the file to create
}
\author{
Tony Gardella
}

