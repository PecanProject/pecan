\name{write.config.GDAY}
\alias{write.config.GDAY}
\title{Write GDAY configuration files}
\usage{
write.config.GDAY(settings, run.id)
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
Writes a config file for GDAY
}
\details{
Requires a pft xml object, a list of trait values for a
single model run, and the name of the file to create
}
\author{
Martin De Kauwe
}
