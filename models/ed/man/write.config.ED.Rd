\name{write.config.ED}
\alias{write.config.ED}
\title{Write ED configuration files}
\usage{
  write.config.ED(defaults, trait.values, settings, outdir,
    run.id)
}
\arguments{
  \item{pft}{}

  \item{trait.samples}{vector of samples for a given trait}

  \item{settings}{list of settings from pecan settings
  file}

  \item{outdir}{directory for config files to be written
  to}

  \item{run.id}{id of run}
}
\value{
  configuration file and ED2IN namelist for given run
}
\description{
  Writes an xml and ED2IN config files for use with the
  Ecological Demography model.
}
\details{
  Requires a pft xml object, a list of trait values for a
  single model run, and the name of the file to create
}
\author{
  David LeBauer, Shawn Serbin, Carl Davidson
}

