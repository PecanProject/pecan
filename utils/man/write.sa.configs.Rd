\name{write.sa.configs}
\alias{write.sa.configs}
\title{Write sensitivity analysis configs}
\usage{
  write.sa.configs(defaults, quantile.samples, host,
  outdir, settings, model = "ED2", clean = FALSE)
}
\arguments{
  \item{pft}{pft id used to query PEcAn database}

  \item{quantile.samples}{}

  \item{host}{server to which config files will be sent}

  \item{outdir}{directory for model output (on server)}

  \item{settings}{list of settings}

  \item{write.config}{a model-specific function to write
  config files, e.g. \link{write.config.ED}}

  \item{convert.samples}{a model-specific function that
  transforms variables from units used in database to units
  used by model, e.g. \link{convert.samples.ED}}

  \item{ensemble.samples}{list of lists supplied by
  \link{get.sa.samples}}
}
\value{
  nothing, writes sensitivity analysis configuration files
  as a side effect
}
\description{
  Write sensitivity analysis config files
}
\details{
  Writes config files for use in sensitivity analysis.
}
\author{
  David LeBauer, Carl Davidson
}

