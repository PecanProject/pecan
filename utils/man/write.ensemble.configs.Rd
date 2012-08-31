\name{write.ensemble.configs}
\alias{write.ensemble.configs}
\title{Write ensemble configs}
\usage{
  write.ensemble.configs(defaults, ensemble.samples, host,
    outdir, settings, model, clean = FALSE)
}
\arguments{
  \item{defaults}{pft}

  \item{ensemble.samples}{list of lists supplied by
  \link{get.ensemble.samples}}

  \item{host}{server to which config files will be sent}

  \item{outdir}{directory for model output (on server)}

  \item{settings}{list of PEcAn settings}

  \item{write.config}{a model-specific function to write
  config files, e.g. \link{write.config.ED}}

  \item{clean}{remove old output first?}
}
\value{
  nothing, writes ensemble configuration files as a side
  effect
}
\description{
  Write ensemble config files
}
\details{
  Writes config files for use in meta-analysis and returns
  a list of run ids. Given a pft.xml object, a list of
  lists as supplied by get.sa.samples, a name to
  distinguish the output files, and the directory to place
  the files.
}
\author{
  David LeBauer, Carl Davidson
}

