\name{run.write.configs}
\alias{run.write.configs}
\title{Run model specific write configuration functions}
\usage{
  run.write.configs(model, write = TRUE)
}
\arguments{
  \item{model}{the ecosystem model to generate the
  configuration files for}

  \item{write}{should the runs be written to the database}
}
\description{
  Main driver function to call the ecosystem model specific
  (e.g. ED, SiPNET) run and configuration file scripts
}
\author{
  David LeBauer, Shawn Serbin
}

