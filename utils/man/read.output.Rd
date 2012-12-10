\name{read.output}
\alias{read.output}
\title{Read output}
\usage{
  read.output(run.id, outdir, start.year = NA,
    end.year = NA, variables = "GPP")
}
\arguments{
  \item{run.id}{the id distiguishing the model run}

  \item{outdir}{the directory that the model's output was
  sent to}

  \item{start.year}{}

  \item{end.year}{}

  \item{variables}{}

  \item{model}{}
}
\value{
  vector of output variable
}
\description{
  Reads the output of a single model run
}
\details{
  Generic function to convert model output in from MsTMIP
  units (kg m-2 s-1) to kg ha-1 y-1. Currently this
  function converts Carbon fluxes: GPP, NPP, NEE,
  TotalResp, AutoResp, HeteroResp, DOC_flux, Fire_flux, and
  Stem (Stem is specific to the BioCro model) and Water
  fluxes: Evaporation (Evap), Transpiration(TVeg), surface
  runoff (Qs), subsurface runoff (Qsb), and rainfall
  (Rainf). For more details, see the
  \href{http://nacp.ornl.gov/MsTMIP_variables.shtml}{MsTMIP
  variables} documentation
}
\author{
  Michael Dietze
}

