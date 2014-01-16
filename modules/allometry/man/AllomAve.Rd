\name{AllomAve}
\alias{AllomAve}
\title{AllomAve}
\usage{
  AllomAve(pfts, components = 6, outdir = NULL, con = NULL,
    field = NULL,
    parm = system.file("data/Table3_GTR-NE-319.v2.csv", package = "PEcAn.allometry"),
    ngibbs = 5000, nchain = 3)
}
\arguments{
  \item{pfts}{pft list from PEcAn settings (if con) OR list
  of pft spcd's \itemize{ \item{"acronym"}{ - USDA species
  acronyms (see plants.usda.gov), used with FIELD data
  (vector)} \item{"spcd"}{ - USFS species codes, use with
  PARM data (vector)} \item{"name"}{ - name used to
  identify output files (single)} }}

  \item{components}{IDs for allometry components from
  Jenkins et al 2004 Table 5. Default is stem biomass (6).
  See data(allom.components)}

  \item{outdir}{output directory files are written to.
  Default is getwd()}

  \item{con}{database connection}

  \item{field}{path(s) to raw data files}

  \item{parm}{path to allometry equation file (NULL default
  loads Jenkins Table 3)}

  \item{ngibbs}{number of MCMC iterations (per chain) to
  run}

  \item{nchain}{number of MCMC chains}
}
\value{
  nested list of parameter summary statistics
}
\description{
  allometery wrapper function that handles loading and
  subsetting the data, fitting the Bayesian models, and
  generating diagnostic figures. Set up to loop over
  multiple PFTs and components.  Writes raw MCMC and PDF of
  diagnositcs to file and returns table of summary stats.
}
\details{
  There are two usages of this function. When running
  "online" (connected to the PEcAn database), pass the
  database connection, con, and the pfts subsection of the
  PEcAn settings. Wheb running "stand alone" pass the pft
  list mapping species to species codes and the file paths
  to the allometry table and field data (optional)
}
\examples{
pfts = list(FAGR = data.frame(spcd=531,name="FAgr",acronym="FAGR"))
allom.stats = AllomAve(pfts,ngibbs=500)
}
\author{
  Michael Dietze
}

