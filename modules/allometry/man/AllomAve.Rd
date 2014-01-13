\name{AllomAve}
\alias{AllomAve}
\title{AllomAve}
\usage{
  AllomAve(pfts, components = c(6, 18, 43), outdir,
    con = NULL, field = NULL,
    parm = system.file("data/Table3_GTR-NE-319.v2.csv", package = "PEcAn.allometry"),
    ngibbs = 5000, nchain = 3)
}
\arguments{
  \item{pfts}{pft list from PEcAn settings (if con) OR list
  of pft spcd's}

  \item{components}{IDs for allometry components from
  Jenkins et al 2004 Table 5}

  \item{outdir}{output directory files are written to}

  \item{con}{database connection}

  \item{field}{path(s) to raw data files}

  \item{parm}{path to allometry equation file (default is
  Jenkins Table 3)}

  \item{ngibbs}{number of MCMC iterations (per chain) to
  run}

  \item{nchain}{number of MCMC chains}
}
\value{
  none
}
\description{
  AllomAve
}

