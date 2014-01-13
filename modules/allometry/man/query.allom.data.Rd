\name{query.allom.data}
\alias{query.allom.data}
\title{query.allom.data}
\usage{
  query.allom.data(pft_name, variable, con, nsim = 10000)
}
\arguments{
  \item{pft_name}{name of Plant Functional Type to be
  queried}

  \item{variable}{name of response variable}

  \item{con}{open database connection}

  \item{nsim}{number of pseudo-data simulations for
  estimating SE

  database is assumed to conform to the PEcAn Schema}
}
\description{
  Module to grab allometric information from the raw data
  table Will grab both original field data and tallied
  equations

  Tallied equation format based on Jenkins et al 2004 USFS
  General Technical Report NE-319
}
\author{
  Michael Dietze
}

