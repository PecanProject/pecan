\name{pft.add.spp}
\alias{pft.add.spp}
\title{Associate species with a PFT.}
\usage{
  pft.add.spp(pft, acronym, test = TRUE, con = NULL, ...)
}
\arguments{
  \item{pft}{String name of the PFT in the database}

  \item{acronym}{USDA Plants Database Symbol. (standard
  genus-species acronym) see \url{http://plants.usda.gov}}

  \item{test}{Runs the function in test mode.  No species
  are actually added, but checks are run for existing
  species-PFT pairs, unmatched acronyms, missing species,
  or duplicate species}

  \item{con}{Database connection object.  If not provided
  will use \code{\link{query.bety.con}} to connect to
  BETY-db.}

  \item{...}{optional arguements for connecting to database
  (e.g. password, user name, database}
}
\value{
  Function does not return a value but does print out
  diagnostic statements.
}
\description{
  adds a list of species to a pft based on USDA Plants
  acronyms
}
\details{
  This function is used to add PFT-Species pairs to the
  database table 'pfts_species'.  In the initial
  implementation the PFT has to be defined already and the
  species are added based on their USDA Symbol
  (genus/species acronym).  Multiple species can be added
  at once but only one PFT at a time.
}
\author{
  Michael C. Dietze
}

