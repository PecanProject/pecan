\name{query.priors}
\alias{query.priors}
\title{Query Priors}
\usage{
  query.priors(pft, trstr, out = NULL, con = NULL, ...)
}
\arguments{
  \item{pft}{String name of the PFT in the database}

  \item{trstr}{string of traits to query priors for}

  \item{con}{database connection, can be list of arguments
  for connecting to database}

  \item{...}{optional arguments for connecting to database
  (e.g. password, user name, database)}
}
\value{
  return priors for a given pft
}
\description{
  Query priors associated with a string of traits and plant
  functional type
}
\examples{
\dontrun{
query.bety.priors('ebifarm.pavi', c('SLA', 'Vcmax', 'leaf_width'))
}
}

