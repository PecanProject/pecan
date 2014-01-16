\name{query.priors}
\alias{query.priors}
\title{Query Priors}
\usage{
query.priors(pft, trstr = NULL, out = NULL, con = NULL, ...)
}
\arguments{
  \item{pft}{String name of the PFT in the database}

  \item{trstr}{string of traits to query priors for}

  \item{out}{output location}

  \item{con}{database connection, can be list of arguments
  for connecting to database}

  \item{...}{optional arguments for connecting to database
  (e.g. password, user name, database)}
}
\value{
priors for a given pft
}
\description{
Query priors associated with a string of traits and plant
functional type
}
\examples{
\dontrun{
query.priors('ebifarm.pavi', vecpaste('SLA', 'Vcmax', 'leaf_width'))
}
}
\author{
David LeBauer
}

