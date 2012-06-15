\name{query.pft_species}
\alias{query.pft_species}
\title{Query species given pft name}
\usage{
  query.pft_species(pft, con = NULL, ...)
}
\arguments{
  \item{pft}{string pft name}

  \item{con}{database connection}

  \item{...}{optional arguments for connecting to database
  (e.g. password, user name, database)}
}
\value{
  string of species.id for species associated with pft
}
\description{
  select plant id's associated with pft
}
\examples{
\dontrun{
query.pft_species('ebifarm.pavi')
}
}

