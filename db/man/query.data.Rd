\name{query.data}
\alias{query.data}
\title{Query data and transform stats to SE by calling \code{\link{fetch.stats2se}};}
\usage{
  query.data(trait, spstr,
    extra.columns = "sites.lat, sites.lon, ", con = NULL,
    ...)
}
\arguments{
  \item{trait}{trait to query from the database}

  \item{spstr}{}

  \item{extra.columns}{}

  \item{con}{database connection}

  \item{...}{extra arguments}
}
\description{
  Function to query data from database for specific species
  and convert stat to SE
}
\author{
  David LeBauer, Carl Davidson
}
\seealso{
  used in \code{\link{query.trait.data}};
  \code{\link{fetch.stats2se}};
  \code{\link{transformstats}} performs transformation
  calculations
}

