\name{query.yields}
\alias{query.yields}
\title{Query yield data and transform stats to SE by calling \code{\link{fetch.stats2se}};}
\usage{
  query.yields(trait = "yield", spstr, extra.columns = "",
    con = query.base.con(settings), ...)
}
\arguments{
  \item{trait}{yield trait to query}

  \item{spstr}{species to query for yield data}

  \item{extra.columns}{}

  \item{con}{database connection}

  \item{...}{extra arguments}
}
\description{
  Function to query yields data from database for specific
  species and convert stat to SE
}
\author{
  <unknown>
}
\seealso{
  used in \code{\link{query.trait.data}};
  \code{\link{fetch.stats2se}};
  \code{\link{transformstats}} performs transformation
  calculations
}

