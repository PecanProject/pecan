\name{fetch.stats2se}
\alias{fetch.stats2se}
\title{Fetch data and transform stats to SE}
\usage{
  fetch.stats2se(connection, query)
}
\arguments{
  \item{connection}{connection to BETYdb}

  \item{query}{MySQL query to traits table}
}
\value{
  dataframe with trait data
}
\description{
  Queries data from BETY and transforms statistics to SE
}
\details{
  Performs query and then uses \code{transformstats} to
  convert miscellaneous statistical summaries to SE
}
\seealso{
  used in \code{\link{query.trait.data}};
  \code{\link{transformstats}} performs transformation
  calculations
}

