\name{query.base.con}
\alias{query.base.con}
\title{Query database connection}
\usage{
  query.base.con(settings, ...)
}
\arguments{
  \item{...}{optional arguments for connecting to database
  (e.g. password, user name, database)}
}
\value{
  database connection object
}
\description{
  Creates database connection object.
}
\details{
  Also removes any existing connections.

  Deprecated, please use db.open
}
\examples{
\dontrun{
con <- query.base.con(settings)
}
}

