\name{db.exists}
\alias{db.exists}
\title{db.exists}
\usage{
db.exists(params, write = TRUE)
}
\arguments{
  \item{params}{database connection information}
}
\value{
TRUE if database connection works; else FALSE
}
\description{
Test connection to database
}
\details{
Useful to only run tests that depend on database when a
connection exists
}
\author{
David LeBauer, Rob Kooper
}

