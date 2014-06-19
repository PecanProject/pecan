\name{get.trait.data}
\alias{get.trait.data}
\title{Gets trait data from the database}
\usage{
  get.trait.data(pfts, dbfiles, database, forceupdate,
    trait.names = NULL)
}
\arguments{
  \item{pfts}{the list of pfts to get traits for}

  \item{dbfiles}{location where previous results are found}

  \item{database}{database connection parameters}

  \item{forceupdate}{set this to true to force an update,
  auto will check to see if an update is needed.}

  \item{trait.names}{list of traits to query. If TRUE, uses
  trait.dictionary}
}
\value{
  list of pfts with update posteriorids
}
\description{
  Get trait data from the database.
}
\details{
  This will use the following items from setings: -
  settings$pfts - settings$database$bety - settings$run$dbfiles
  - settings$meta.analysis$update
}
\author{
  David LeBauer, Shawn Serbin
}

