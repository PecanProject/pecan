\name{get.trait.data.pft}
\alias{get.trait.data.pft}
\title{Gets trait data from the database}
\usage{
get.trait.data.pft(pft, dbfiles, dbcon, forceupdate = TRUE,
  trait.names = traitdictionary$id)
}
\arguments{
  \item{pft}{the pft whos traits to retrieve}

  \item{dbfiles}{location where previous results are found}

  \item{dbcon}{database connection}

  \item{forceupdate}{set this to true to force an update,
  auto will check to see if an update is needed.}
}
\value{
updated pft with posteriorid
}
\description{
Get trait data from the database for a single pft
}
\author{
David LeBauer, Shawn Serbin, Rob Kooper
}

