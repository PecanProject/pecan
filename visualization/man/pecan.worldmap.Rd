\name{pecan.worldmap}
\alias{pecan.worldmap}
\title{PEcAn worldmap}
\usage{
  pecan.worldmap(infile, outfile = "worldmap.png")
}
\arguments{
  \item{infile}{csv file with columns lat, lon, and
  (variable)}

  \item{outfile}{png file}
}
\value{
  NULL plot as side effect
}
\description{
  Plot map of world with model output
}
\details{
  This is just a first draft
}
\examples{
pecan.worldmap(infile = system.file("extdata/miscanthusyield.csv",
                                     package = "PEcAn.visualization"),
               outfile = file.path(tempdir(), 'worldmap.png'))
}
\author{
  David LeBauer
}

