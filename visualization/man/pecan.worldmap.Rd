\name{pecan.worldmap}
\alias{pecan.worldmap}
\title{PEcAn worldmap}
\usage{
  pecan.worldmap(df.in, outfile = "worldmap.png",
    xlim = c(-130, -30), ylim = c(-40, 60))
}
\arguments{
  \item{df.in}{data.frame with columns lat, lon, and
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
miscanthusyield <- read.csv(system.file("extdata/miscanthusyield.csv",
                                     package = "PEcAn.visualization"))
pecan.worldmap(df.in = miscanthusyield,
               outfile = file.path(tempdir(), 'worldmap.png'))
}
\author{
  David LeBauer
}

