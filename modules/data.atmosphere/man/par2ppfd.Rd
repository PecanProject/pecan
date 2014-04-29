\name{par2ppfd}
\alias{par2ppfd}
\title{par2ppfd}
\usage{
  par2ppfd(watts)
}
\arguments{
  \item{PAR}{(W / m2)}
}
\value{
  PPFD (mol / m2 / s)
}
\description{
  convert PAR to PPFD
}
\details{
  Converts photosynthetically active radiation (PAR, units
  of Watts / m2) to photosynthetic photon flux density
  (PPFD) in units of mol / m2 / s From Campbell and Norman
  p151 PPFD = PAR * (J/m2/s) * (1 mol / 2.35e5 J) 2.35e5 J
  / mol is the energy content of solar radiation in the PAR
  waveband 0.486 is based on the approximation that PAR is
  0.45-0.50 of the total radiation
}
\author{
  David LeBauer

  David LeBauer
}

