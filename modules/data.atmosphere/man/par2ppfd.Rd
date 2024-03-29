% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metutils.R
\name{par2ppfd}
\alias{par2ppfd}
\title{par2ppfd}
\usage{
par2ppfd(watts)
}
\arguments{
\item{watts}{PAR (W / m2)}
}
\value{
PPFD (umol / m2 / s)
}
\description{
convert PAR to PPFD
}
\details{
Converts photosynthetically active radiation (PAR, units of Watts / m2) to
photosynthetic photon flux density (PPFD) in units of umol / m2 / s
From Campbell and Norman p151
PPFD = PAR * (J/m2/s) * (1 mol / 2.35e5 J)
2.35e5 J / mol is the energy content of solar radiation in the PAR waveband
}
\author{
David LeBauer
}
