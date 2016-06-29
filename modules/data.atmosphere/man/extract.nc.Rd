\name{extract.nc}
\alias{extract.nc}
\title{extract.nc}
\usage{
extract.nc(in.path, in.prefix, outfolder, start_date, end_date, slat, slon,
  overwrite = FALSE, verbose = FALSE, ...)
}
\arguments{
  \item{in.path}{location on disk where inputs are stored}

  \item{in.prefix}{prefix of input files}

  \item{outfolder}{location on disk where outputs will be
  stored}

  \item{start_date}{the start date of the data to be
  permuted (will only use the year part of the date)}

  \item{end_date}{the end date of the data to be permuted
  (will only use the year part of the date)}

  \item{slat}{the latitude of the site}

  \item{slon}{the longitude of the site}

  \item{overwrite}{should existing files be overwritten}

  \item{verbose}{should ouput of function be extra verbose}
}
\description{
Given latitude and longitude coordinates, extract site data
from NARR file
}
\author{
Betsy Cowdery
}

