\name{extract_NLCD}
\alias{extract_NLCD}
\title{extract.NLCD}
\usage{
extract_NLCD(buffer, coords, data_dir = NULL, con = NULL, year = 2011)
}
\arguments{
  \item{buffer}{search radius (meters)}

  \item{coords}{data frame containing elements 'long' and
  'lat'. Currently just supports single point extraction.}

  \item{data_dir}{directory where input data is located.
  Can be NUL if con is specified}

  \item{con}{connection to PEcAn database. Can be NULL if
  data_dir is specified}
}
\value{
dataframe of fractional cover of different cover classes
}
\description{
Based on codes from Christy Rollinson and from Max Joseph
(http://mbjoseph.github.io/2014/11/08/nlcd.html)
}
\author{
Mike Dietze
}

