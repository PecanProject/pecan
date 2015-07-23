\name{download.NLCD}
\alias{download.NLCD}
\title{download.NLCD}
\usage{
download.NLCD(outdir, year = 2011, con = NULL)
}
\arguments{
  \item{outdir}{Directory to download NLCD to}

  \item{year}{which NLCD year to download}

  \item{con}{Optional database connection. If specified
  then the code will check to see if the file already
  exists in PEcAn before downloading, and will also create
  a database entry for new downloads}
}
\description{
Downloads and unzips the National Land Cover Database
http://www.mrlc.gov/nlcd2011.php. Will automatically insert
into PEcAn database if database connection provided.
}
\author{
Mike Dietze
}

