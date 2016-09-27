\name{download.AmerifluxLBL}
\alias{download.AmerifluxLBL}
\title{download.AmerifluxLBL}
\usage{
download.AmerifluxLBL(sitename, outfolder, start_date, end_date,
  overwrite = FALSE, verbose = FALSE, username = "pecan", ...)
}
\arguments{
  \item{site}{the Ameriflux ID of the site to be
  downloaded, used as file name prefix. The "SITE_ID" field
  in
  \href{http://ameriflux.lbl.gov/sites/site-list-and-pages/}{list
  of Ameriflux sites}}

  \item{outfolder}{location on disk where outputs will be
  stored}

  \item{start_date}{the start date of the data to be
  downloaded. Format is YYYY-MM-DD (will only use the year
  part of the date)}

  \item{end_date}{the end date of the data to be
  downloaded. Format is YYYY-MM-DD (will only use the year
  part of the date)}

  \item{overwrite}{should existing files be overwritten}

  \item{verbose}{should the function be very verbose}
}
\description{
Download Ameriflux LBL CSV files
}
\author{
Ankur Desai, based on download.Ameriflux.R by Josh
Mantooth, Rob Kooper
}

