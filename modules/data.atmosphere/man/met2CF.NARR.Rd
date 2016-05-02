\name{met2CF.NARR}
\alias{met2CF.NARR}
\title{met2CF.NARR}
\usage{
met2CF.NARR(in.path, in.prefix, outfolder, start_date, end_date,
  overwrite = FALSE, verbose = FALSE, ...)
}
\arguments{
  \item{in.path}{}

  \item{in.prefix}{}

  \item{outfolder}{}

  \item{start_date}{the start date of the data to be
  downloaded (will only use the year part of the date)}

  \item{end_date}{the end date of the data to be downloaded
  (will only use the year part of the date)}

  \item{overwrite}{should existing files be overwritten}

  \item{verbose}{should ouput of function be extra verbose}
}
\description{
Convert NARR files to CF files
}
\author{
Elizabeth Cowdery, Rob Kooper
}

