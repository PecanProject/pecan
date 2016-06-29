\name{permute.nc}
\alias{permute.nc}
\title{permute.nc}
\usage{
permute.nc(in.path, in.prefix, out.path, start_date, end_date,
  overwrite = FALSE, verbose = FALSE, ...)
}
\arguments{
  \item{in.path}{location on disk where inputs are stored}

  \item{in.prefix}{prefix of input and output files}

  \item{outfolder}{location on disk where outputs will be
  stored}

  \item{start_date}{the start date of the data to be
  permuted (will only use the year part of the date)}

  \item{end_date}{the end date of the data to be permuted
  (will only use the year part of the date)}

  \item{overwrite}{should existing files be overwritten}

  \item{verbose}{should ouput of function be extra verbose}
}
\description{
Permute netCDF files
}
\author{
Elizabeth Cowdery, Rob Kooper
}

