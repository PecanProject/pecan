% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_local_CMIP5.R
\name{extract.local.CMIP5}
\alias{extract.local.CMIP5}
\title{Extract NLDAS from local download
Extract NLDAS meteorology for a point from a local download of the full grid}
\usage{
extract.local.CMIP5(
  outfolder,
  in.path,
  start_date,
  end_date,
  lat.in,
  lon.in,
  model,
  scenario,
  ensemble_member = "r1i1p1",
  date.origin = NULL,
  adjust.pr = 1,
  overwrite = FALSE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{outfolder}{- directory where output files will be stored}

\item{in.path}{- path to the raw full grids}

\item{start_date}{- first day for which you want to extract met (yyyy-mm-dd)}

\item{end_date}{- last day for which you want to extract met (yyyy-mm-dd)}

\item{lat.in}{site latitude in decimal degrees}

\item{lon.in}{site longitude in decimal degrees}

\item{model}{which GCM to extract data from}

\item{scenario}{which experiment to pull (p1000, historical, ...)}

\item{ensemble_member}{which CMIP5 experiment ensemble member}

\item{date.origin}{(optional) specify the date of origin for timestamps in the files being read.
If NULL defaults to 1850 for historical simulations (except MPI-ESM-P) and 
850 for p1000 simulations (plus MPI-ESM-P historical).  Format: YYYY-MM-DD}

\item{adjust.pr}{- adjustment factor fore precipitation when the extracted values seem off}

\item{overwrite}{logical. Download a fresh version even if a local file with the same name already exists?}

\item{verbose}{logical. to control printing of debug info}

\item{...}{Other arguments, currently ignored}
}
\description{
This function extracts CMIP5 data from grids that have been downloaded and stored locally.
             Files are saved as a netCDF file in CF conventions at *DAILY* resolution.  Note: At this point
             in time, variables that are only available at a native monthly resolution will be repeated to
             give a pseudo-daily record (and can get dealt with in the downscaling workflow).  These files 
             are ready to be used in the general PEcAn workflow or fed into the downscaling workflow.
}
\author{
Christy Rollinson
}
