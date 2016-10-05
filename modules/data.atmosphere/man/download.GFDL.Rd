\name{download.GFDL}
\alias{download.GFDL}
\title{download.GFDL}
\usage{
download.GFDL(outfolder, start_date, end_date, site_id, lat.in, lon.in,
  overwrite = FALSE, verbose = FALSE, model = "CM3", scenario = "rcp45",
  ensemble_member = "r1i1p1", ...)
}
\arguments{
  \item{outfolder}{}

<<<<<<< HEAD
  \item{start_date}{}

  \item{end_date}{}

  \item{lat}{}

  \item{lon}{}

  \item{model}{, select which GFDL model to run (options
  are CM3, ESM2M, ESM2G)}

  \item{scenario}{, select which scenario to run (options
  are rcp26, rcp45, rcp60, rcp85)}

  \item{ensemble_member}{, select which ensemble_member to
  initialize the run (options are r1i1p1, r3i1p1, r5i1p1)}
=======
\item{scenario}{, select which scenario to run (options are rcp26, rcp45, rcp60, rcp85)}

\item{ensemble_member}{, select which ensemble_member to initialize the run (options are r1i1p1, r3i1p1, r5i1p1)}
>>>>>>> master
}
\description{
Download GFDL CMIP5 outputs for a single grid point using
OPeNDAP and convert to CF
}
\author{
James Simkins
}

