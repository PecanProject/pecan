\name{browndog.met}
\alias{browndog.met}
\title{get met data from browndog}
\usage{
browndog.met(browndog, source, site, start_date, end_date, model, dir)
}
\arguments{
  \item{browndog,}{list with url, username and password to
  connect to browndog}

  \item{source,}{the source of the met data, currently only
  NARR an Ameriflux is supported}

  \item{site,}{site information should have id, lat, lon
  and name (ameriflux id)}

  \item{start_date,}{start date for result}

  \item{end_date,}{end date for result}

  \item{model,}{model to convert the met data to}

  \item{dir,}{folder where results are stored (in
  subfolder)}
}
\description{
Use browndog to get the met data for a specific model
}
\author{
Rob Kooper
}

