\name{get.ncepmet}
\alias{get.ncepmet}
\title{get NCEP met}
\usage{
  get.ncepmet(lat = as.numeric(settings$run$site$lat),
    lon = as.numeric(settings$run$site$lon),
    start.date = settings$run$start.date,
    end.date = settings$run$end.date,
    site.id = settings$run$site$id,
    con = query.base.con(settings))
}
\arguments{
  \item{lat}{}

  \item{lon}{}

  \item{start.date}{}

  \item{end.date}{}

  \item{site.id}{}

  \item{con}{}
}
\value{
  weather data frame with weather formatted for BioCro
  model
}
\description{
  Get Met data from NCEP
}
\details{
  Retrieves NCEP met data for specified lat x lon and time
  span
}
\author{
  David LeBauer
}

