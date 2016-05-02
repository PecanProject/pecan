\name{met.process}
\alias{met.process}
\title{met.process}
\usage{
met.process(site, input_met, start_date, end_date, model, host, dbparms, dir,
  browndog = NULL)
}
\arguments{
  \item{site}{Site info from settings file}

  \item{input}{currently "NARR" or "Ameriflux"}

  \item{start_date}{the start date of the data to be
  downloaded (will only use the year part of the date)}

  \item{end_date}{the end date of the data to be downloaded
  (will only use the year part of the date)}

  \item{model}{model_type name}

  \item{host}{Host info from settings file}

  \item{dbparms}{database settings from settings file}

  \item{dir}{directory to write outputs to}
}
\description{
met.process
}
\author{
Elizabeth Cowdery, Michael Dietze
}

