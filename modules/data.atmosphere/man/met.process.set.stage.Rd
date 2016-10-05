\name{met.process.set.stage}
\alias{met.process.set.stage}
\title{met.process.set.stage}
\usage{
met.process.set.stage(info.list, start_date, end_date, con)

met.process.set.stage(info.list, start_date, end_date, con)
}
\arguments{
  \item{input.info.list}{,list containing DB record ids,
  start-dates and end_dates}

  \item{start_date}{, Start date as given by run}

  \item{end_date}{, End date as given bu run}

  \item{con,}{database connection}

  \item{info.list}{object passed from met.files.tree
  function containing existing file information}

  \item{start_date}{object containing start date of current
  model run attempts}

  \item{met.source}{object containing end date of the
  current model run attempt}
}
\description{
met.process.set.stage

met.process.stage

Function to set up stage for PEcAn met processing and set
"update" or "new" routine. It compares run start/end dates
to those that exist in the input records of the BETY
database, returning a list object that sets up met
processing stages and gives dates that need to be updated
in the database and those that need to be downloaded.
}
\author{
Tony Gardella, Elizabeth Cowdery

Tony Gardella
}

