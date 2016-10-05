\name{met.files.tree}
\alias{met.files.tree}
\title{met.files.tree}
\usage{
met.files.tree(site.id, machine.id, met, met.reg, con)
}
\arguments{
  \item{site.id}{object wih site id}

  \item{machine.id}{object with machine id}

  \item{met}{object created in met.process with name of the
  met source (Ameriflux, NARR,CRUNCEP, etc.)}

  \item{met.reg}{object created in met.process; used for
  format id}

  \item{con}{connection object}
}
\description{
Function will check database for matching met files for a
given run dependant on site, machine, and source. It will
return a list containing the start/end dates and id
associated with each input record for each stage of
met.process. If none exists, it will return an empty
object.
}
\author{
Tony Gardella
}

