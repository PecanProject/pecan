\name{dbfile.input.insert}
\alias{dbfile.input.insert}
\title{Insert file into tables}
\usage{
dbfile.input.insert(filename, siteid, startdate, enddate, mimetype, formatname,
  con, hostname = Sys.info()[["nodename"]])
}
\arguments{
  \item{filename}{the name of the file to be inserted}

  \item{siteid}{the id of the site that this data is
  applicable to}

  \item{startdate}{the start date of the data stored in the
  file}

  \item{enddate}{the end date of the data stored in the
  file}

  \item{mimetype}{the mime-type of the file}

  \item{formatname}{the name of the format to distinguish
  between simmilair mime-types}

  \item{con}{database connection object}

  \item{hostname}{the name of the host where the file is
  stored, this will default to the name of the current
  machine}

  \item{params}{database connection information}
}
\value{
data.frame with the id, filename and pathname of the input
that is requested
}
\description{
Function to insert a file into the dbfiles table as an
input
}
\details{
This will write into the dbfiles, inputs, machines and
formats the required data to store the file
}
\examples{
\dontrun{
  dbfile.input.insert('trait.data.Rdata', siteid, startdate, enddate, 'application/x-RData', 'traits', dbcon)
}
}
\author{
Rob Kooper
}

