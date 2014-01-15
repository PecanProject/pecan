\name{dbfile.insert}
\alias{dbfile.insert}
\title{Insert file into tables}
\usage{
dbfile.insert(filename, type, id, con, hostname = Sys.info()[["nodename"]])
}
\arguments{
  \item{filename}{the name of the file to be inserted}

  \item{con}{database connection object}

  \item{hostname}{the name of the host where the file is
  stored, this will default to the name of the current
  machine}

  \item{params}{database connection information}
}
\value{
data.frame with the id, filename and pathname of the file
that is written
}
\description{
Function to insert a file into the dbfiles table
}
\details{
This will write into the dbfiles and machines the required
data to store the file
}
\examples{
\dontrun{
  dbfile.insert('somefile.txt', 'Input', 7, dbcon)
}
}
\author{
Rob Kooper
}

