\name{sendmail}
\alias{sendmail}
\title{Clear EBI-CLUSTER worker node local scratch directories of old PEcAn output}
\usage{
  sendmail(from, to, subject, body)
}
\arguments{
  \item{from}{the sender of the mail message}

  \item{to}{the receipient of the mail message}

  \item{subject}{the subject of the mail message}

  \item{body}{the body of the mail message}
}
\value{
  nothing
}
\description{
  Sends email. This assumes the program sendmail is
  installed.
}
\examples{
\dontrun{
sendmail("bob
}
\author{
  Rob Kooper
}

