\name{mon2mmm}
\alias{mon2mmm}
\title{mon2mmm}
\usage{
  mon2mmm(mon, lang = "English")
}
\arguments{
  \item{mon}{3-letter month abbreviation}

  \item{lang}{charcter, currently supports "english",
  "portugese"}
}
\value{
  month as three letter abbreviation
}
\description{
  Convert numeric month to 3-letter abbrev.
}
\details{
  Function that determines 3-letter name of the month given
  their number.
}
\examples{
mon2mmm(1)
mon2mmm(1:3)
}

