\name{xmlMerge}
\alias{xmlMerge}
\title{xmlMerge}
\usage{
  xmlMerge(xml1, xml2)
}
\arguments{
  \item{xml1}{first xml list}

  \item{xml2}{second xml list}
}
\value{
  xml object with
}
\description{
  merge 2 xml documents
}
\details{
  combines content from xml1 and xml2. In the case of a
  conflict (both xml1 and xml2 have the same node), node in
  xml2 replaces node in xml1
}
\author{
  Rob Kooper
}

