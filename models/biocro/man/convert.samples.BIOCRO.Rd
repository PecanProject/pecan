\name{convert.samples.BIOCRO}
\alias{convert.samples.BIOCRO}
\title{Convert samples for biocro}
\usage{
  convert.samples.BIOCRO(trait.samples)
}
\arguments{
  \item{trait.samples}{a matrix or dataframe of samples
  from the trait distribution}
}
\value{
  matrix or dataframe with values transformed
}
\description{
  convert parameters from PEcAn database default units to
  biocro defaults
}
\details{
  Performs model specific unit conversions on a a list of
  trait values, such as those provided to write.config
}
\author{
  David LeBauer
}

