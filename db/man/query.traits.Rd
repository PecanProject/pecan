\name{query.traits}
\alias{query.traits}
\title{Query trait data}
\usage{
  query.traits(spstr, priors, con = NULL)
}
\arguments{
  \item{spstr}{string of species id's from BETYdb database}

  \item{priors}{vector of parameters for which priors have
  been specified}

  \item{con}{}
}
\value{
  dataframe with trait data
}
\description{
  Query available trait data associated with a given pft
  and a list of traits
}
\examples{
\dontrun{
spstr <- query.pft_species('ebifarm.c4crop')$spstr
trvec <- c('leafN', 'SLA')
trait.data <- query.bety.traits(spstr, trvec)
}
}
\seealso{
  \code{\link{query.trait.data}}
}

