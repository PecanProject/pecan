\name{run.meta.analysis}
\alias{run.meta.analysis}
\title{Invoke PEcAn meta.analysis
This will use the following items from setings:
- settings$pfts
- settings$database
- settings$run$dbfiles
- settings$meta.analysis$update}
\usage{
run.meta.analysis(pfts, iterations, dbfiles, database)
}
\arguments{
  \item{pfts}{the list of pfts to get traits for}

  \item{iterations}{the number of iterations for the mcmc
  analysis}

  \item{dbfiles}{location where previous results are found}

  \item{database}{database connection parameters}
}
\value{
nothing, as side effect saves \code{trait.mcmc} created by
\code{\link{pecan.ma}} and post.distns created by
\code{\link{approx.posterior(trait.mcmc, ...)}} to
trait.mcmc.Rdata \ and post.distns.Rdata, respectively
}
\description{
Run meta analysis
}
\author{
Shawn Serbin, David LeBauer
}

