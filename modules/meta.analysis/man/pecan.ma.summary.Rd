\name{pecan.ma.summary}
\alias{pecan.ma.summary}
\title{Generate summary statitics and diagnostics for PEcAn meta.analysis}
\usage{
pecan.ma.summary(mcmc.object, pft, outdir, threshold = 1.2, ggmcmc = FALSE)
}
\arguments{
  \item{mcmc.object}{JAGS mcmc output from
  \code{\link{pecan.ma}}}

  \item{pft}{plant functional type}

  \item{outdir}{output directory}

  \item{threshold}{Gelman-Rubin convergence threshold;
  default = 1.2 following Bolker 2008 Ecological Models and
  Data in R}

  \item{ggmcmc}{plot ggmcmc plots (this is slow)? default
  is FALSE}
}
\description{

}
\examples{
\dontrun{
summary <- pecan.ma.summary(trait.mcmc,settings$pfts$pft,settings$outdir,settings$meta.analysis$threshold)
}
}
\author{
David LeBauer, Shawn Serbin
}

