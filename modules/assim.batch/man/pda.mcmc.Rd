\name{pda.mcmc}
\alias{pda.mcmc}
\title{Paramater Data Assimilation using MCMC}
\usage{
pda.mcmc(settings, params = NULL, jvar = NULL, var.names = NULL,
  prior = NULL, chain = NULL, adapt = NULL, adj.min = NULL,
  ar.target = NULL)
}
\arguments{
  \item{settings}{= a pecan settings list}
}
\value{
nothing. Diagnostic plots, MCMC samples, and posterior
distributions are saved as files and db records.
}
\description{
Paramater Data Assimilation using MCMC
}
\details{
Brute-force, only to be used on simple models
}
\author{
Mike Dietze

Ryan Kelly
}

