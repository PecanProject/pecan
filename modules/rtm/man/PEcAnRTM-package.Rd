\name{PEcAnRTM}
\docType{package}
\title{
Radiative transfer models for inversion and simulation
}
\description{
This package includes fast, easy-to-use implementations of various radiative transfer models,
as well as methods for their inversion. A novel feature of this package is the ability to
perform Bayesian inversions of RTMs.
}
\details{
\tabular{ll}{
Package: \tab bayes_inversion\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-19\cr
License: \tab GPL (>= 2)\cr
}
All basic RTMs compiled with the package can be called via R functions (see documentation).
Bayesian inversions can similarly be called from R. All underlying numerical analyses are
optimized for speed using Rcpp.
}
\author{
Alexey Shiklomanov, Shawn Serbin, Michael Dietze

Maintainer: Alexey Shiklomanov <ashiklom@bu.edu>
}
\references{
Jacquemoud and Feret (1990)
etc.
}
\keyword{ package }
\examples{
## Perform Bayesian inversion using test spectra
data(testspec)
ngibbs = 1000
tt <- system.time(z2 <- invert_prospect(testspec_ACRU[,1], ngibbs))
print(tt)

## Burnin and thin
Thin = 2
bt <- seq(ngibbs/2, ngibbs, by=Thin)
z <- z2[bt,]

## MCMC Trace plots
par(mfrow=c(3,2))
plot(z2[,1], type='l')
plot(z2[,2], type='l')
plot(z2[,3], type='l')
plot(z2[,4], type='l')
plot(z2[,5], type='l')

## Validation via comparison with original spectrum
plot(prospect4(mean(z[,1]), mean(z[,2]), mean(z[,3]), mean(z[,4])), type='l')
lines(testspec_ACRU[,1], col=2)
}
