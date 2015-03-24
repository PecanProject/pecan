##' @name invert_prospect
##' @title Basic Bayesian inversion of PROSPECT4 model
##' @author Alexey Shiklomanov
##' @param spectra A matrix of reflectance spectra with dimensions 2101 x n,
##'     where n is the number of spectra. Vectors are coerced to matrices.
##' @param ngibbs The number of MCMC steps for the iteration. Default is 15000.
##'     Convergence for good, individual spectra happens around 5000 steps.
##' @param adapt How often to adapt the Jump distribution, in MCMC steps. Default = 25
##' @param min_adapt Minimum value to adapt by. This prevents the Jump distribution variance
##'     from going to zero when no values are accepted. Default is 0.05.
##' @return Matrix of samples of each PROSPECT parameter and residual variance
##' @examples
##' data(testspec)
##' samples <- invert_prospect(testspec_ACRU[,1], 1000)
##' par(mfrow=c(2,2))
##' plot(samples[,1], type='l', ylab="N")
##' plot(samples[,2], type='l', ylab="Cab")
##' plot(samples[,3], type='l', ylab="Cw")
##' plot(samples[,4], type='l', ylab="Cm")

invert_prospect <- function(spectra,
                             ngibbs = 15000,
                             adapt = 25,
                             min_adapt = 0.05){
    data(prospect4)
    out <- pinvbayes(ngibbs, as.matrix(spectra), adapt, min_adapt, P4data)
    colnames(out) <- c("N", "Cab", "Cw", "Cm", "rsd")
    return(out)
}

##' @name invert_prospect_re
##' @title Bayesian inversion of PROSPECT4 model with random effects
##' @author Alexey Shiklomanov
##' @param spectra A matrix of reflectance spectra with dimensions 2101 x n,
##'     where n is the number of spectra. Vectors are coerced to matrices.
##' @param ngibbs The number of MCMC steps for the iteration. Default is 15000.
##'     Convergence for good, individual spectra happens around 5000 steps.
##' @param adapt How often to adapt the Jump distribution, in MCMC steps. Default = 25
##' @param min_adapt Minimum value to adapt by. This prevents the Jump distribution variance
##'     from going to zero when no values are accepted. Default is 0.05.
##' @return Matrix of samples of each PROSPECT parameter, residual variance, and
##'     random effects variance for each parameter, and random effects values
##'     for each leaf.
##' @examples
##' data(testspec)
##' samples <- invert_prospect_re(testspec_ACRU[,1:5], 1000)
##' dim(samples)
##' par(mfrow=c(2,2))
##' plot(samples[,1], type='l', ylab="N")
##' plot(samples[,2], type='l', ylab="Cab")
##' plot(samples[,3], type='l', ylab="Cw")
##' plot(samples[,4], type='l', ylab="Cm")
invert_prospect_re <- function(spectra,
							   ngibbs = 15000,
							   adapt = 25,
							   min_adapt = 0.05){
	data(prospect4)
	out <- pinvbayes_re(ngibbs, as.matrix(spectra), adapt, min_adapt, P4data)
	return(out)
}

##' @name invert_prospect_MLE
##' @title Simple maximum likelihood inversion of PROSPECT4
##' @details MLE inversion based on log of sum of squares of error.
##' @author Alexey Shiklomanov
##' @param spectra A matrix of reflectance spectra with dimensions 2101 x n,
##'     where n is the number of spectra.
##' @return Vector of maximum likelihood values of PROSPECT parameters
##' @examples
##' data(testspec)
##' fit <- invert_prospect_MLE(testspec_ACRU[,1])
##' plot(400:2100, prospect4([fit[1], fit[2], fit[3], fit[4]),
##'     type='l',
##'     xlab="Wavelength (nm)",
##'     ylab="Reflectance")
##' lines(400:2100, testspec_ACRU[,1], col=2)

invert_prospect_MLE <- function(spectra){
	merit <- function(pars){
		spec <- prospect4(pars[1], pars[2], pars[3], pars[4])
		return(log(sum((spec-spectra)^2)))
	}
	fit <- optim(c(1.4, 30, 0.01, 0.01, 0.5), merit)
	return(fit$par)
}