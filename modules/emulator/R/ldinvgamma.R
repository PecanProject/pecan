##' Log-dinvgamma, based on MCMCpack
##' 
##' @export
##'
##' @param x vector of quantiles
##' @param shape,scale shape and scale parameters for the inverse Gamma distribution
##' 
##' @return log density of the inverse Gamma distribution evaluated at \code{x}
##' 
##' @author Michael Dietze
ldinvgamma <- function(x, shape, scale = 1) {
    if (shape <= 0 | scale <= 0) {
        stop("Shape or scale parameter negative in dinvgamma().\n")
    }
    alpha <- shape
    beta <- scale
    return(alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(x) - (beta / x))
} # ldinvgamma

