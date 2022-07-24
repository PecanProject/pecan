##' Log-dinvgamma, based on MCMCpack
##' 
##' @name ldinvgamma
##' @title ldinvgamma
##' @export
##'
##' @param x Name of variable to plot on X axis
##' @param shape shape of points
##' @param scale scale, NULL by default
##' 
##' @return log.density
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

