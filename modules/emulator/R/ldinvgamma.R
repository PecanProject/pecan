##' Log-dinvgamma, based on MCMCpack
##' 
##' @name ldinvgamma
##' @title ldinvgamma
##' @export
##'
##' @param x
##' @param shape
##' @param scale
##' 
##' @return log.density
##' 
##' @author Michael Dietze
`ldinvgamma` <-
function (x, shape, scale = 1)
{
    if (shape <= 0 | scale <= 0) {
        stop("Shape or scale parameter negative in dinvgamma().\n")
    }
    alpha <- shape
    beta <- scale
    log.density <- alpha * log(beta) - lgamma(alpha) - (alpha + 
        1) * log(x) - (beta/x)
    return(log.density)
}

