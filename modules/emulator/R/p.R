##' p
##' @export
##' 
##' @param x jump distribution
##' @param ... Additional arguments
##' 
p <- function(x, ...) UseMethod("p", x)
