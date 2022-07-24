##' @name p
##' @title p
##' @export
##' 
##' @param x Name of variable to plot on X axis
##' @param ... Additional arguments
##' 
p <- function(x, ...) UseMethod("p", x)
