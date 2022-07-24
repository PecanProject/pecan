##' @name calcSpatialCov
##' @title calcSpatialCov 
##' @export
##'
##' @param x Name of variable to plot on X axis
##' @param ... Adiitional arguments
##' 
##' @author Michael Dietze
calcSpatialCov <- function(x, ...) UseMethod("calcSpatialCov", x)
