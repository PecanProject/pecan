##' @name calcSpatialCov
##' @title calcSpatialCov 
##' @export
##'
##' @param x either a spatial distance matrix or a list of component spatial distance matrices
##' @param ... Additional arguments
##' 
##' @author Michael Dietze
calcSpatialCov <- function(x, ...) UseMethod("calcSpatialCov", x)
