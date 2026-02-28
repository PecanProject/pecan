##' calcSpatialCov
##' @export
##'
##' @param d either a spatial distance matrix or a list of component spatial distance matrices
##' @param psi spatial corr
##' @param tau spatial var
##' @param ... Additional arguments
##'
##' @author Michael Dietze
calcSpatialCov <- function(d, psi, tau, ...) UseMethod("calcSpatialCov", d)
