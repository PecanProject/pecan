##' Currently assumes an exponential spatial dependency
##'
##' Can make gaussian by passing squared distance matrix
##'
##' @param d spatial distance matrix
##' @param psi spatial corr
##' @param tau spatial var
##' @param ... additional arguments (currently unused)
##' @return spatial covariance matrix
##' @author Michael Dietze
##' @export
calcSpatialCov.matrix <- function(d, psi, tau, ...) {
  return(tau * exp(-psi * d))
}
