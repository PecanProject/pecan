##' Currently assumes an exponential spatial dependency
##'
##' Can make gaussian by passing squared distance matrix
##'
##' @name calcSpatialCov.matrix
##' @title calcSpatialCov.matrix
##' @param x spatial distance matrix
##' @param psi spatial corr
##' @param tau spatial var
##' @param ... additional arguments (currently unused)
##' @return spatial covariance matrix
##' @author Michael Dietze
##' @exportS3Method PEcAn.emulator calcSpatialCov
calcSpatialCov.matrix <- function(x, psi, tau, ...) {
  d <- x
  nl <- nrow(d)
  H  <- matrix(0, nl, nl)
  for (i in seq_len(nl)) {
    # for(j in 1:nl){ H[i,j] <- tau*exp(-psi*d[i,j]) }
    for (j in seq_len(nl)) {
      H[i, ] <- tau * exp(-psi * d[i, ])
    }
  }
  return(H)
}
