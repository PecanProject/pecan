##' Currently assumes an exponential spatial dependency
##' 
##' can make gaussian by passing squared distance matrix
##' 
##' @name calcSpatialCov.matrix
##' @title calcSpatialCov.matrix 
##' @export
##'
##' @param d spatial distance matrix
##' @param psi spatial corr
##' @param tau spatial var
##' 
##' @author Michael Dietze
`calcSpatialCov.matrix` <-
function(d,psi,tau){
  nl <- nrow(d)
  H <- matrix(0,nl,nl)
  for(i in 1:nl){
#    for(j in 1:nl){
#      H[i,j] <- tau*exp(-psi*d[i,j])
#    }
    for(j in 1:nl){
      H[i,] <- tau*exp(-psi*d[i,])
    }
  }
  return(H)
}

