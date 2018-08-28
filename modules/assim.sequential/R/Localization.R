##' @title Local.support
##' @name  Local.support
##' @author Hamze Dokoohaki
##' 
##' @param Pf Forecast error coveriance matrix
##' @description 
##' @return It returns a localized covariance matrix by taking a Schur product between Pf and a corrolation function
##' @export

Local.support <-function(Pf){
  #making a matrix as the size of the Pf
  rho <-matrix(1,dim(Pf)[1],dim(Pf)[1])
  
  return(Pf*rho)
}