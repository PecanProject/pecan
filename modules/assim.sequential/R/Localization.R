##' @title Local.support
##' @name  Local.support
##' @author Hamze Dokoohaki
##' 
##' @param Pf Forecast error coveriance matrix
##' @param distance.mat is matrix of distances between state variables and sites with the same dimension of Pf.
##' @param scalef scalef is a numeric value that requires tunning and it controls the shape of the corrolation function
##' @description 
##' distance.mat matrix doesn't need to be just the physical distance, however it represent a measure of similarity between state variables in different sites.
##' @return It returns a localized covariance matrix by taking a Schur product between Pf and a corrolation function
##' @export

Local.support <-function(Pf, distance.mat, scalef=1){
  # they need to be with the same dimension because there is gonna be an Schur product which is elementwise
  if (!all(dim(Pf) == dim(distance.mat))) PEcAn.logger::logger.severe("The dimensions of the Pf and the distance matrix needs to be the same.")
  
  #making a matrix as the size of the Pf
  rho <- exp((-1*distance.mat^2)/(2*scalef^2))
  
  return(Pf*rho)
}