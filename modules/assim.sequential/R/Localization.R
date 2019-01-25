##' @title Local.support
##' @name  Local.support
##' @author Hamze Dokoohaki
##' 
##' @param Pf Forecast error coveriance matrix
##' @param distance.mat is matrix of distances between sites.
##' @param scalef scalef is a numeric value that requires tunning and it controls the shape of the corrolation function
##' @description 
##' distance.mat matrix doesn't need to be just the physical distance, however it represent a measure of similarity between state variables in different sites.
##' @return It returns a localized covariance matrix by taking a Schur product between Pf and a corrolation function
##' @export

Local.support <-function(Pf, distance.mat, scalef=1){
  #making a matrix as the size of the Pf
  rho <- exp((-1*distance.mat^2)/(2*scalef^2))
  
  return(Pf*rho)
}


##' @title simple.local
##' @name  simple.local
##' @author Hamze Dokoohaki
##' 
##' @param Pf Forecast error coveriance matrix
##' @param distance.mat is matrix of distances between sites.
##' @param scalef scalef is a numeric value that requires tunning and it controls the shape of the corrolation function
##' @description Adopted from Data assimilation for spatio-temporal processes - p250 - Sebastian Reich
##' @return It returns a localized covariance matrix by taking a Schur product between Pf and a corrolation function
##' @export
simple.local <-function(Pf, distance.mat, scalef=2){
  ### Data assimilation for spatio-temporal processes - p250 - Sebastian Reich
  s <- distance.mat/rloc
  s[s <2] <- 1- ((0.5)*(s[s <2]))
  s[s>2] <- 0
  return(s*Pf)
}



##' @title piecew.poly.local
##' @name  piecew.poly.local
##' @author Hamze Dokoohaki
##' 
##' @param Pf Forecast error coveriance matrix
##' @param distance.mat is matrix of distances between sites.
##' @param scalef scalef is a numeric value that requires tunning and it controls the shape of the corrolation function
##' @description 5th order piecewise polynomial adopted from Data assimilation for spatio-temporal processes - p250 - Sebastian Reich
##' 
##' @return It returns a localized covariance matrix by taking a Schur product between Pf and a corrolation function
##' @export
piecew.poly.local <-function(Pf, distance.mat, scalef=2){
  ### Data assimilation for spatio-temporal processes - p250 - Sebastian Reich
  s <- distance.mat/rloc
  
  s[s < 1] <- 1 - ((5/3)*(s[s < 1])^2)+((5/8)*(s[s < 1])^3) + (0.5*(s[s < 1])^4)-(0.25*(s[s < 1])^5)
  
  s[s <=2 & s >1] <- (-2/3)*(s[s <=2 & s >1])^(-1)+4-(5*(s[s <=2 & s >1]))+((5/3)*(s[s <=2 & s >1])^2)+((5/8)*(s[s <=2 & s >1])^3)-(0.5*(s[s <=2 & s >1])^4)+((1/12)*(s[s <=2 & s >1])^5)
  s[s>2] <- 0
  
  return(s*Pf)
}