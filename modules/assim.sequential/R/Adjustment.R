##' @title adj.ens
##' @name  adj.ens
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho and Hamze Dokoohaki
##' 
##' @param Pf  A cov matrix of forecast state variables.  
##' @param X   Dataframe or matrix of forecast state variables for different ensembles.
##' @param mu.f A vector with forecast mean estimates of state variables.
##' @param mu.a A vector with analysis mean estimates of state variables.
##' @param Pa The state estimate cov matrix of analysis.
##’ @details
##’  
##' @description This functions gives weights to different ensemble members based on their likelihood during the analysis step. Then it adjusts the analysis mean estimates of state variables based on the estimated weights.
##' 
##' @return Returns a vector of adjusted analysis mean estimates of state variables.
##' @export

adj.ens<-function(Pf, X, mu.f, mu.a, Pa){

  S_f  <- svd(Pf)
  L_f  <- S_f$d
  V_f  <- S_f$v
  
  ## normalize
  Z <- X*0
  
  for(i in seq_len(nrow(X))){

    Z[i,] <- 1/sqrt(L_f) * t(V_f)%*%(X[i,]-mu.f)

  }
  Z[is.na(Z)]<-0
  Z[is.infinite(Z)] <- 0
  
  ## analysis
  S_a  <- svd(Pa)
  
  L_a  <- S_a$d
  V_a  <- S_a$v
  
  ## analysis ensemble 
  X_a <- X*0
  for(i in seq_len(nrow(X))){
    # she decomposed Pa - then it's putting it back together but with a different Z which comes from the likelihood of that ens    
    X_a[i,] <- V_a %*%diag(sqrt(L_a))%*%Z[i,] + mu.a
  }
  
  if (sum(mu.a - colMeans(X_a)) > 1
      || sum(mu.a - colMeans(X_a)) < -1) { 
    PEcAn.logger::logger.warn('Problem with ensemble adjustment (1)')
  }
  if (sum(diag(Pa) - diag(stats::cov(X_a))) > 5 
      || sum(diag(Pa) - diag(stats::cov(X_a))) < -5) {
    PEcAn.logger::logger.warn('Problem with ensemble adjustment (2)')
  }
  analysis <- as.data.frame(X_a)
  
  return(analysis)
}