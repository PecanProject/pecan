#' @export
adj.ens<-function(Pf,X,X.new,mu.f,mu.a,Pa,processvar){
  S_f  <- svd(Pf)
  L_f  <- S_f$d
  V_f  <- S_f$v
  
  ## normalize
  Z <- X*0
  
  for(i in seq_len(nrow(X))){
    if(processvar == TRUE) {
      Z[i,] <- 1/sqrt(L_f) * t(V_f)%*%(X.new[i,]-mu.f)
    }else{
      Z[i,] <- 1/sqrt(L_f) * t(V_f)%*%(X[i,]-mu.f)
    }
  }
  Z[is.na(Z)]<-0
  
  ## analysis
  S_a  <- svd(Pa)
  
  L_a  <- S_a$d
  V_a  <- S_a$v
  
  ## analysis ensemble
  X_a <- X*0
  for(i in seq_len(nrow(X))){
    # she decomposed Pa - then it's putting it back together but with a different Z which comes from the liklihood of that ens
    X_a[i,] <- V_a %*%diag(sqrt(L_a))%*%Z[i,] + mu.a
  }
  
  # # calculate likelihoods
  #      for(i in seq_len(nens)){
  #        wt.mat[i,t]<-dmnorm_chol(FORECAST[[t]][i,], mu.a, solve(Pa), log = TRUE)
  #      }
  
  if(sum(mu.a - colMeans(X_a)) > 1 | sum(mu.a - colMeans(X_a)) < -1) logger.warn('Problem with ensemble adjustment (1)')
  if(sum(diag(Pa) - diag(cov(X_a))) > 5 | sum(diag(Pa) - diag(cov(X_a))) < -5) logger.warn('Problem with ensemble adjustment (2)')
  
  analysis <- as.data.frame(X_a)

  return(analysis)
}