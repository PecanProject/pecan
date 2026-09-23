tobit <- function(x){
  x[x < 0] <- 0
  x
}


##' @title disturbance data assimilation.
##' @name  disturbance_DA
##' 
##' @param X numeric matrix (dim = M*N, where M is the ensemble number while N is the number of variables): model forecasts for each variable of each ensemble member.
##' @param Y numeric vector (dim = L, the number of observations): vector stores the available observations for the corresponding variables.
##' @param R numeric matrix (dim = L*L): the covariance matrix of the observations.
##' @param H integral vector (dim = L): mapping operator (Y -> X. e.g., c(1,3,4) represents the fact that we have three observations covered the 1st, 2nd, and 4th state variables).
##' @param aqq numeric vector (dim = N): the vector of the `shape` parameter for the gamma distribution for each state variable.
##' @param bqq numeric vector (dim = N): the vector of the `rate` parameter for the gamma distribution for each state variable.
##' @param disturbance logical (dim = 1): if the current site experienced the disturbance (1) or not (0).
##' @param mu0 numeric vector (dim = B where B is the number of biomass variables): percent decreases of biomass to be used for the disturbance model
##' @param V0 numeric matrix (dim B*B): covariance to be used for the disturbance model
##'  
##' @description This function provides semi-analytical updates for the disturbance probability.
##' 
##' @return returns a vector of mua (dim = N, vector of means for each updated state variable) and pa (dim = N*N, covariance matrix for the updated state variables).
##' @importFrom dplyr %>%
##' @export

disturbance_DA <- function (X, Y, R, H, aqq, bqq, disturbance, mu0=rep(80, 80), V0=diag(rep(0.25, 2))) {
  
  # store number of ensemble members, state vars, components
  ne <- nrow(X)
  n.state <- ncol(X)
  n.comp <- 2
  
  # initialize variables
  Nf <- as.matrix(X) # array(NA, dim=c(ne,n.state))
  muF <- array(NA, dim = c(n.comp, n.state))
  CF <- array(NA, dim = c(n.comp, n.state, n.state))
  pF <- array(NA, dim = c(n.comp, n.state, n.state))
  muA <- array(NA, dim = c(n.comp, n.state))
  CA <- array(NA, dim = c(n.comp, n.state, n.state))
  wA <- array(NA, dim = c(n.comp))
  
  # select undisturbed / disturbed forecast
  d.ens.p <- c(0.5, 0.5)
  d.forecast <- rmultinom(ne, 1, d.ens.p)
  d.class <- apply(d.forecast,2,which.max) - 1
  sel.undisturbed <- which(d.class == 0)
  
  # add process error to undisturbed ensemble members
  Q <- array(0, dim = c(n.state, n.state))
  for (i in 1:n.state) {
    Q[i,i] <- 1 / (aqq[i] / bqq[i])
  }
  for(j in sel.undisturbed) {
    Nf[j, ] = mvtnorm::rmvnorm(1, unlist(X[j, ]), Q)
  }
  
  # run disturbance model and compute forecast stats
  sel.dist = which(d.class == 1)
  bio.idxs = c(1, 2) # --- Fill in ---- (indices of biomass state variables)
  for(j in sel.dist){
    Nf[j,] = disturbance.p(Nf[j,], 1-mu0, V0, bio.idxs, 0.25)
  }
  Nf <- tobit(Nf)
  muF[1,] = colMeans(X[sel.undisturbed,])
  CF[1,,] = var(Nf[sel.undisturbed,])
  pF[1,,] = solve(CF[1,,])
  muF[2,] = colMeans(Nf[sel.dist,])
  CF[2,,] = var(Nf[sel.dist,])
  pF[2,,] = try(solve(CF[2,,]))
  
  # default values if disturbed forecast stats fail
  if(grepl("Error", pF[2,1,1])) {
    muF[2,] = colMeans(X)
    muF[2,bio.idxs] = mu0
    cf.diag <- rep(0.25, n.state)
    cf.diag[bio.idxs] <- diag(V0)
    CF[2,,] = diag(cf.diag)
    pF[2,,] = solve(CF[2,,])
  }
  
  # kalman update to compute analysis means, covariances, weights
  n.obs <- length(Y)
  pD <- c(0.9, 0.1) # prior probability of disturbance 0.1, default value which can be tweaked
  H.mat <- array(0, dim=c(n.obs, n.state))
  for (i in 1:n.obs) {
    H.mat[i,H[i]] <- 1
  }
  for (d in 1:2) {
    muf <- muF[d,]
    Cf <- CF[d,,]
    pf <- pF[d,,]
    CA[d,,] <- solve(pf + t(H.mat) %*% solve(R) %*% H.mat)
    muA[d,] <-  CA[d,,] %*% (pf %*% muf + t(H.mat) %*% solve(R) %*% Y)
    lik <- dmvnorm(Y, mean=H.mat %*% muf, sigma = H.mat %*% Cf %*% t(H.mat) + R)
    wA[d] <- lik * pD[d]
  }
  wA <- wA / sum(wA)
  
  # n is the number of ensemble members or observations
  n_samples <- nrow(X) 
  
  # Calculate the squared anomalies/residuals between forecast and expected trajectory
  # (Assuming Nf is your forecast and X is your starting state)
  residuals <- (Nf - X)^2 
  SS_residual <- colSums(residuals) # Sum of squares for each state variable
  
  # Update the Hyperparameters for the next cycle
  aqq.new <- aqq + (n_samples / 2)
  bqq.new <- bqq + (SS_residual / 2)
  
  # resample an analysis ensemble from the posterior
  ana.ens <- array(NA, dim=c(ne, n.state))
  ana.class <- rbinom(ne, size=1, prob=wA) + 1
  for (i in 1:ne) {
    d <- ana.class[i]
    ana.ens[i,] <- rmvnorm(1, mean = muA[d,], sigma = CA[d,,])
  }
  
  # overall mean / covariance of the 2-component mixture
  mu.overall <- colSums(wA %*% muA)                       # weighted mean of component means
  C.overall  <- matrix(0, n.state, n.state)
  for (d in 1:n.comp) {
    dmu <- muA[d,] - mu.overall
    C.overall <- C.overall + wA[d] * (CA[d,,] + tcrossprod(dmu))
  }
  
  # # Instead of a weighted mixture, select the component with the highest weight
  # best_comp <- which.max(wA)
  # 
  # mu.overall <- muA[best_comp, ]
  # C.overall  <- CA[best_comp, , ]
  
  # return values
  return(list(ensemble = ana.ens, means = muA, covs = CA, weights = wA,
              mu.overall = mu.overall, cov.overall = C.overall, aqq = aqq.new, bqq = bqq.new))
}


##' @param x vector of biomass / soil moisture values, dim N (number of state variables)
##' @param mu0 mean biomass % retention after disturbance, dim B (number of biomass state variables)
##' @param V0 covariance in post disturbance leaf and stem biomass, dim BxB
##' @param bio.idxs vector of indices of biomass values in x, dim B
##' @param alloc.soil fraction of removed C that goes in soil

##' @description
##' This function simulates a disturbance on a vector of biomass values.
##' 
##' @return returns an updated vector of state variables (dim N)
disturbance.p <- function(x,mu0,V0,bio.idxs,alloc.soil){        
  old.biomass <- x[bio.idxs]
  new.biomass <- tobit(mvtnorm::rmvnorm(1,mu0*old.biomass,tcrossprod(old.biomass,old.biomass)*V0)) ## draw disturbed leaf and stem
  check.max <- which(new.biomass > old.biomass)
  if(length(check.max)>0) new.biomass[check.max] = old.biomass[check.max] # constrain new.biomass[i] <= old.biomass[i]
  residual = sum(old.biomass-new.biomass)
  x[bio.idxs] <- new.biomass # update state with disturbed biomass
  x[4] <- x[4] + residual*alloc.soil # some of the biomass gets transported to the soil
  removal <- residual*(1-alloc.soil)
  return(x)
}


