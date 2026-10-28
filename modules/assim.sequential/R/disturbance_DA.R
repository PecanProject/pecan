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
##' @param mu0 numeric vector (dim = B where B is the number of biomass variables): percent decreases of biomass to be used for the disturbance model.
##' @param V0 numeric matrix (dim B*B): covariance to be used for the disturbance model
##' @param pD prior of disturbed Vs. undisturbed probability, default is c(0.99, 0.01).
##'  
##' @description This function provides semi-analytical updates for the disturbance probability.
##' 
##' @return returns a vector of mua (dim = N, vector of means for each updated state variable) and pa (dim = N*N, covariance matrix for the updated state variables).
##' @importFrom dplyr %>%
##' @export

disturbance_DA <- function (X, Y, R, H, aqq, bqq, disturbance, mu0=rep(0.8, 0.8), V0=diag(rep(0.25, 2)), pD = c(0.99, 0.01)) {
  
  # store number of ensemble members, state vars, components, variable names.
  ne <- nrow(X)
  n.state <- ncol(X)
  n.comp <- 2
  var.names <- unique(names(X))
  
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
  d.forecast <- stats::rmultinom(ne, 1, d.ens.p)
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
    temp <- disturbance.p(Nf[j,], var.names, 1-mu0, V0, bio.idxs, 0.25)
    Nf[j,] <- temp$x
  }
  Nf <- tobit(Nf)
  muF[1,] = colMeans(X[sel.undisturbed,])
  CF[1,,] = stats::var(Nf[sel.undisturbed,])
  pF[1,,] = solve(CF[1,,])
  muF[2,] = colMeans(Nf[sel.dist,])
  CF[2,,] = stats::var(Nf[sel.dist,])
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
  H.mat <- array(0, dim=c(n.obs, n.state))
  # loop over observations based on their mapping index to the specific state variables.
  for (i in 1:n.obs) {
    H.mat[i,H[i]] <- 1
  }
  # loop over disturbed and undisturbed ensembles.
  for (d in 1:2) {
    muf <- muF[d,]
    Cf <- CF[d,,]
    pf <- pF[d,,]
    CA[d,,] <- solve(pf + t(H.mat) %*% solve(R) %*% H.mat)
    muA[d,] <-  CA[d,,] %*% (pf %*% muf + t(H.mat) %*% solve(R) %*% Y) # updated analysis.
    # the multivariate normal density likelihood of the observed data given the forecast model parameters.
    lik <- mvtnorm::dmvnorm(Y, mean=H.mat %*% muf, sigma = H.mat %*% Cf %*% t(H.mat) + R)
    # multiply the likelihood by the disturbed/undisturbed prior probability.
    wA[d] <- lik * pD[d]
  }
  wA <- wA / sum(wA) # normalize the weight.
  
  # Grab the undisturbed weight (assuming index 1 = undisturbed, index 2 = disturbed)
  prob_undisturbed <- wA[1]
  
  # Only update hyperparameters if there is a non-zero probability of being undisturbed
  if (prob_undisturbed > 0 && length(sel.undisturbed) > 0) {
    # Subset matrix to undisturbed ensemble members
    undisturbed_Nf <- Nf[sel.undisturbed, , drop = FALSE]
    undisturbed_X  <- X[sel.undisturbed, , drop = FALSE]
    
    n_samples <- nrow(undisturbed_X)
    
    # Calculate sum of squared residuals across undisturbed members, weighted by posterior prob
    residuals <- (undisturbed_Nf - undisturbed_X) ^ 2
    SS_residual <- colSums(residuals) * prob_undisturbed
    
    # Update Hyperparameters
    aqq.new <- aqq + ((n_samples * prob_undisturbed) / 2)
    bqq.new <- bqq + (SS_residual / 2)
  } else {
    aqq.new <- aqq
    bqq.new <- bqq
  }
  
  # Resample/adjust an analysis ensemble separately for undisturbed (d=1) and disturbed (d=2) classes
  ana.ens <- matrix(NA, nrow = ne, ncol = n.state)
  
  # Define list of member indices for each class (d.class: 0 = undisturbed, 1 = disturbed)
  class_members <- list(
    which(d.class == 0), # Undisturbed (d = 1)
    which(d.class == 1)  # Disturbed   (d = 2)
  )
  
  for (d in 1:n.comp) {
    inds <- class_members[[d]]
    if (length(inds) > 0) {
      # Extract forecast ensemble for class d
      X_d <- X[inds, , drop = FALSE]
      
      # Apply standard ensemble adjustment using component d's moments
      # Cf[d,,] = forecast covariance, muF[d,] = forecast mean, muA[d,] = analysis mean, CA[d,,] = analysis covariance
      ana.ens[inds, ] <- adj.ens(Cf[d, , ], X_d, muF[d, ], muA[d, ], CA[d, , ])
    }
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
##' @param var.names vector of variable names across x.
##' @param mu0 mean biomass % retention after disturbance, dim B (number of biomass state variables)
##' @param V0 covariance in post disturbance leaf and stem biomass, dim BxB
##' @param bio.idxs vector of indices of biomass values in x, dim B
##' @param alloc.soil fraction of removed C that goes in soil

##' @description
##' This function simulates a disturbance on a vector of biomass values.
##' 
##' @return returns list containing: 
##' 1) an updated vector of state variables (dim N), `X`;
##' 2) total biomass loss before and after the disturbance, `tot.bioloss`;
##' 3) biomass added to the SOC, `soc.add`;
##' 4) total removal of biomass, `removal`;
disturbance.p <- function(x,var.names,mu0,V0,bio.idxs,alloc.soil){        
  old.biomass <- x[bio.idxs]
  new.biomass <- tobit(mvtnorm::rmvnorm(1,mu0*old.biomass,tcrossprod(old.biomass,old.biomass)*V0)) ## draw disturbed leaf and stem
  check.max <- which(new.biomass > old.biomass)
  if(length(check.max)>0) new.biomass[check.max] = old.biomass[check.max] # constrain new.biomass[i] <= old.biomass[i]
  residual = sum(old.biomass-new.biomass)
  x[bio.idxs] <- new.biomass # update state with disturbed biomass
  soc.ind <- which(var.names == "TotSoilCarb")
  x[soc.ind] <- x[soc.ind] + residual*alloc.soil # some of the biomass gets transported to the soil
  removal <- residual*(1-alloc.soil)
  return(list(x = x, tot.bioloss = residual, soc.add = residual*alloc.soil, removal = residual*(1-alloc.soil)))
}


