##' @name predict.GP
##' @title predict.GP
##' @export
##' 
##' @param gp Gaussian Process
##' @param xpred value of x where prediction should be made
##' @param cI credible interval
##' @param pI prediction interval
##' @param splinefcns spline functions
##' 
##' @author Michael Dietze
predict.GP <- function(gp, xpred, cI = NULL, pI = NULL, splinefcns = NULL) {
  
  npred <- length(xpred)
  if (is.matrix(xpred)) {
    npred <- nrow(xpred)
  }
  nugget <- gp$nugget
  isotropic <- gp$isotropic
  d <- gp$d
  dim <- 1  #; if(!isotropic) dim <- length(d)
  x <- gp$x.compact
  x.id <- gp$x.id
  n.unique <- length(gp$x.id)
  # npred <- npred-n.unique
  y <- gp$y
  
  dprime <- NULL
  if (isotropic) {
    dprime <- distance.matrix(rbind(xpred, x), 2)
  } else {
    if (is.null(dim(x))) {
      dprime <- distance(c(xpred, x), 2)
    } else {
      dprime <- distance(rbind(xpred, x), 2)
    }
  }
  
  if (gp$method == "bayes") {
    samp <- gp$samp
    tauw <- coda::mcmc(gp$tauw[samp, ])
    psi  <- coda::mcmc(gp$psi[samp, ])
    mu   <- coda::mcmc(gp$mu)
    tauv <- W <- NULL
    if (nugget) {
      tauv <- coda::mcmc(gp$tauv)
      W <- coda::mcmc(gp$W)
    }
  } else {
    ## MLE
    psi <- gp$psi
    mu <- gp$mu
    tauw <- gp$tauw
    tauv <- gp$tauv
  }
  
  ## Krige w/o interval
  if ((is.null(cI) && is.null(pI)) || gp$method == "MLE") {
    psibar <- NULL
    if (isotropic) {
      psibar <- stats::median(psi)
    } else {
      if (is.matrix(psi)) {
        psibar <- apply(psi, 2, stats::median)
      } else {
        psibar <- psi
      }
    }
    tauwbar <- stats::median(tauw)
    Sprime <- calcSpatialCov(dprime, psibar, tauwbar)
    S12 <- Sprime[1:(npred * dim), (npred * dim + 1):(n.unique + npred * dim)]
    S22 <- Sprime[(npred * dim + 1):(n.unique + npred * dim),
                  (npred * dim + 1):(n.unique + npred * dim)]
    S22inv <- solve(S22)
    if (gp$zeroMean) {
      ey <- eyprime <- 0
    } else {
      ey <- eyprime <- stats::median(mu)  #mean(y)
    }
    ybar <- tapply(y, x.id, mean)
    yprime <- eyprime + S12 %*% S22inv %*% (ybar - ey)
    
    if (!is.null(splinefcns)) {
      ## add trend surface back on
      for (i in seq_len(nrow(xpred))) {
        f <- rep(NA, ncol(xpred))
        y0 <- splinefcns[[ncol(xpred) + 1]]
        for (j in seq_along(xpred)) {
          f[j] <- splinefcns[[j]](xpred[i, j])
        }
        y.trend[i] <- y0 + sum(f - y0)
      }
      yprime <- yprime + y.trend
    }
    
    return(yprime)
  }
  
  ### Credible and prediction intervals
  nsamp <- length(samp)
  # cInt <- pInt <- matrix(NA,nsamp,npred*dim)
  cInt <- pInt <- matrix(NA, nsamp, npred)
  progress_bar <- utils::txtProgressBar(min = 0, max = length(samp), style = 3)
  for (g in samp) {
    j <- i <- which(g == samp)
    if (dim == 1) {
      psi1 <- psi[i]
    } else {
      psi1 <- psi[i, ]
    }
    tauw1 <- tauw[i]
    if (nugget) {
      tauv1 <- tauv[i]
    }
    Sprime <- calcSpatialCov(dprime, psi1, tauw1)
    ## S22 <- Sprime[(npred*dim+1):(n.unique+npred*dim),(npred*dim+1):(n.unique+npred*dim)]
    S22 <- Sprime[(npred + 1):(n.unique + npred), (npred + 1):(n.unique + npred)]
    S22inv <- try(solve(S22))
    if (!is.numeric(S22inv)) {
      next
    }
    ## S12 <- Sprime[1:(npred*dim),(dim*npred+1):(n.unique+npred*dim)] S11 <-
    ## Sprime[1:(npred*dim),1:(npred*dim)]
    S12 <- Sprime[1:(npred), (npred + 1):(n.unique + npred)]
    S11 <- Sprime[1:(npred), 1:(npred)]
    Sbar <- S11 - S12 %*% S22inv %*% t(S12)
    
    y.trend <- 0
    if (!is.null(splinefcns)) {
      ## add trend surface back on
      for (i in seq_len(nrow(xpred))) {
        f <- rep(NA, length(xpred))
        y0 <- splinefcns[[length(xpred) + 1]]
        for (j in seq_along(xpred)) {
          f[j] <- splinefcns[[j]](xpred[i, j])
        }
        y.trend[i] <- y0 + sum(f - y0)
      }
    }
    
    if (nugget) {
      Wprime    <- mvtnorm::rmvnorm(1, S12 %*% S22inv %*% (W[i, ]), Sbar)
      cInt[j, ] <- mu[i] + Wprime + y.trend
      pInt[j, ] <- stats::rnorm(npred * dim, cInt[j, ], sqrt(tauv1))
    } else {
      cInt[j, ] <- mu[i] + S12 %*% S22inv %*% (y - mu[i]) + y.trend
      mypred <- try(mvtnorm::rmvnorm(1, cInt[j, ], Sbar), silent = TRUE)  ##wrap to prevent eigen failure
      if (is.numeric(mypred)) {
        pInt[j, ] <- mypred
      }
    }
    utils::setTxtProgressBar(progress_bar , i)
  }
  close(progress_bar)
  cIntQuant <- pIntQuant <- NULL
  if (!is.null(cI)) {
    cIntQuant <- apply(cInt, 2, stats::quantile, cI, na.rm = T)
  }
  if (!is.null(pI)) {
    pIntQuant <- apply(pInt, 2, stats::quantile, pI, na.rm = T)
  }
  return(list(ci = cIntQuant, pi = pIntQuant))
} # predict.GP
