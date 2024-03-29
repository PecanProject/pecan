##' @name GaussProcess
##' @title GaussProcess
##' @export
##'
##' @param x set of independent variables
##' @param y dependent variable
##' @param isotropic Boolean indicating whether the GP is fit isotropically. If FALSE, distances are calculated deparately for each direction
##' @param nugget allows additional error in Y rather than fix interpolation to go through points
##' @param method method for calculating correlations
##' @param ngibbs number of MCMC iterations (per chain) to run
##' @param burnin Number of samples to discard as burnin (auto must be FALSE)
##' @param thin thinning of the matrix to make things faster. Default is to thin to 1
##' @param jump.ic initial condition for jump standard deviation.
##' @param prior 'unif', 'IG'
##' @param mix joint=mix over psi simultanously, each=mix over psi individually
##' @param psi spatial corr
##' @param zeroMean True if mean is 0, else false
##' @param exclude <- isn't used anywhere, should be dropped
##' @param ... Additional arguments
##'
##' @author Michael Dietze
GaussProcess <- function(x, y, isotropic = TRUE, nugget = TRUE, method = "bayes", ngibbs = 5000, 
                         burnin = 1000, thin = 1, jump.ic = c(1.1, 0.2), prior = "IG", mix = "joint",
                         psi = NULL, zeroMean = FALSE, exclude = NULL, ...) {
  ## isotropic <- FALSE;nugget<-FALSE;method='bayes';ngibbs <- 50; burnin <- 10;thin<- 1;
  ## jump.ic<-c(1.1,0.2); prior <- 'unif'
  

  ## library('dietze')
  
  if (burnin > ngibbs) {
    burnin <- floor(ngibbs * 0.25)
  }
  
  if (!(method %in% c("bayes", "MLE"))) {
    stop(cat(method, "not yet implemented"))
  }
  
  ## deal with repeated measures
  x.full    <- x
  x.id      <- groupid(x)
  x.compact <- NULL
  n.unique  <- max(unique(x.id))
  n <- length(x)
  if (is.matrix(x)) {
    n <- nrow(x)
  }
  dimx <- dim(x)
  for (i in unique(x.id)) {
    if (is.null(dimx)) {
      x.compact <- c(x.compact, x.full[which(x.id == i)[1]])
    } else {
      x.compact <- rbind(x.compact, x.full[which(x.id == i)[1], ])
    }
  }
  if (!nugget && n > n.unique) {
    y <- y[!duplicated(y)]
    ## stop('repeated measured detected, but nugget == FALSE')
  }
  ##settings
  ##  isotropic <- (is.matrix(d) || (is.list(d) && length(d) == 1))         # isotropic -> correlation same in all directions
  
  ##calc distance matrix
  d <- NULL
  if (isotropic) {
    d <- distance.matrix(x.compact, 2)
  } else {
    d <- distance(x.compact, 2)
  }
  dim <- 1
  if (!isotropic) {
    dim <- length(d)
  }
  
  ## IC and Priors
  mu <- mean(y)
  if (zeroMean) {
    mu <- 0
  }
  av    <- bv <- 0.001  #nugget IG prior
  aw    <- bw <- 0.001  #covariance IG prior
  ap    <- bp <- 0.01  #spatial IG prior
  mu.V0 <- 10 ^ ceiling(log10(stats::var(y)) + 2)  #mean prior variance
  tauw  <- tauv <- stats::var(y) * 0.5
  if (is.null(psi)) {
    psi <- rep(1, dim)
  }
  S       <- calcSpatialCov(d, psi, tauw)  #spatial covariance
  Tinv    <- diag(1 / tauv, n)
  W       <- y - mu  #spatial random effects
  nseq    <- seq_len(n.unique)
  W.full  <- W[x.id]
  id.count <- as.vector(table(x.id))
  X        <- matrix(rep(1, n.unique), n.unique, 1)
  
  ## maximum likelihood
  if (zeroMean) {
    parm <- c(tauw, psi)
    if (nugget) {
      parm <- c(tauw, tauv, psi)
    }
    nmin <- stats::nlm(gp_mle2, parm, d = d, nugget = nugget, myY = y, maxval = 1e+30)
    mu <- 0
    tauw <- abs(nmin$estimate[1])
    if (nugget) {
      tauv <- abs(nmin$estimate[2])
      psi <- abs(nmin$estimate[3:length(parm)])
    } else {
      tauv <- 0
      psi <- abs(nmin$estimate[2:length(parm)])
    }
  } else {
    parm <- c(mu, tauw, psi)
    if (nugget) {
      parm <- c(mu, tauw, tauv, psi)
    }
    nmin <- stats::nlm(gp_mle, parm, d = d, nugget = nugget, myY = y, maxval = 1e+30)
    mu <- nmin$estimate[1]
    tauw <- abs(nmin$estimate[2])
    if (nugget) {
      tauv <- abs(nmin$estimate[3])
      psi <- abs(nmin$estimate[4:length(parm)])
    } else {
      tauv <- 0
      psi <- abs(nmin$estimate[3:length(parm)])
    }
  }
  ## function (theta, d, nugget, myY, maxval = Inf)
  
  if (method == "MLE") {
    return(list(method = method, tauw = tauw, tauv = tauv, mu = mu, psi = psi, 
                nugget = nugget, isotropic = isotropic, d = d, x.id = x.id, 
                x.compact = x.compact, y = y, mle = nmin, zeroMean = zeroMean))
  }
  
  ## Storage
  samp      <- seq(burnin, ngibbs, thin)
  nsamp     <- length(samp)
  tauwjump  <- jump(jump.ic[1])
  psijump   <- jump(jump.ic[2])
  if (mix == "each") {
    psijump <- mvjump(ic = jump.ic[2], dim = ncol(x))
  }
  tauwgibbs <- matrix(NA, ngibbs, 1)  #spatial var
  psigibbs  <- matrix(NA, ngibbs, dim)  #spatial corr
  mugibbs   <- rep(NA, nsamp)  #mean
  Wgibbs    <- tauvgibbs <- NULL
  if (nugget) {
    Wgibbs <- matrix(NA, nsamp, n.unique)  #spatial random effects
    tauvgibbs <- rep(NA, nsamp)  #nugget var
  }
  
  # reset spatial covariance for mle
  S <- calcSpatialCov(d, psi, tauw)
  
  ## progress bar
  progress_bar <- utils::txtProgressBar(min = 0, max = ngibbs, style = 3)
  
  ## Gibbs loop
  for (g in seq_len(ngibbs)) {
    
    cc <- 1
    ## draw W
    if (nugget) {
      Sinv <- solve(S)
      if (n.unique == n) {
        M <- solve(Tinv + Sinv)
        m <- Tinv %*% (y - mu)
        W <- mvtnorm::rmvnorm(1, M %*% m, M)
        W.full <- W
      } else {
        ##method 1, draw W's individually
        ##for(i in 1:n.unique){
        ##  sel <- nseq[nseq != i]
        ##  u <- which(x.id == i)
        ##  mubar <- S[i,sel] %*% solve(S[sel,sel]) %*% W[sel]
        ##  Sbar <- S[i,i]-S[i,sel] %*% solve(S[sel,sel]) %*% S[sel,i]
        ##  M <- 1/(length(u)/tauv+Sbar)
        ##  m <- sum(y[u]-mu)/tauv
        ##  W[i] <- rnorm(1,M*m,M)
        ##}
        ##method 2, aggregate Y's
        yagg   <- tapply(y - mu, x.id, sum)
        M      <- solve(id.count * diag(1 / tauv, n.unique) + Sinv)
        m      <- diag(1 / tauv, n.unique) %*% yagg
        W      <- mvtnorm::rmvnorm(1, M %*% m, M)
        W.full <- W[x.id]
      }
    } else {
      ## no nugget -- deterministic
      W <- W.full <- y - mu
    }
    
    cc <- 2
    ## draw psi
    if (mix == "joint") {
      psistar <- exp(stats::rnorm(dim, log(psi), p(psijump)))
      Sstar <- calcSpatialCov(d, psistar, tauw)
      ##anum.p <- try(sum(log(dinvgamma(psistar,ap,bp))) + dmvnorm(as.vector(W),rep(0,n.unique),Sstar,log=TRUE),TRUE)
      ##aden.p <- sum(log(dinvgamma(psi,ap,bp))) + dmvnorm(as.vector(W),rep(0,n.unique),S,log=TRUE)      anum.p <- aden.p <- 0  ## inproper uniform prior
      if (prior == "IG") {
        anum.p <- sum(ldinvgamma(psistar, ap, bp))
        aden.p <- sum(ldinvgamma(psi, ap, bp))
      }
      anum.p <- try(mvtnorm::dmvnorm(as.vector(W), rep(0, n.unique), Sstar, log = TRUE) + anum.p, TRUE)
      aden.p <- mvtnorm::dmvnorm(as.vector(W), rep(0, n.unique), S, log = TRUE) + aden.p
      if (is.numeric(anum.p) && 
          is.finite(anum.p) && 
          exp(anum.p - aden.p) > stats::runif(1) && 
          min(psistar) > 0) {
        psi <- psistar
        S   <- Sstar
      }
      psigibbs[g, ] <- psi
      psijump <- stats::update(psijump, psigibbs)
    } else {
      ## mix = each
      psistar <- psi
      for (i in seq_len(dim)) {
        psistar[i] <- exp(stats::rnorm(1, log(psi), p(psijump)[i]))
        Sstar      <- calcSpatialCov(d, psistar, tauw)
        anum.p     <- aden.p <- 0  ## inproper uniform prior
        if (prior == "IG") {
          anum.p <- sum(ldinvgamma(psistar, ap, bp))
          aden.p <- sum(ldinvgamma(psi, ap, bp))
        }
        anum.p <- try(mvtnorm::dmvnorm(as.vector(W), rep(0, n.unique), Sstar, log = TRUE) + anum.p, TRUE)
        aden.p <- mvtnorm::dmvnorm(as.vector(W), rep(0, n.unique), S, log = TRUE) + aden.p
        if (is.numeric(anum.p) && 
            is.finite(anum.p) && 
            exp(anum.p - aden.p) > stats::runif(1) && 
            min(psistar) > 0) {
          psi <- psistar
          S <- Sstar
        } else {
          psistar <- psi
        }
      }
      psigibbs[g, ] <- psi
      psijump <- stats::update(psijump, psigibbs)
    }
    
    cc <- 3
    ## draw tauw
    taustar <- exp(stats::rnorm(1, log(tauw), p(tauwjump)))
    Sstar   <- calcSpatialCov(d, psi, taustar)
    anum    <- aden <- 0
    if (prior == "IG") {
      anum <- ldinvgamma(taustar, aw, bw)
      aden <- ldinvgamma(tauw, aw, bw)
    }
    anum <- try(anum + mvtnorm::dmvnorm(as.vector(W), rep(0, n.unique), Sstar, log = TRUE))
    aden <- aden + mvtnorm::dmvnorm(as.vector(W), rep(0, n.unique), S, log = TRUE)
    if (is.numeric(anum) && 
        is.finite(anum) && 
        exp(anum - aden) > stats::runif(1)) {
      tauw <- taustar
      S <- Sstar
    }
    tauwgibbs[g, ] <- tauw
    tauwjump <- stats::update(tauwjump, tauwgibbs)
    
    cc <- 4
    ## draw tauv
    if (nugget) {
      tauv <- MCMCpack::rinvgamma(1, av + n / 2, bv + 0.5 * sum((y - rep(mu, n) - W.full) ^ 2))
      Tinv <- diag(1 / tauv, n)
    }
    
    cc <- 5
    ## draw mu
    if (zeroMean) {
      mu <- 0
    } else {
      if (nugget) {
        M <- 1 / (n / tauv + 1 / mu.V0)
        m <- sum(y - W.full) / tauv
        mu <- stats::rnorm(1, M * m, M)
      } else {
        Sinv <- solve(S)
        M <- solve(t(X) %*% Sinv %*% X + 1 / mu.V0)
        m <- t(X) %*% Sinv %*% y  ##[1:10]
        mu <- stats::rnorm(1, M * m, M)
      }
    }
    
    cc <- 6
    ## store
    if (g %in% samp) {
      i <- which(samp == g)
      mugibbs[i] <- mu
      ## psigibbs[i] <- psi tauwgibbs[i] <- tauw
      if (nugget) {
        tauvgibbs[i] <- tauv
        Wgibbs[i, ] <- W
      }
    }
    utils::setTxtProgressBar(progress_bar , g)
  }
  close(progress_bar)
  
  return(list(method = method, tauwjump = tauwjump, tauw = tauwgibbs, 
              psijump = psijump, psi = psigibbs, mu = mugibbs, tauv = tauvgibbs,
              W = Wgibbs, nugget = nugget, isotropic = isotropic, d = d, samp = samp, 
              x.id = x.id, x.compact = x.compact, y = y, zeroMean = zeroMean))
} # GaussProcess
