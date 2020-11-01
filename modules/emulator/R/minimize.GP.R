##' @name minimize.GP
##' @title minimize.GP
##' @export
##'
##' @param gp
##' @param rng
##' @param x0 
##' @param splinefuns
##' 
##' @author Michael Dietze
minimize.GP <- function(gp, rng, x0, splinefuns = NULL) {
  
  isotropic <- gp$isotropic
  x.id      <- gp$x.id
  ey        <- 0
  
  if (gp$method == "bayes") {
    samp <- gp$samp
    tauw <- coda::mcmc(gp$tauw[samp, ])
    psi  <- coda::mcmc(gp$psi[samp, ])
    mu   <- coda::mcmc(gp$mu)
    tauv <- W <- NULL
  } else {
    ## MLE
    psi  <- gp$psi
    mu   <- gp$mu
    tauw <- gp$tauw
    tauv <- gp$tauv
  }
  
  psibar <- NULL
  if (isotropic) {
    psibar <- median(psi)
  } else {
    if (is.matrix(psi)) {
      psibar <- apply(psi, 2, median)
    } else {
      psibar <- psi
    }
  }
  tauwbar <- median(tauw)
  S <- calcSpatialCov(gp$d, psibar, tauwbar)
  # S12 <- Sprime[1:(npred*dim),(npred*dim+1):(n.unique+npred*dim)] S22 <-
  # Sprime[(npred*dim+1):(n.unique+npred*dim),(npred*dim+1):(n.unique+npred*dim)]
  S22inv <- solve(S)
  if (gp$zeroMean) {
    ey <- 0
  } else {
    ey <- max(mu)  #mean(y) 
  }
  ybar <- tapply(gp$y, gp$x.id, mean)
  k    <- S22inv %*% (ybar - ey)
  
  nlm(gpeval, x0, k = k, mu = ey, tau = tauwbar, psi = psibar,
      x = gp$x.compact, rng = rng, splinefcns = splinefcns)
} # minimize.GP


##' Calculates the probability of a set of parameter values, given by xnew
##'
##' @name gpeval
##' @title gpeval
##' @export
##'
##' @param xnew
##' @param k
##' @param mu
##' @param tau
##' @param psi
##' @param x
##' @param rng range
##' @param splinefcns
##' 
##' @author Michael Dietze 
gpeval <- function(xnew, k, mu, tau, psi, x, rng, splinefcns) {
  
  ## second calc value
  S12 <- sapply(seq_along(k), function(i) {
    tau * exp(-sum(psi * (xnew - x[i, ]) ^ 2))
  })
  yprime <- mu + sum(S12 * k)
  
  if (!is.null(splinefcns)) {
    ## add trend surface back on
    y0 <- splinefuns[[length(xnew) + 1]]
    f <- sapply(seq_along(xnew), function(j) {
      splinefuns[[j]](xnew[j])
    })
    y.trend <- y0 + sum(f - y0)
    yprime <- yprime + ytrend
  }
  
  return(yprime)
} # gpeval


##' @name ddist
##' @title ddist
##' @export
ddist <- function(x, prior) {
  eval(parse(text = paste("d", prior$distn, sep = "")))(x, prior$parama, prior$paramb)
} # ddist


# calculate.prior <- function(samples, priors){ traits <- names(samples) joint <-
# sum(sapply(1:nrow(priors), function(i) -log(ddist(samples[[i]], priors[i,])))) #note:
# this is within the negative log domain return(joint) }

##' @name calculate.prior
##' @title calculate.prior
##' @export
calculate.prior <- function(samples, priors) {
  sum(sapply(seq_along(priors), function(i) eval(priors[[i]], list(x = samples[[i]]))))
} # calculate.prior

##' @name get_ss
##' @title get_ss
##' @export
get_ss <- function(gp, xnew, pos.check) {
  
  SS <- numeric(length(gp))
  
  X <- matrix(unlist(xnew), nrow = 1, byrow = TRUE)
  
  for(igp in seq_along(gp)){
    Y <- mlegp::predict.gp(gp[[igp]], newData = X[, 1:ncol(gp[[igp]]$X), drop=FALSE], se.fit = TRUE) 
    
    j <- (igp %% length(pos.check)) 
    if(j == 0) j <- length(pos.check)
    
    if(pos.check[j]){
      if(Y$fit < 0){
        return(-Inf)
      }
      repeat {
        SS[igp] <- rnorm(1, Y$fit, Y$se.fit)
        if (SS[igp] > 0) {
          break
        }
      }
    }else{
      SS[igp] <- rnorm(1, Y$fit, Y$se.fit)
    }
  }
  return(SS)
  
} # get_ss

##' @name get_y
##' @title get_y
##' @export
get_y <- function(SSnew, xnew, llik.fn, priors, llik.par) {
  
  likelihood <- pda.calc.llik(SSnew, llik.fn, llik.par)
  
  prior.prob <- calculate.prior(xnew, priors)
  posterior.prob <- likelihood + prior.prob
  
  return(posterior.prob)
  
} # get_y

# is.accepted <- function(ycurr, ynew, format='lin'){ z <- exp(ycurr-ynew) acceptance <-
# z>runif(1) return(acceptance) }

##' @name is.accepted
##' @title is.accepted
##' @export
is.accepted <- function(ycurr, ynew, format = "lin") {
  a <- exp(ynew - ycurr)
  a > runif(1)
} # is.accepted

##' Function to sample from a GP model
##' that is assumed to be a -lnLikelihood surface
##' with flat priors and bounded region
##'
##' @name mcmc.GP
##' @title mcmc.GP
##' @export
##'
##' @param gp Gaussian Process
##' @param x0 initial values
##' @param nmcmc number of iterations
##' @param rng range of knots
##' @param format lin = lnlike fcn, log = log(lnlike)
##' @param mix each = jump each dim. independently, joint = jump all at once 
##' @param splinefcns spline functions, not used
##' @param jmp0 initial jump variances
##' @param ar.target acceptance rate target
##' @param settings PEcAn settings list
##' @param priors prior list
##' @param run.block is this a new run or making the previous chain longer
##' @param n.of.obs number of observations
##' @param llik.fn list that contains likelihood functions
##' @param hyper.pars hyper parameters
##' @param resume.list list of needed info if we are running the chain longer
##' 
##' @author Michael Dietze
mcmc.GP <- function(gp, x0, nmcmc, rng, format = "lin", mix = "joint", splinefcns = NULL, 
                    jmp0 = 0.35 * (rng[, 2] - rng[, 1]), ar.target = 0.5, priors = NA, settings, 
                    run.block = TRUE, n.of.obs, llik.fn, hyper.pars, resume.list = NULL) {
  
  pos.check <- sapply(settings$assim.batch$inputs, `[[`, "ss.positive")
  
  if(length(unlist(pos.check)) == 0){
    # if not passed from settings assume none
    pos.check <- rep(FALSE, length(settings$assim.batch$inputs))
  }else if(length(unlist(pos.check)) != length(settings$assim.batch$inputs)){
    # maybe one provided, but others are forgotten
    # check which ones are provided in settings
    from.settings <- sapply(seq_along(pos.check), function(x) !is.null(pos.check[[x]]))
    tmp.check <- rep(FALSE, length(settings$assim.batch$inputs))
    # replace those with the values provided in the settings
    tmp.check[from.settings] <- as.logical(unlist(pos.check))
    pos.check <- tmp.check
  }else{
    pos.check <- as.logical(pos.check)
  }
  
  # get SS
  currSS <- get_ss(gp, x0, pos.check)
  
  
  currllp <- pda.calc.llik.par(settings, n.of.obs, currSS, hyper.pars)
  pcurr   <- unlist(sapply(currllp, `[[` , "par"))
  
  xcurr <- unlist(x0)
  dim   <- length(x0)
  samp  <- matrix(NA, nmcmc, dim)
  par   <- matrix(NA, nmcmc, length(pcurr), dimnames = list(NULL, names(pcurr))) # note: length(pcurr) can be 0
  
  
  if (run.block) {
    jcov <- diag((jmp0)^2)
    accept.count <- 0
    start <- 1
    # jmp <- mvjump(ic=jmp0,rate=ar.target, nc=dim)
  } else {
    jcov <- jmp0
    accept.count <- resume.list$ac
    prev.samp    <- resume.list$prev.samp
    prev.par     <- resume.list$par
    colnames(prev.samp) <- names(x0)
    samp  <- rbind(prev.samp, samp)
    par   <- rbind(prev.par, par)
    start <- dim(prev.samp)[1] + 1
    nmcmc <- dim(samp)[1]
    # jmp <- mvjump(ic=diag(jmp0),rate=ar.target, nc=dim)
  }
  
  # make sure it is positive definite, see note below
  jcov <- lqmm::make.positive.definite(jcov, tol=1e-12)
  
  for (g in start:nmcmc) {
    
    if (mix == "joint") {
      
      # adapt
      if ((g > 2) && ((g - 1) %% settings$assim.batch$jump$adapt == 0)) {
        params.recent <- samp[(g - settings$assim.batch$jump$adapt):(g - 1), ]
        colnames(params.recent) <- names(x0)
        # accept.count <- round(jmp@arate[(g-1)/settings$assim.batch$jump$adapt]*100)
        jcov <- pda.adjust.jumps.bs(settings, jcov, accept.count, params.recent)
        accept.count <- 0  # Reset counter
        
        # make sure precision is not going to be an issue
        # NOTE: for very small values this is going to be an issue
        # maybe include a scaling somewhere while building the emulator
        jcov <- lqmm::make.positive.definite(jcov, tol=1e-12)
      }
      
      ## propose new parameters
      xnew <- TruncatedNormal::rtmvnorm(1, mu =  c(xcurr), sigma = jcov, lb = rng[,1], ub = rng[,2])
      # if(bounded(xnew,rng)){
      
      # re-predict SS
      currSS <- get_ss(gp, xcurr, pos.check)
      
      
      
      # don't update the currllp ( = llik.par, e.g. tau) yet
      # calculate posterior with xcurr | currllp
      ycurr  <- get_y(currSS, xcurr, llik.fn, priors, currllp)
      HRcurr <- TruncatedNormal::dtmvnorm(c(xnew), c(xcurr), jcov,
                                          lb = rng[,1], ub = rng[,2], log = TRUE, B = 1e2)
      
      newSS  <- get_ss(gp, xnew, pos.check)
      if(all(newSS != -Inf)){
        
        newllp <- pda.calc.llik.par(settings, n.of.obs, newSS, hyper.pars)
        ynew   <- get_y(newSS, xnew, llik.fn, priors, newllp)
        HRnew <- TruncatedNormal::dtmvnorm(c(xcurr), c(xnew), jcov,
                                           lb = rng[,1], ub = rng[,2], log = TRUE, B = 1e2)
        
        if (is.accepted(ycurr+HRcurr, ynew+HRnew)) {
          xcurr  <- xnew
          currSS <- newSS
          accept.count <- accept.count + 1
        }
        
        # now update currllp | xcurr
        currllp <- pda.calc.llik.par(settings, n.of.obs, currSS, hyper.pars)
        pcurr   <- unlist(sapply(currllp, `[[` , "par"))
      }
      # } mix = each
    } else {
      for (i in seq_len(dim)) {
        ## propose new
        repeat {
          xnew[i] <- rnorm(1, xcurr[[i]], p(jmp)[i])
          if (bounded(xnew[i], rng[i, , drop = FALSE])) {
            break
          }
        }
        # if(bounded(xnew,rng)){
        currSS <- get_ss(gp, xcurr, pos.check)
        
        ycurr  <- get_y(currSS, xcurr, llik.fn, priors, currllp)
        
        
        newSS  <- get_ss(gp, xnew, pos.check)
        
        
        newllp <- pda.calc.llik.par(settings, n.of.obs, newSS, hyper.pars)
        ynew   <- get_y(newSS, xnew, llik.fn, priors, newllp)
        if (is.accepted(ycurr, ynew)) {
          xcurr  <- xnew
          currSS <- newSS
        }
        
        currllp <- pda.calc.llik.par(settings, n.of.obs, currSS, hyper.pars)
        pcurr   <- unlist(sapply(currllp, `[[` , "par"))
        
        # }
      }
    }
    samp[g, ] <- unlist(xcurr)
    par[g, ]  <- pcurr
    
    if(g %% 1000 == 0) PEcAn.logger::logger.info(g, "of", nmcmc, "iterations")
    # print(p(jmp)) jmp <- update(jmp,samp)
  }
  
  
  chain.res <- list(jump = jcov, ac = accept.count, prev.samp = samp, par = par, n.of.obs = n.of.obs)
  
  return(list(mcmc.samp = samp, mcmc.par = par, chain.res = chain.res))
  ## xnew <- gpeval,x0,k=k,mu=ey,tau=tauwbar,psi=psibar,x=gp$x.compact,rng=rng)
  
  ################### IN PROGRESS ##############
} # mcmc.GP


##' @name bounded
##' @title bounded
##' @export
bounded <- function(xnew, rng) {
  xnew <- as.vector(as.matrix(xnew))
  down <- xnew > rng[, 1]
  up <- xnew < rng[, 2]
  return(all(up & down))
} # bounded


##' @name plot.mvjump
##' @title plot.mvjump
##' @export
##'
##' @param jmp
##' 
##' @author Michael Dietze
plot.mvjump <- function(jmp) {
  par(mfrow = c(1, 2))
  plot(attr(jmp, "history")[, 1], ylab = "Jump Parameter", main = "Jump Parameter")
  abline(h = mean(attr(jmp, "history")[, 1], na.rm = TRUE))
  text(0.9 * length(attr(jmp, "history")[, 1]), 
       min(attr(jmp, "history")[, 1]) + 0.8 * 
         (max(attr(jmp, "history")[, 1]) - min(attr(jmp, "history")[, 1])), 
       paste("mean=", mean(attr(jmp, "history")[, 1])))
  plot(attr(jmp, "arate"), ylab = "Acceptance Rate", 
       main = "Acceptance Rate", 
       ylim = c(0, 1))
  abline(h = attr(jmp, "target"))
  abline(h = mean(attr(jmp, "arate"), na.rm = TRUE), col = 2)
} # plot.mvjump
