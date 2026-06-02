#Code adapted by ASL from greenbrown, which is not available for this version of R

library(zoo)

FitDoubleLogBeck <- structure(function(
    ##title<< 
  ## Fit a double logisitic function to a vector according to Beck et al. (2006)
  ##description<<
  ## This function fits a double logistic curve to observed values using the function as described in Beck et al. (2006) (equation 3).
  
  x, ### vector or time series to fit
  
  t = 1:length(x), ### time steps
  
  tout = t, ### time steps of output (can be used for interpolation)
  
  weighting = TRUE, ### apply the weighting scheme to the observed values as described in Beck et al. 2006? This is useful for NDVI observations because higher values will get an higher weight in the estimation of the double logisitic function than lower values.	
  
  hessian = FALSE, ### compute standard errors of parameters based on the Hessian?
  
  plot = FALSE, ### plot iterations for logistic fit?
  
  ninit = 30, ### number of inital parameter sets from which to start optimization
  
  ...
  ### further arguments (currently not used)
  
  ##references<< 
  ## Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): Improved monitoring of vegetation dynamics at very high latitudes: A new method using MODIS NDVI. - Remote Sensing of Environment 100:321-334.
  
  ##seealso<<
  ## \code{\link{TSGFdoublelog}}, \code{\link{Phenology}} 
  
) {
  n <- length(x)
  avg <- mean(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  mn <- min(x, na.rm = TRUE)
  ampl <- mx - mn
  
  .doubleLog <- function(par, t) {
    mn <- par[1]
    mx <- par[2]
    sos <- par[3]
    rsp <- par[4]
    eos <- par[5]
    rau <- par[6]
    xpred <- mn + (mx - mn) * (1/(1 + exp(-rsp * (t - sos))) + 
                                 1/(1 + exp(rau * (t - eos))))
    return(xpred)
  }
  .error <- function(par, x, weights) {
    if (any(is.infinite(par))) 
      return(99999)
    if (par[1] > par[2]) 
      return(99999)
    xpred <- .doubleLog(par, t = t)
    sse <- sum((xpred - x)^2 * weights, na.rm = TRUE)
    return(sse)
  }
  if (weighting) {
    iter <- 1:2
  } else {
    iter <- 1
  }
  weights <- rep(1, length(x))
  doy <- quantile(t, c(0.25, 0.75), na.rm = TRUE)
  prior <- rbind(c(mn, mx, doy[1], 0.5, doy[2], 0.5), c(mn, 
                                                        mx, doy[2], 0.5, doy[1], 0.5), c(mn - ampl/2, mx + ampl/2, 
                                                                                         doy[1], 0.5, doy[2], 0.5), c(mn - ampl/2, mx + ampl/2, 
                                                                                                                      doy[2], 0.5, doy[1], 0.5))
  prior <- rbind(prior,
                 apply(prior, 2, function(x) rnorm(ninit-4, mean(x), sd(x)*2))
  )
  if (plot) 
    plot(t, x)
  for (i in iter) {
    if (i > 1) {
      opt.df <- opt.df[order(opt.df$cost), ]
      prior <- rbind(opt.df[1:5, -(1:2)], opt$par)
    }
    opt.l <- apply(prior, 1, optim, .error, x = x, weights = weights, 
                   method = "BFGS", control = list(maxit = 1000), hessian = FALSE)
    opt.df <- cbind(cost = unlist(plyr::llply(opt.l, function(opt) opt$value)), 
                    convergence = unlist(plyr::llply(opt.l, function(opt) opt$convergence)), 
                    plyr::ldply(opt.l, function(opt) opt$par))
    best <- which.min(opt.df$cost)
    if (opt.df$convergence[best] == 1) {
      opt <- opt.l[[best]]
      opt <- optim(opt.l[[best]]$par, .error, x = x, weights = weights, 
                   method = "BFGS", control = list(maxit = 1500), 
                   hessian = FALSE)
      prior <- rbind(prior, opt$par)
      xpred <- .doubleLog(opt$par, t)
    } else if (opt.df$convergence[best] == 0) {
      opt <- opt.l[[best]]
      prior <- rbind(prior, opt$par)
      xpred <- .doubleLog(opt$par, t)
    }
    parinit <- opt$par
    mn <- opt$par[1]
    mx <- opt$par[2]
    sos <- opt$par[3]
    rsp <- opt$par[4]
    eos <- opt$par[5]
    rau <- opt$par[6]
    m <- lm(c(0, 100) ~ c(sos, eos))
    tr <- coef(m)[2] * t + coef(m)[1]
    tr[tr < 0] <- 0
    tr[tr > 100] <- 100
    res <- xpred - x
    weights <- 1/((tr * res + 1)^2)
    weights[res > 0 & res <= 0.01] <- 1
    weights[res < 0] <- 4
  }
  if (hessian) {
    opt <- optim(opt$par, .error, x = x, weights = weights, 
                 method = "BFGS", hessian = TRUE)
    .qr.solve <- function(a, b, tol = 1e-07, LAPACK = TRUE) {
      if (!is.qr(a)) 
        a <- qr(a, tol = tol, LAPACK = LAPACK)
      nc <- ncol(a$qr)
      nr <- nrow(a$qr)
      if (a$rank != min(nc, nr)) 
        stop("singular matrix 'a' in solve")
      if (missing(b)) {
        if (nc != nr) 
          stop("only square matrices can be inverted")
        b <- diag(1, nc)
      }
      res <- try(qr.coef(a, b), silent=TRUE) 
      if (class(res)[1] == "try-error") {
        res <- matrix(NA, ncol(b), nrow(b))
      }
      res
    }
    vc <- .qr.solve(opt$hessian)
    npar <- nrow(vc)
    s2 <- opt$value^2/(n - npar)
    std.errors <- sqrt(diag(vc) * s2)
    unc.alg <- "Standard errors computed from Hessian."
    
    # compute standard errors with backup algorithm
    if (all(is.na(std.errors))) {
      prior <- rbind(opt.df[1:5, -(1:3)], opt$par)
      v <- apply(prior, 2, var)
      std.errors <- sqrt(v * s2)
      message("Standard errors computed with backup algorithm.")
      unc.alg <- "Standard errors computed from variance of best optimization trials"
    }
    names(std.errors) <- c("mn", "mx", "sos", "rsp", "eos", "rau")
  }
  #    if (opt$convergence != 0) {
  #        opt$par[] <- NA
  #        xpred <- rep(NA, length(tout))
  #    } else {
  xpred <- .doubleLog(opt$par, tout)
  #    }
  xpred.out <- zoo(xpred, order.by = tout)
  if (plot) {
    plyr::llply(opt.l, function(opt) {
      xpred <- .doubleLog(opt$par, t)
      lines(t, xpred, col = "cyan")
    })
    lines(t, xpred, col = "blue", lwd = 2)
  }
  names(opt$par) <- c("mn", "mx", "sos", "rsp", "eos", "rau")
  fit.formula <- expression(mn + (mx - mn) * (1/(1 + exp(-rsp * 
                                                           (t - sos))) + 1/(1 + exp(rau * (t - eos)))))
  output <- list(predicted = xpred.out, params = opt$par, formula = fit.formula)
  if (hessian) 
    output <- list(predicted = xpred.out, params = opt$par, 
                   formula = fit.formula, stdError = std.errors)
  
  return(output)
  ### The function returns a list with fitted values, parameters, fitting formula and standard errors if hessian is TRUE
}, ex=function() {
  
  # select one year of data
  x <- as.vector(window(ndvi, start=c(1994,1), end=c(1994, 12)))
  plot(x)
  
  # fit double-logistic function to one year of data
  fit <- FitDoubleLogBeck(x)
  lines(fit$predicted, col="blue")
  
  # # do more inital trials, plot iterations and compute parameter uncertainties
  # FitDoubleLogBeck(x, hessian=TRUE, plot=TRUE, ninit=100)	
  # 
  # # fit double-logistic function to one year of data, 
  # # interpolate to daily time steps and calculate phenology metrics
  # tout <- seq(1, 12, length=365)	# time steps for output (daily)
  # fit <- FitDoubleLogBeck(x, tout=tout)
  # PhenoDeriv(fit$predicted, plot=TRUE)
  
})

