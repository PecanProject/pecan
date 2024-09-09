##' Approximate the posterior MCMC with a closed form pdf
##'
##' returns priors where posterior MCMC are missing
##' 
##' NOTE: this function is similar to PEcAn.priors::fit.dist
##' @title Approximate posterior
##' @param trait.mcmc meta analysis outputs
##' @param priors dataframe of priors used in meta analysis
##' @param trait.data data used in meta-analysis (used for plotting)
##' @param outdir directory in which to plot results
##' @param filename.flag text to be included in the posteriors.pdf filename to make unique
##' @return posteriors data frame, similar to priors,
##' but with closed form pdfs fit to meta-analysis results  
##' @export
##' @author David LeBauer, Carl Davidson, Mike Dietze
##' @examples
##' \dontrun{
##'   data('trait.mcmc', package = 'PEcAn.utils')
##'   data('prior.distns', package = 'PEcAn.utils')
##'   approx.posterior(trait.mcmc, priors = prior.distns)
##' }
approx.posterior <- function(trait.mcmc, priors, trait.data = NULL, outdir = NULL, filename.flag = "") {
  
  ## initialization
  posteriors <- priors
  do.plot <- !is.null(outdir)
  if (do.plot == TRUE) {
    grDevices::pdf(file.path(outdir, paste("posteriors", filename.flag, ".pdf", sep = "")))
  }
  
  ## loop over traits
  for (trait in names(trait.mcmc)) {
    
    dat <- trait.mcmc[[trait]]
    vname <- colnames(dat[[1]])
    if ("beta.o" %in% vname) {
      dat <- as.matrix(dat)[, "beta.o"]
    }
    
    pdist <- priors[trait, "distn"]
    pparm <- as.numeric(priors[trait, 2:3])
    ptrait <- trait
    

    ## first determine the candidate set of models based on any range restrictions
    zerobound <- c("exp", "gamma", "lnorm", "weibull")
    if (pdist %in% "beta") {
      m   <- mean(dat)
      v   <- stats::var(dat)
      k   <- (1 - m)/m
      a   <- (k / ((1 + k) ^ 2 * v) - 1) / (1 + k)
      b   <- a * k
      fit <- try(suppressWarnings(MASS::fitdistr(dat, "beta", list(shape1 = a, shape2 = b))), silent = TRUE)
      
      if (do.plot) {
        x <- seq(0, 1, length = 1000)
        plot(stats::density(dat), col = 2, lwd = 2, main = trait)
        if (!is.null(trait.data)) {
          graphics::rug(trait.data[[trait]]$Y, lwd = 2, col = "purple")
        }
        graphics::lines(x, stats::dbeta(x, fit$estimate[1], fit$estimate[2]), lwd = 2, type = "l")
        graphics::lines(x, stats::dbeta(x, pparm[1], pparm[2]), lwd = 3, type = "l", col = 3)
        graphics::legend("topleft", 
               legend = c("data", "prior", "post", "approx"), 
               col = c("purple", 3, 2, 1), lwd = 2)
      }
      posteriors[trait, "parama"] <- fit$estimate[1]
      posteriors[trait, "paramb"] <- fit$estimate[2]
    } else if (pdist %in% zerobound || (pdist == "unif" & pparm[1] >= 0)) {
      dist.names <- c("exp", "lnorm", "weibull", "norm")
      fit <- list()
      fit[[1]] <- try(suppressWarnings(MASS::fitdistr(dat, "exponential")), silent = TRUE)
      ## fit[[2]] <- fitdistr(dat,'f',list(df1=10,df2=2*mean(dat)/(max(mean(dat)-1,1))))
      fit[[2]] <- try(suppressWarnings(MASS::fitdistr(dat, "lognormal")), silent = TRUE)
      fit[[3]] <- try(suppressWarnings(MASS::fitdistr(dat, "weibull")), silent = TRUE)
      fit[[4]] <- try(suppressWarnings(MASS::fitdistr(dat, "normal")), silent = TRUE)
      
      if (!trait == "cuticular_cond") {
        fit[[5]] <- try(suppressWarnings(MASS::fitdistr(dat, "gamma")), silent = TRUE)
        dist.names <- c(dist.names, "gamma")
      }
      failfit.bool <- sapply(fit, class) == "try-error"
      fit[failfit.bool] <- NULL
      dist.names <- dist.names[!failfit.bool]
      
      fparm <- lapply(fit, function(x) { as.numeric(x$estimate) })
      fAIC <- lapply(fit, stats::AIC)
      
      bestfit <- which.min(fAIC)
      posteriors[ptrait, "distn"] <- dist.names[bestfit]
      posteriors[ptrait, "parama"] <- fit[[bestfit]]$estimate[1]
      if (bestfit == 1) {
        posteriors[ptrait, "paramb"] <- NA
      } else {
        posteriors[ptrait, "paramb"] <- fit[[bestfit]]$estimate[2]
      }

      if (do.plot) {
        .dens_plot(posteriors, priors, ptrait, dat, trait, trait.data)
      }
    } else {
      ## default: NORMAL
      posteriors[trait, "distn"] <- "norm"
      posteriors[trait, "parama"] <- mean(dat)
      posteriors[trait, "paramb"] <- stats::sd(dat)
      if (do.plot) {
        .dens_plot(posteriors, priors, ptrait, dat, trait, trait.data)
      }
    }
  }  ## end trait loop
  
  if (do.plot) {
    grDevices::dev.off()
  }
  
  return(posteriors)
} # approx.posterior


.dens_plot <- function(posteriors, priors, ptrait, dat, trait, trait.data,
                      plot_quantiles = c(0.01, 0.99)) {
  f <- function(x) {
    cl <- call(paste0("d", posteriors[ptrait, "distn"]),
               x,
               posteriors[ptrait, "parama"], 
               posteriors[ptrait, "paramb"])
    eval(cl)
  } # f

  fq <- function(x) {
    cl <- call(paste0("q", priors[ptrait, "distn"]),
               x, 
               priors[ptrait, "parama"], 
               priors[ptrait, "paramb"])
    eval(cl)
  } # fq

  fp <- function(x) {
    cl <- call(paste0("d", priors[ptrait, "distn"]), 
               x, 
               priors[ptrait, "parama"], 
               priors[ptrait, "paramb"])
    eval(cl)
  } # fp
  
  qbounds <- fq(plot_quantiles)
  x <- seq(qbounds[1], qbounds[2], length = 1000)
  rng <- range(dat)
  if (!is.null(trait.data)) {
    rng <- range(trait.data[[trait]]$Y)
  }

  plot(stats::density(dat), col = 2, lwd = 2, main = trait, xlim = rng)
  if (!is.null(trait.data)) {
    graphics::rug(trait.data[[trait]]$Y, lwd = 2, col = "purple")
  }
  graphics::lines(x, f(x), lwd = 2, type = "l")
  graphics::lines(x, fp(x), lwd = 3, type = "l", col = 3)
  graphics::legend("topleft", 
         legend = c("data", "prior", "post", "approx"), 
         col = c("purple", 3, 2, 1), lwd = 2)
}
