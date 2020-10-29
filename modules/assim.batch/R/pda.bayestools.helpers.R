##' Create priors for BayesianTools
##' 
##' Helper function for creating log-priors compatible with BayesianTools package
##'
##' @param prior.sel `data.frame` containing prior distributions of the selected parameters
##'
##' @return out Prior class object for BayesianTools package
##' @details `prior.sel` must contain the following columns:
##'   * `distn` -- String describing a distribution; e.g. `norm` for `dnorm`, `rnorm`, etc.
##'   * `parama`, `paramb` -- First and second parameters, respectively, of the corresponding distribution
##'
##' Optionally, `prior.sel` may also contain the following columns:
##'   * `param_name` -- Parameter name, which will be carried through to the prior object and sampler
##'   * `lower`, `upper` -- Lower and upper bounds, respectively. These can be leveraged by the BayesianTools samplers.
##'   * `best` -- Best guess for a parameter estimate. BayesianTools can also use this, though I'm not sure how...
##'
##' @author Istem Fer, Alexey Shiklomanov
##' @export
pda.create.btprior <- function(prior.sel) {
  
  # TODO: test exponential -- it only has one argument, so this won't work

  # Returns a function that calculates the density of the specified 
  # distribution given the parameters
  ddist_generator <- function(distn, a, b) {
    fun_string <- paste0('d', distn)
    f <- match.fun(fun_string)
    out <- function(x) f(x, a, b, log = TRUE)
    return(out)
  }

  # Returns a function that draws from the specified distribution with the 
  # specified parameters
  rdist_generator <- function(distn, a, b) {
    fun_string <- paste0('r', distn)
    f <- match.fun(fun_string)
    out <- function(n = 1) f(n, a, b)
    return(out)
  }

  # Create a list of density and random draw functions
  ddist_funs <- with(prior.sel, mapply(ddist_generator, distn, parama, paramb))
  rdist_funs <- with(prior.sel, mapply(rdist_generator, distn, parama, paramb))
  if ('param_name' %in% names(prior.sel)) {
    names(ddist_funs) <- names(rdist_funs) <- prior.sel[['param_name']]
  }

  # `mapply` statement returns
  density <- function(params) {
    dens_vec <- mapply(function(f, x) f(x), ddist_funs, params)   # Returns vector of log densities
    out <- sum(dens_vec)
    return(out)
  }

  # Returns vector of random draws
  sampler <- function() {
    out <- vapply(rdist_funs, function(f) f(), numeric(1))
    return(out)
  }

  # BayesianTools lower and upper bounds and best guess, if specified in data.frame
  lower <- NULL
  if ('lower' %in% names(prior.sel)) {
      lower <- prior.sel[['lower']]
  }
  upper <- NULL
  if ('upper' %in% names(prior.sel)) {
      upper <- prior.sel[['upper']]
  }
  best <- NULL
  if ('best' %in% names(prior.sel)) {
      best <- prior.sel[['best']]
  }
  
  # Use createPrior{BayesianTools} function to create prior class object compatible
  # with rest of the functions
  out <- BayesianTools::createPrior(density = density, sampler = sampler, 
                                    lower = lower, upper = upper, best = best)
  return(out)
} # pda.create.btprior


##' Helper function for applying BayesianTools specific settings from PEcAn general settings
##'
##' @title Apply settings for BayesianTools
##' @param settings PEcAn settings
##'
##' @return bt.settings list of runMCMC{BayesianTools} settings
##'
##' @author Istem Fer
##' @export
##' 
pda.settings.bt <- function(settings) {
  
  sampler <- settings$assim.batch$bt.settings$sampler
  
  iterations <- as.numeric(settings$assim.batch$bt.settings$iter)
  optimize <- ifelse(!is.null(settings$assim.batch$bt.settings$optimize), 
                     settings$assim.batch$bt.settings$optimize, 
                     TRUE)
  # consoleUpdates = ifelse(!is.null(settings$assim.batch$bt.settings$consoleUpdates),
  # as.numeric(settings$assim.batch$bt.settings$consoleUpdates), max(round(iterations/10),100))
  adapt <- ifelse(!is.null(settings$assim.batch$bt.settings$adapt), 
                  settings$assim.batch$bt.settings$adapt, 
                  TRUE)
  adaptationInverval = ifelse(!is.null(settings$assim.batch$bt.settings$adaptationInverval),
                              as.numeric(settings$assim.batch$bt.settings$adaptationInverval),
                              max(round(iterations/100*5),100))
  adaptationNotBefore <- ifelse(!is.null(settings$assim.batch$bt.settings$adaptationNotBefore), 
                                as.numeric(settings$assim.batch$bt.settings$adaptationNotBefore), 
                                adaptationInverval)
  DRlevels <- ifelse(!is.null(settings$assim.batch$bt.settings$DRlevels),
                     as.numeric(settings$assim.batch$bt.settings$DRlevels), 
                     1)
  if (!is.null(settings$assim.batch$bt.settings$gibbsProbabilities)) {
    gibbsProbabilities <- as.numeric(unlist(settings$assim.batch$bt.settings$gibbsProbabilities))
  } else {
    gibbsProbabilities <- NULL
  }
  
  if (sampler == "Metropolis") {
    bt.settings <- list(iterations = iterations,
                        optimize = optimize, 
                        DRlevels = DRlevels, 
                        adapt = adapt, 
                        adaptationNotBefore = adaptationNotBefore,
                        gibbsProbabilities = gibbsProbabilities)
  } else if (sampler %in% c("AM", "M", "DRAM", "DR")) {
    bt.settings <- list(iterations = iterations, startValue = "prior")
  } else if (sampler %in% c("DE", "DEzs", "DREAM", "DREAMzs", "Twalk")) {
    bt.settings <- list(iterations = iterations)
  } else if (sampler == "SMC") {
    bt.settings <- list(initialParticles = list("prior", iterations))
  } else {
    PEcAn.logger::logger.error(paste0(sampler, " sampler not found!"))
  }
  
  return(bt.settings)
} # pda.settings.bt

#' Flexible function to create correlation density plots
#' 
#' numeric matrix or data.frame
#' @author Florian Hartig
#' @param mat matrix or data frame of variables
#' @param density type of plot to do
#' @param thin thinning of the matrix to make things faster. Default is to thin to 5000 
#' @param method method for calculating correlations
#' @param whichParameters all params or some
#' @references The code for the correlation density plot originates from Hartig, F.; Dislich, C.; Wiegand, T. & Huth, A. (2014) Technical Note: Approximate Bayesian parameterization of a process-based tropical forest model. Biogeosciences, 11, 1261-1272.
#' @export
#' 
correlationPlot <- function(mat, density = "smooth", thin = "auto", method = "pearson", whichParameters = NULL) {
  
  if (inherits(mat, "bayesianOutput")) {
    mat <- BayesianTools::getSample(mat, thin = thin, whichParameters = whichParameters)
  }
  
  numPars <- ncol(mat)
  names <- colnames(mat)
  
  panel.hist.dens <- function(x, ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr), add = TRUE)
    graphics::par(usr = c(usr[1:2], 0, 1.5))
    h <- graphics::hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    graphics::rect(breaks[-nB], 0, breaks[-1], y, col = "blue4", ...)
  } # panel.hist.dens
  
  # replaced by spearman
  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr), add = TRUE)
    graphics::par(usr = c(0, 1, 0, 1))
    r <- stats::cor(x, y, use = "complete.obs", method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if (missing(cex.cor)) {
      cex.cor <- 0.8 / graphics::strwidth(txt)
    }
    graphics::text(0.5, 0.5, txt, cex = cex.cor * abs(r))
  } # panel.cor
  
  plotEllipse <- function(x, y) {
    usr <- graphics::par("usr")
    on.exit(graphics::par(usr), add = TRUE)
    graphics::par(usr = c(usr[1:2], 0, 1.5))
    cor <- stats::cor(x, y)
    el <- ellipse::ellipse(cor)
    graphics::polygon(el[, 1] + mean(x), el[, 2] + mean(y), col = "red")
  } # plotEllipse
  
  correlationEllipse <- function(x) {

    cor <- stats::cor(x)
    ToRGB <- function(x) {
      grDevices::rgb(x[1] / 255, x[2] / 255, x[3] / 255)
    }
    C1 <- ToRGB(c(178, 24, 43))
    C2 <- ToRGB(c(214, 96, 77))
    C3 <- ToRGB(c(244, 165, 130))
    C4 <- ToRGB(c(253, 219, 199))
    C5 <- ToRGB(c(247, 247, 247))
    C6 <- ToRGB(c(209, 229, 240))
    C7 <- ToRGB(c(146, 197, 222))
    C8 <- ToRGB(c(67, 147, 195))
    C9 <- ToRGB(c(33, 102, 172))
    CustomPalette <- grDevices::colorRampPalette(rev(c(C1, C2, C3, C4, C5, C6, C7, C8, C9)))
    ord <- order(cor[1, ])
    xc <- cor[ord, ord]
    colors <- unlist(CustomPalette(100))
    ellipse::plotcorr(xc, col = colors[xc * 50 + 50])
  } # correlationEllipse
  
  if (density == "smooth") {
    ellipse::pairs(mat, lower.panel = function(...) {
      graphics::par(new = TRUE)
      IDPmisc::ipanel.smooth(...)
    }, diag.panel = panel.hist.dens, upper.panel = panel.cor)
  } else if (density == "corellipseCor") {
    ellipse::pairs(mat, lower.panel = plotEllipse, diag.panel = panel.hist.dens, upper.panel = panel.cor)
  } else if (density == "ellipse") {
    correlationEllipse(mat)
  } else if (density == F) {
    ellipse::pairs(mat, lower.panel = panel.cor, diag.panel = panel.hist.dens, upper.panel = panel.cor)
  } else stop("wrong sensity argument")
  
  # The if block above is generating return values
} # correlationPlot
