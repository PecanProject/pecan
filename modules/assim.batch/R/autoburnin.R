#' @name getBurnin
#' @title Calculate burnin value
#' @description Automatically detect burnin based on one of several methods.
#' @param jags_out List of MCMC sample matrices or `mcmc.list` object
#' @param method Character string indicating method ("rmw" or "gelman.plot")
#' @param threshold Maximum value of Gelman diagnostic
#' @param use.confidence Logical. If TRUE (default), use 95% confidence interval for Gelman Diagnostic.
#' If FALSE, use the point estimate.
#' @param width Width of window for `rmw` method. Default = niter / 2
#' @param njump Number of steps for `rmw` method. Default = 50 
#' @param plotfile Filename for a PNG of the Gelman plot. Default = '/dev/null'
#' @param ... Other parameters to `gelman.plot`
#' @inheritParams coda::gelman.diag
#' 
#' @details 
#' rmw (Reverse moving window) method divides the chains into `njump` sections, each of size `width`, and loops over them starting from the last one until the first block that has not converged.
#' The iteration starting the next block after that is the burnin.
#' 
#' `gelman.plot` method uses the `gelman.plot` function from the `coda` package to calculate the Gelman diagnostic cumulatively from the beginning of the chain.
#' The iteration after the last time the `gelman.diag` exceeds the threshold for any of the parameters is the burnin.
#' 
#' @examples
#'      library(coda)
#'      data(line)
#'      burnin <- getBurnin(line, threshold = 1.05, 
#'                          method = 'gelman.plot')
#' @author Alexey Shiklomanov, Michael Dietze
#' @export
getBurnin <- function(jags_out, 
                      method = "rmw", 
                      threshold = 1.1,
                      use.confidence = TRUE,
                      width = ceiling(niter(jags_out)/2), 
                      njump = 50,
                      autoburnin = FALSE,
                      plotfile = "/dev/null",
                      ...) {
  if (!coda::is.mcmc.list(jags_out)) jags_out <- makeMCMCList(jags_out)
  stopifnot(niter(jags_out) > 50)
  if (method == "rmw"){
    burnin <- getBurnin.rmw(jags_out, 
                            width = width,
                            njump = njump,
                            threshold = threshold,
                            use.confidence = use.confidence,
                            ...)
  } else if (method == "gelman.plot") {
    burnin <- getBurnin.gelman.plot(jags_out, 
                                    threshold = threshold,
                                    use.confidence = use.confidence,
                                    autoburnin = autoburnin,
                                    plotfile = plotfile,
                                    ...)
  } else {
    stop(sprintf("Method '%s' not valid. Only 'rmw' and 'gelman.plot' currently supported.", 
                method))
  }
  if (burnin == 1) message("*** Chains have not converged yet ***")
  return(burnin)
}

getBurnin.rmw <- function(x, width = ceiling(niter(x)/2), njump = 50,
                          threshold = 1.1, use.confidence = TRUE, ...) {
  stopifnot(width %% 1 == 0)
  stopifnot(njump %% 1 == 0)
  startx <- start(x)
  endx <- end(x)
  converged <- TRUE
  gdcol <- ifelse(use.confidence, 2, 1)
  a <- floor(seq(endx - width + 1, startx, length.out = njump))
  b <- ceiling(seq(endx, startx + width - 1, length.out = njump))
  i <- 1
  while(converged & i <= length(a)) {
    xsub <- window(x, start=a[i], end=b[i])
    gd_raw <- coda::gelman.diag(xsub, autoburnin=FALSE)
    gd <- c(gd_raw$psrf[,gdcol], gd_raw$mpsrf)
    if (any(gd > threshold)) {
      converged <- FALSE
      burnin <- max(a[i-1], 2)
      # This allows convergence checking with `burnin == 1`
      # Burnin will only be 1 here if the chains converge instantly, which is highly unlikely
      if (i <= 1) {
        burnin <- 1
      }
    }
    i <- i + 1
  }
  return (burnin)
} 


getBurnin.gelman.plot <- function(jags_out, threshold = 1.1, 
                                  use.confidence = TRUE, autoburnin = FALSE,
                                  plotfile = "/dev/null", ...) {
  png(plotfile)
  GBR <- try(coda::gelman.plot(jags_out, autoburnin = autoburnin, ...))
  dev.off()
  if (class(GBR) == "try-error") {
    message("Unable to calculate Gelman diagnostic. Assuming no convergence.")
    return(1)
  }
  column <- ifelse(use.confidence, 2, 1)
  gbr_values <- GBR$shrink[,, column, drop = FALSE]
  gbr_exceed <- gbr_values > threshold
  if (all(!gbr_exceed)) {
    # Chains converged instantly -- no burnin required
    burnin <- 2     # This isn't 1 to allow testing for convergence with `burnin == 1`
  } else {
    burnin <- GBR$last.iter[tail(which(rowSums(gbr_exceed) > 0), 1) + 1]
  }
  if (is.na(burnin)) {
    message("*** Chains have not converged yet ***")
    mvals <- as.data.frame(matrix(gbr_values, nrow(gbr_values), ncol(gbr_values)))
    colnames(mvals) <- colnames(gbr_values)
    mex <- as.data.frame(matrix(gbr_exceed, nrow(gbr_exceed), ncol(gbr_exceed)))
    colnames(mex) <- sprintf("PSRF %s > %.2f", colnames(gbr_exceed), threshold)
    print(cbind(tail(mvals), tail(mex)))
    burnin <- 1
  }
  return(burnin)
} # getBurnin.gelman.plot

#' @name autoburnin
#' @title Automatically calculate and apply burnin value
#' @author Michael Dietze, Alexey Shiklomanov
#' @param return.burnin Logical. If `TRUE`, return burnin value in addition to samples (as list). 
#' @param ... Additional arguments for `getBurnin`, `gelman.plot`, and `gelman.diag`
#' Default = FALSE.
#' @inheritParams getBurnin
#' @examples
#'      library(coda)
#'      data(line)
#'      line_burned <- autoburnin(line, threshold = 1.05, return.burnin=FALSE)
#' @export
autoburnin <- function(jags_out, return.burnin = FALSE, ...) {
  burnin <- getBurnin(jags_out, ...)
  if (burnin == 1) {
    return(jags_out)
  }
  out <- window(jags_out, start = burnin)
  if (return.burnin) {
    out <- list(samples = out, burnin = burnin)
  }
  return(out)
} # autoburnin

#' @name makeMCMCList
#' @title Make MCMC list from samples list
#' @param samps samples list (output from invert.custom)
#' @export
makeMCMCList <- function(samps) {
  samps.mcmc <- lapply(samps, mcmc)
  stopifnot(all(sapply(samps.mcmc, coda::is.mcmc)))
  samps.mcmc.list <- coda::mcmc.list(samps.mcmc)
  stopifnot(coda::is.mcmc.list(samps.mcmc.list))
  return(samps.mcmc.list)
} # makeMCMCList
