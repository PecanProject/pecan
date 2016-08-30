#' @name getBurnin
#' @title Calculate burnin value
#' @description Use `coda::gelman_plot` to calculate first iteration number where Gelman diagnostic never exceeds the specified threshold.
#' @param jags_out List of MCMC sample matrices or `mcmc.list` object
#' @param threshold Maximum value of Gelman diagnostic
#' @param use.confidence Logical. If TRUE (default), use 95% confidence interval for Gelman Diagnostic.
#' If FALSE, use the point estimate.
#' @inheritParams coda::gelman.diag
#' @param ... Other parameters to `gelman.plot`
#' @examples
#'      library(coda)
#'      data(line)
#'      burnin <- getBurnin(line, threshold = 1.05)
#' @author Michael Dietze, Alexey Shiklomanov
#' @export
getBurnin <- function(jags_out, threshold = 1.1, use.confidence = TRUE, autoburnin = FALSE,
                      plotfile = "/dev/null", ...) {
    library(coda)
    if (length(find("logger.info")) == 0) {
        msg <- message
    } else {
        msg <- logger.info
    }
    if (!is.mcmc.list(jags_out)) jags_out <- makeMCMCList(jags_out)
    png(plotfile)
    GBR <- try(gelman.plot(jags_out, autoburnin = autoburnin, ...))
    dev.off()
    if (class(GBR) == "try-error") {
        msg("Unable to calculate Gelman diagnostic. Assuming no convergence.")
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
        msg("*** Chains have not converged yet ***")
        mvals <- as.data.frame(matrix(gbr_values, nrow(gbr_values), ncol(gbr_values)))
        colnames(mvals) <- colnames(gbr_value)
        mex <- as.data.frame(matrix(gbr_exceed, nrow(gbr_exceed), ncol(gbr_exceed)))
        colnames(mex) <- sprintf("PSRF %s > %.2f", colnames(gbr_exceed), threshold)
        print(cbind(tail(mvals), tail(mex)))
        burnin <- 1
    }
    return(burnin)
}

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
autoburnin <- function(jags_out, return.burnin = FALSE, ...){
    burnin <- getBurnin(jags_out, ...)
    if (burnin == 1) return(jags_out)
    out <- window(jags_out, start = burnin)
    if (return.burnin) {
        out <- list(samples = out, burnin = burnin)
    }
    return(out)
}

#' @name makeMCMCList
#' @title Make MCMC list from samples list
#' @param samps samples list (output from invert.custom)
#' @export
makeMCMCList <- function(samps) {
    samps.mcmc <- lapply(samps, mcmc)
    stopifnot(all(sapply(samps.mcmc, is.mcmc)))
    samps.mcmc.list <- mcmc.list(samps.mcmc)
    stopifnot(is.mcmc.list(samps.mcmc.list))
    return(samps.mcmc.list)
}

