#' @name getBurnin
#' @title Calculate burnin value
#' @description Use `coda::gelman_plot` to calculate first iteration number where Gelman diagnostic never exceeds the specified threshold.
#' @param jags_out List of MCMC sample matrices or `mcmc.list` object
#' @param threshold Maximum value of Gelman diagnostic
#' @param ... Other parameters to `gelman.plot`
#' @examples
#'      library(coda)
#'      data(line)
#'      burnin <- getBurnin(line, threshold = 1.05)
#' @author Michael Dietze, Alexey Shiklomanov
#' @export
getBurnin <- function(jags_out, threshold = 1.1, ...) {
    library(coda)
    if (length(find("logger.info")) == 0) {
        msg <- message
    } else {
        msg <- logger.info
    }
    if (!is.mcmc.list(jags_out)) jags_out <- makeMCMCList(jags_out)
    png("/dev/null")
    GBR <- try(gelman.plot(jags_out, autoburnin=FALSE, ...))
    dev.off()
    if (class(GBR) == "try-error") {
        msg("Unable to calculate Gelman diagnostic. Assuming no convergence.")
        return(1)
    }
    gbr_shrink <- GBR$shrink[,,2] > threshold
    if (all(!gbr_shrink)) {
        # Chains converged instantly -- no burnin required
        burnin <- 1
    } else {
        burnin <- GBR$last.iter[tail(which(rowSums(gbr_shrink) > 0), 1) + 1]
    }
    if (is.na(burnin)) {
        msg("*** Chains have not converged yet ***")
        burnin <- 1
    }
    return(burnin)
}

#' @name autoburnin
#' @title Automatically calculate and apply burnin value
#' @author Michael Dietze, Alexey Shiklomanov
#' @param return.burnin Logical. If `TRUE`, return burnin value in addition to samples (as list). 
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
        out <- list(samples = samples, burnin = burnin)
    }
    return(out)
}

