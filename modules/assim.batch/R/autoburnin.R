#' @name getBurnin
#' @title Calculate burnin value
#' @description Use `coda::gelman_plot` to calculate first iteration number where Gelman diagnostic never exceeds the specified threshold.
#' @param jags_out List of MCMC sample matrices or `mcmc.list` object
#' @param threshold Maximum value of Gelman diagnostic
#' @examples
#'      library(coda)
#'      data(line)
#'      burnin <- getBurnin(line, threshold = 1.05)
#' @author Michael Dietze, Alexey Shiklomanov
#' @export
getBurnin <- function(jags_out, threshold = 1.1) {
    library(coda)
    if (!is.mcmc.list(jags_out)) jags_out <- makeMCMCList(jags_out)
    png("/dev/null")
    GBR <- gelman.plot(jags_out, autoburnin=FALSE)
    dev.off()
    burnin <- GBR$last.iter[tail(which(rowSums(GBR$shrink[,,2] > threshold) > 0), 1) + 1]
    if (length(burnin) == 0) {
        if (length(find("logger.info")) == 0) {
            msg <- message
        } else {
            msg <- logger.info
        }
        msg("*** Chains have not converged yet ***")
        burnin <- 1
    }
    return(burnin)
}

#' @name autoburnin
#' @title Automatically calculate and apply burnin value
#' @author Michael Dietze, Alexey Shiklomanov
#' @inheritParams getBurnin
#' @export
autoburnin <- function(jags_out, ...){
    burnin <- getBurnin(jags_out, ...)
    # Apply burnin
    out <- window(jags_out, start = burnin)
    return(out)
}

