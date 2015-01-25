##' @name sumtab
##' @title Summary table for MCMC chains
##' @details {
##' Produce summary table containing mean and confidence interval width for a given set of chains.
##' }

##' @param m 'mcmc.list' object from which chains will be extracted.
##' @return Returns matrix of means and confidence interval widths.
##' @export
##' 
##' @author Alexey Shiklomanov

sumtab <- function(m, m.name=deparse(substitute(m))) {
        s <- summary(m)
        means <- s$statistics[,1]
        ci.low <- s$quantiles[,1]
        ci.up <- s$quantiles[,5]
        ci.range <- ci.up - ci.low
        out <- data.frame(rbind(means, ci.low, ci.up, ci.range),
                          vartype = c("mean", "ci_low", "ci_up", "ci_range"), 
                          row.names = NULL)
        out$species <- m.name
        return(out)
}

##' @name summary.table
##' @title Large summary table
##' 

summary.table <- function(path=FALSE){
        if(path!=FALSE){
                get.results(path)
        }
        results.vec <- ls(".GlobalEnv")
        results.vec <- results.vec[grep("\\.l$", results.vec)]
        tablist <- lapply(results.vec, function(x) sumtab(get(x), x))
        results.tab <- do.call(rbind, tablist)
        return(results.tab)
}

