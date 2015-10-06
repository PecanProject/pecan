# Functions for processing output

#' @name burnin.thin
#' @title Burn-in and thinning of MCMC samples
#' @param samples Matrix of MCMC samples
#' @param target Target number of samples (default = 5000). Only applicable if 
#' auto=TRUE.
#' @param burnin.ratio Fraction of samples to burn-in; i.e. 2 means to remove 
#' first 1/2 of samples, 3 means 1/3, etc. (default = 2). Only applicable if 
#' auto=TRUE.
#' @param auto Whether or not to perform automatic burnin and thin based on 
#' target number of samples.
#' @param burnin Number of samples to discard as burnin (auto must be FALSE)
#' @param thin Thinning interval (auto must be FALSE)
burnin.thin <- function(samples,
                        target = 5000,
                        burnin.ratio = 2,
                        auto = TRUE,
                        burnin = NULL,
                        thin = NULL){
    ngibbs <- nrow(samples)
    if(auto) {
        burnin <- floor(ngibbs / burnin.ratio)
        thin <- floor((ngibbs - burnin) / target)
        if(thin < 1){
            warning("Fewer than target samples after burnin. No thinning applied.")
            thin <- 1
        }
    }
    bt <- seq(burnin, ngibbs, by=thin)
    samples.bt <- samples[bt,]
    return(samples.bt)
}

#' @name load.from.name
#' @title Load object from an RData file
#' @param filename Full name (without path!) of RData file
#' @param filepath Path of RData file (default=".")
load.from.name <- function(filename, filepath="."){
    f.path <- file.path(filepath, filename)
    load(f.path)
    f.name <- gsub("(.*)[.]RData", "\\1", filename)
    f.get <- get(f.name)
    return(f.get)
}

#' @name summary.mvnorm
#' @title Multivariate normal fit
#' @description Fit multivariate normal to samples. Return means and covariance 
#' matrix as a long list (for easy construction of data.tables)
#' @param samples Matrix of MCMC samples.
summary.mvnorm <- function(samples){
    stopifnot(colnames(samples) != NULL)
    parnames <- colnames(samples)
    sigmanames <- sprintf("%s.%s.sigma", rep(parnames, 1, each=6),
                          rep(parnames, 6))
    fit <- mvn("XXX", samples)
    mu <- as.numeric(fit$parameters$mean)
    sigma <- c(fit$parameters$variance$Sigma)
    names(mu) <- sprintf("%s.mu", parnames)
    names(sigma) <- sigmanames
    out.list <- c(as.list(mu), as.list(sigma))
    return(out.list)
}

#' @name summary.simple
#' @title Simple summary statistics on MCMC samples
#' @description Calculate simple univariate summary statistics and return as 
#' named list
#' @param samples Matrix of MCMC samples
summary.simple <- function(samples){
    stopifnot(colnames(samples) != NULL)
    parnames <- colnames(samples)
    mu <- colMeans(samples, na.rm=TRUE)
    names(mu) <- sprintf("%s.mu", parnames)
    sigma <- apply(samples, 2, sd, na.rm=TRUE)
    names(sigma) <- sprintf("%s.sigma", parnames)
    q25 <- apply(samples, 2, quantile, 0.025, na.rm=TRUE)
    names(q25) <- sprintf("%s.q25", parnames)
    med <- apply(samples, 2, median, na.rm=TRUE)
    names(med) <- sprintf("%s.med", parnames)
    q975 <- apply(samples, 2, quantile, 0.975, na.rm=TRUE)
    names(q975) <- sprintf("%s.q975", parnames)
    out.list <- c(as.list(mu), as.list(sigma), as.list(q25),
                  as.list(med), as.list(q975))
    return(out.list)
}
