# Functions for processing output

# Perform burn-in and thinning of MCMC samples
burnin.thin <- function(samples,
                        target = 5000,
                        burnin.ratio = 2,
                        auto = TRUE,
                        burnin = NULL,
                        thin = NULL){
    ngibbs <- nrow(samples)
    if(automatic) {
        burnin <- floor(ngibbs / burnin.ratio)
        thin <- floor((ngibbs - burnin) / target)
    }
    bt <- seq(burnin, ngibbs, by=thin)
    samples.bt <- samples[bt,]
    return(samples.bt)
}

# Load an object from an RData file with a custom name
load.from.name <- function(filename, filepath="."){
    f.path <- file.path(filepath, filename)
    load(f.path)
    f.name <- gsub("(.*)[.]RData", "\\1", filename)
    f.get <- get(f.name)
    return(f.get)
}

# Fit multivariate normal to samples. Return means and covariance matrix as a 
# long list (for easy construction of data.tables)
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

# Calculate simple univariate summary statistics and return as named list
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
