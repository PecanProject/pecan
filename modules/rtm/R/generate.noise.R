# Generate autocorrelated noise for spectra
generate.noise <- function(n=2101, sigma=1e-4, p=700){
    x <- filter(rnorm(n, 0, sigma), filter=rep(1,p), circular=TRUE)
    return(x)
}
