#' @name generate.noise
#' @title Generate autocorrelated spectral noise
#' @param n Length of output vector (default = 2101)
#' @param sigma Gaussian noise standard deviation (default=1e-4)
#' @param fw Filter width. Will be coerced to an odd number if even (default = 
#' 201).
#' @param fsd Scaling factor for filter standard deviation (default = 6)
generate.noise <- function(n=2101, sigma.noise=1e-4, sigma.filter=1, fw=201, fsd=6){
    if(fw %% 2 == 0) fw <- fw + 1   # fw must be odd
    f.in <- 1:fw
    f.raw <- dnorm(f.in, median(f.in), fw/fsd)
    f <- f.raw * 1/max(f.raw)
    x <- filter(rnorm(n, 0, sigma.noise), filter=f, circular=TRUE)
    return(x)
}
