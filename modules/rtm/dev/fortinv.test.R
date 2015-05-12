library(PEcAnRTM)

prospect <- function(){
    N <- 1.4
    Cab <- 30
    Car <- 10
    Cbrown <- 0.5
    Cw <- 0.004
    Cm <- 0.004
    z <- .Fortran("prospect_5b", N, Cab, Car, Cbrown, Cw, Cm,
                  matrix(0, 2101, 2))
    return(z[[7]][,1])
}

# Test inversion
obs <- prospect()
inits <- c(1, 10, 5, 0.1, 1e-4, 1e-4)
pmin <- c(1, 0, 0, 0, 0, 0)
ngibbs <- as.integer(40)
r.temp <- matrix(0, 40, 7)
f.list <- .Fortran("invert_basic", obs, inits, pmin, ngibbs, r.temp)
results <- f.list[[5]]
par(mfrow=c(2,2))
plot(results[,1], type='l')
plot(results[,2], type='l')
plot(results[,3], type='l')
plot(results[,5], type='l')

