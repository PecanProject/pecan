library(PEcAnRTM)

prospect <- function(params = c(1.4, 30, 10, 0.5, 0.004, 0.004)){
    N <- params[1]
    Cab <- params[2]
    Car <- params[3]
    Cbrown <- params[4]
    Cw <- params[5]
    Cm <- params[6]
    z <- .Fortran("prospect_5b", N, Cab, Car, Cbrown, Cw, Cm,
                  matrix(0, 2101, 2))
    return(z[[7]][,1])
}

# Test inversion
obs <- prospect()
inits <- c(1, 10, 5, 0.1, 1e-4, 1e-4)
pm <- c(1, 0, 0, 0, 0, 0)
ng <- 10000
ngibbs <- as.integer(ng)
r.temp <- matrix(0, ng, 7)
t1 <- proc.time()
f.list <- .Fortran("invert_basic", obs, inits, pm, ngibbs, r.temp)
t2 <- proc.time()
print(t2 - t1)
results <- f.list[[5]]
nms <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm", "rsd")
par(mfrow=c(4,2))
for(i in 1:length(nms)) plot(results[,i], type='l', main=nms[i])

pars.final <- colMeans(results[-ng/2:0, -7])
invspec <- prospect(pars.final)
plot(invspec, type='l', main="Accuracy")
lines(obs, col=2)
