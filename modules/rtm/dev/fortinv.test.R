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
    return(z[[length(z)]][,1])
}

invert_basic <- function(modname, obs, inits, cons, 
                   pmu, psd, plog, minp, ngibbs){
    if(modname == "prospect_5b"){
        print("PROSPECT 5B model")
        names.all <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm")
    } else {
        print("Error: No model found")
        return
    }
    names.inits <- names(inits)
    names.cons <- names(cons)
    npars <- length(inits)
    ipars <- match(names.inits, names.all)
    ncons <- length(cons)
    icons <- match(names.cons, names.all)

    observed <- as.matrix(obs)
    nspec <- ncol(observed)

    ngibbs <- as.integer(ngibbs)
    results <- matrix(0, ngibbs, npars+1)

    in.list <- list("invert_basic", observed, nspec, modname,
                    inits, npars, ipars, cons, ncons, icons,
                    pmu, psd, plog, minp, ngibbs, results)

    t1 <- proc.time()
    out.list <- do.call(.Fortran, in.list)
    t2 <- proc.time()
    print(t2 - t1)
    return(out.list[[length(out.list)]])
}

# Test inversion
obs <- prospect()
nms <- c("N", "Cab", "Car", "Cbrown", "Cw", "Cm", "rsd")
inits <- c("N"=1, "Cab"=10, "Car"=5, "Cw"=1e-4, "Cm"=1e-4)
cons <- c("Cbrown"=0)
minp <- c(1, 0, 0, 0, 0)
pmu <- c(0, 0, 0, 0, 0)
psd <- c(10, 10, 10, 10, 10)
plog <- rep(TRUE, 6)
ngibbs <- 100

results <- invert_basic("prospect_5b", obs, inits, cons,
                        pmu, psd, plog, minp, ngibbs)
par(mfrow=c(4,2))
for(i in 1:length(nms)) plot(results[,i], type='l', main=nms[i])

nstart <- floor(ng/2)
pars.final <- colMeans(results[-nstart:0, -7])
invspec <- prospect(pars.final)
plot(invspec, type='l', main="Accuracy", ylim=c(0,0.6))
lines(obs, col=2)
