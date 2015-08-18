invert.mle <- function(observed, inits, constants, model){
    observed <- as.matrix(observed)
    merit <- function(params, constants){
        mod.params <- params[-length(params)]
        rsd <- params[length(params)]
        spec <- model(mod.params, constants)
        error <- spec - observed
        logl <- sum(dnorm(error, 0, rsd, 1))
        return(-logl)
    }
    fit <- optim(inits, merit, constants=constants)
    return(fit)
}
