#' Functions for default priors
lognorm.mu <- function(mean, sd) log(mean / sqrt(1 + (mean/sd)^2))
lognorm.sigma <- function(mean, sd) sqrt(log(1 + (mean/sd)^2))

prior.defaultvals.prospect <- function(sd.inflate = 3){
    pmean <- c("N" = 0.7,
               "Cab" =32.81,
               "Car" = 8.51,
               "Cw" =0.0129,
               "Cm" =0.0077)
    psd <- c("N" = 0.6, 
             "Cab" = 17.87,
             "Car" = 3.2,
             "Cw" = 0.0073,
             "Cm" = 0.0035)
    psd <- psd * sd.inflate
    pmu <- lognorm.mu(pmean, psd)
    psigma <- lognorm.sigma(pmean, psd)
    return(list(mu = pmu, sigma = psigma))
}

priorfunc.prospect <- function(pmu, psigma){
    prior <- function(params){
        if(is.null(names(params))) params[1] <- params[1] - 1
        else params["N"] <- params["N"] - 1
        priors <- dlnorm(params, pmu, psigma, log=TRUE)
        return(sum(priors))
    }
    return(prior)
}
