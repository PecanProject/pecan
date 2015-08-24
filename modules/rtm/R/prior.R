#' Functions for default priors
lognorm.mu <- function(mean, sd) log(mean / sqrt(1 + (mean/sd)^2))
lognorm.sigma <- function(mean, sd) sqrt(log(1 + (mean/sd)^2))

#' @name prior.defaultvals.prospect
#' @title Default prior parameters for PROSPECT models
#' @param sd.inflate Standard deviation multiplier (default = 3)
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

#' @name priorfunc.prospect
#' @title Default PROSPECT 5 prior function
#' @param pmu Lognormal mu parameter
#' @param psigma Lognormal sigma parameter
#' @details Assumes lognormal distribution for all parameters. NOTE that prior 
#' on N is shifted by 1.
priorfunc.prospect <- function(pmu, psigma){
    prior <- function(params){
        if(is.null(names(params))){
            warning("Parameters are not named. \n
                    Assuming N Cab (Car) (Cbrown) Cw Cm for priors")
            params[1] <- params[1] - 1
        }
        else params["N"] <- params["N"] - 1
        priors <- dlnorm(params, pmu, psigma, log=TRUE)
        return(sum(priors))
    }
    return(prior)
}
