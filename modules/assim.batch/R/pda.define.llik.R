##' Define PDA Likelihood Functions
##'
##' @title Define PDA Likelihood Functions
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return List of likelihood functions, one for each dataset to be assimilated against.
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.define.llik.fn <- function(settings) {
  # Currently just returns a single likelihood, assuming the data are flux NEE/FC or LE.
  llik.fn <- list()
  for (i in seq_along(settings$assim.batch$inputs)) {
    # NEE + heteroskedastic Laplace likelihood
    if (settings$assim.batch$inputs[[i]]$likelihood == "Laplace") {
      llik.fn[[i]] <- function(model.out, obs.data, llik.par) {
        resid <- abs(model.out - obs.data)
        pos <- (model.out >= 0)
        LL <- c(dexp(resid[pos], 
                     1 / (llik.par[1] + llik.par[2] * model.out[pos]), 
                     log = TRUE), 
                dexp(resid[!pos],
                     1 / (llik.par[1] + llik.par[3] * model.out[!pos]), 
                     log = TRUE))
        return(list(LL = sum(LL, na.rm = TRUE), n = sum(!is.na(LL))))
      }
    } else {
      llik.fn[[i]] <- function(model.out, obs.data, llik.par) {
        if (is.list(model.out)) {
          model.out <- unlist(model.out)
        }
        if (is.list(obs.data)) {
          obs.data <- unlist(obs.data)
        }
        # lnL = (n/2) * log(tau) - (tau/2) * SS
        LL <- (llik.par[1]/2) * log(llik.par[2]) - (llik.par[2]/2) * sum((model.out - obs.data)^2, na.rm = TRUE)
        return(list(LL = LL, n = llik.par[1]))
      }
    }
  }
  
  return(llik.fn)
} # pda.define.llik.fn


##' Calculate Likelihoods for PDA
##'
##' @title Calculate Likelihoods for PDA
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return Total log likelihood (i.e., sum of log likelihoods for each dataset)
##'
##' @author Ryan Kelly
##' @export
pda.calc.llik <- function(settings, con, model.out, run.id, inputs, llik.fn) {
  
  n.input <- length(inputs)
  
  LL.vec <- n.vec <- numeric(n.input)
  
  for (k in seq_len(n.input)) {
    
    if (all(is.na(model.out))) {
      # Probably indicates model failed entirely
      return(-Inf)
    }
    
    llik <- llik.fn[[k]](model.out[[k]], inputs[[k]]$obs, inputs[[k]]$par)
    LL.vec[k] <- llik$LL
    n.vec[k] <- llik$n
  }
  
  weights <- rep(1 / n.input, n.input)  # TODO: Implement user-defined weights
  LL.total <- sum(LL.vec * weights)
  neff <- n.vec * weights
  
  ## insert Likelihood records in database
  if (!is.null(con)) {
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # BETY requires likelihoods to be associated with inputs, so only proceed 
    # for inputs with valid input ID (i.e., not the -1 dummy id). 
    # Note that analyses requiring likelihoods to be stored therefore require 
    # inputs to be registered in BETY first.
    db.input.ind <- which(sapply(inputs, function(x) x$input.id) != -1)
    for (k in db.input.ind) {
      db.query(
        paste0("INSERT INTO likelihoods ", 
               "(run_id,            variable_id,                     input_id, ",
               " loglikelihood,     n_eff,                           weight,   ",
               " created_at) ",
               "values ('", 
               run.id, "', '",    inputs[[k]]$variable.id, "', '", inputs[[k]]$input.id, "', '", 
               LL.vec[k], "', '", floor(neff[k]), "', '",          weights[k] , "', '", 
               now,"')"
        ), 
        con)
    }
  }
  
  return(LL.total)
} # pda.calc.llik


##' Calculate Likelihood parameters
##'
##' @title Calculate Likelihood parameters
##' @param settings list
##' @param model.out list
##' @param inputs list
##'
##' @return inputs updated inputs list with likelihood parameters
##'
##' @author Istem Fer
##' @export
pda.calc.error <-function(settings, model_out, inputs){
  
  # llik.priors <- read.csv("~/pecan/modules/assim.batch/inst/llik.params.csv")
  llik.priors <- read.csv(system.file("inst/llik.params.csv", package = "PEcAn.assim.batch"))
  
  for (k in seq_len(n.input)) {
    
    if(settings$assim.batch$inputs[[k]]$likelihood == "Gaussian"){
      gauss.priors <- llik.priors[llik.priors$likelihood == "Gaussian",]
      SS <- sum((inputs[[k]]$obs - model_out[[k]])^2, na.rm = TRUE)
      tau <- rgamma(1, gauss.priors$parama + nrow(inputs[[k]]$obs)/2, gauss.priors$paramb+SS/2)
      inputs[[k]]$par <- c(nrow(inputs[[k]]$obs), tau)
    }
    
  }
  
  return(inputs)
  
} # pda.calc.llikpar


