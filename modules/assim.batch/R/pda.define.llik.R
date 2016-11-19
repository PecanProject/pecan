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
 
  llik.fn <- list()
  
  for (i in seq_along(settings$assim.batch$inputs)) {
    
    # heteroskedastic Laplace likelihood, error stats is the likelihood
    if (settings$assim.batch$inputs[[i]]$likelihood == "Laplace") {
      
      llik.fn[[i]] <- function(pda.errors, ...) {
        LL <- pda.errors
        return(LL)
      }
      

    } else { # Gaussian or multiplicative Gaussian
      
      llik.fn[[i]] <- function(pda.errors, llik.par) {
        # lnL = (n/2) * log(tau) - (tau/2) * SS
        LL <- (llik.par$n/2) * log(llik.par$par) - (llik.par$par/2) * pda.errors
        return(LL)
      }
      
    } # if-block
  } # for-loop
  
  return(llik.fn)
} # pda.define.llik.fn



##' Calculate Likelihood parameters
##'
##' @title Calculate Likelihood parameters
##' @param settings list
##' @param model_out list
##' @param inputs list
##' @param bias.terms matrix
##'
##' @return pda.errors 
##'
##' @author Istem Fer
##' @export
pda.calc.error <-function(settings, con, model_out, run.id, inputs, bias.terms){
  

  n.input <- length(inputs)
  pda.errors <- list()
  
  
  for (k in seq_len(n.input)) {
    
    if (settings$assim.batch$inputs[[k]]$likelihood == "Laplace") {
      
      resid <- abs(model_out[[k]] - inputs[[k]]$obs)
      pos <- (model_out[[k]] >= 0)
      SS <- c(dexp(resid[pos],
                   1 / (inputs[[k]]$par[1] + inputs[[k]]$par[2] * model_out[[k]][pos]),
                   log = TRUE),
              dexp(resid[!pos],
                   1 / (inputs[[k]]$par[1] + inputs[[k]]$par[3] * model_out[[k]][!pos]),
                   log = TRUE))
      
      pda.errors[[k]] <- sum(SS, na.rm = TRUE) 
      
    } else { # Gaussian(s)
      
      
      SS <- rep(NA, length(bias.terms))
      for(b in seq_along(SS)){
        SS[b] <- sum((bias.terms[b] * model_out[[k]] - inputs[[k]]$obs)^2, na.rm = TRUE)
      }
      
      pda.errors[[k]] <- SS 
      
    }
    
  } # for-loop
  
  ## TODO: insert error records in database

  return(pda.errors)
  
} # pda.calc.error


##' Calculate Likelihoods for PDA
##'
##' @title Calculate Likelihoods for PDA
##' @param all params are the identically named variables in pda.mcmc / pda.emulator
##'
##' @return Total log likelihood (i.e., sum of log likelihoods for each dataset)
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.calc.llik <- function(pda.errors, llik.fn, llik.par) {
  
  n.var <- length(pda.errors)
  
  LL.vec <- numeric(n.var)
  n.vec <- sapply(llik.par, `[[`, "n")
  
  for (k in seq_len(n.var)) {
    LL.vec[k] <- llik.fn[[k]](pda.errors[k], llik.par[[k]])
  }
  
  weights <- rep(1 / n.var, n.var)  # TODO: Implement user-defined weights
  LL.total <- sum(LL.vec * weights)
  neff <- n.vec * weights
  
  # ## insert Likelihood records in database
  # if (!is.null(con)) {
  #   now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  #   
  #   # BETY requires likelihoods to be associated with inputs, so only proceed 
  #   # for inputs with valid input ID (i.e., not the -1 dummy id). 
  #   # Note that analyses requiring likelihoods to be stored therefore require 
  #   # inputs to be registered in BETY first.
  #   db.input.ind <- which(sapply(inputs, function(x) x$input.id) != -1)
  #   for (k in db.input.ind) {
  #     db.query(
  #       paste0("INSERT INTO likelihoods ", 
  #              "(run_id,            variable_id,                     input_id, ",
  #              " loglikelihood,     n_eff,                           weight,   ",
  #              " created_at) ",
  #              "values ('", 
  #              run.id, "', '",    inputs[[k]]$variable.id, "', '", inputs[[k]]$input.id, "', '", 
  #              LL.vec[k], "', '", floor(neff[k]), "', '",          weights[k] , "', '", 
  #              now,"')"
  #       ), 
  #       con)
  #   }
  # }
  
  return(LL.total)
} # pda.calc.llik


pda.calc.llik.par <-function(settings, n, error.stats){
  
  llik.par <- list()
  # llik.priors <- read.csv("~/pecan/modules/assim.batch/inst/llik.params.csv")
  # llik.priors <- read.csv(system.file("inst/llik.params.csv", package = "PEcAn.assim.batch"))
  for(k in seq_along(error.stats)){
    
    llik.par[[k]] <- list()
    
    if (settings$assim.batch$inputs[[k]]$likelihood == "Gaussian" |
        settings$assim.batch$inputs[[k]]$likelihood == "multipGauss") {
      
      llik.par[[k]]$par <- rgamma(1, 0.001 + n[k]/2, 0.001 + error.stats[k]/2)
    }
    
    llik.par[[k]]$n <- n[k]
  }

  return(llik.par)
  
} # pda.calc.llik.par


