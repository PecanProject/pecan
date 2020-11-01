##' Define PDA Likelihood Functions
##'
##' @title Define PDA Likelihood Functions
##' @param settings PEcAn settings list
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
      
      llik.fn[[i]] <- function(pda.errors, llik.par) {
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



##' Calculate sufficient statistics
##'
##' @title Calculate sufficient statistics
##' @param settings list
##' @param con DB connection
##' @param model_out list
##' @param run.id run ID
##' @param inputs list
##' @param bias.terms matrix
##'
##' @return pda.errors 
##'
##' @author Istem Fer
##' @export
pda.calc.error <-function(settings, con, model_out, run.id, inputs, bias.terms){
  
  if(anyNA(model_out, recursive = TRUE)) {   # Probably indicates model failed entirely
    NA.list <- as.list(rep(NA, length(inputs)))
    return(NA.list)
  }

  n.input <- length(inputs)
  pda.errors <- list()
  SSdb <- list()
  # multiplicative Gaussian counter
  bc <- 1
  
  
  for (k in seq_len(n.input)) {
    
    if (settings$assim.batch$inputs[[k]]$likelihood == "Laplace") {
      # heteroskedastic laplacian
        
        resid <- abs(model_out[[k]] - inputs[[k]]$obs)
        pos <- (model_out[[k]] >= 0)
        # SS <- c(stats::dexp(resid[pos],
        #              1 / (inputs[[k]]$par[1] + (inputs[[k]]$par[2] * 
        #                                           sqrt(inputs[[k]]$n_eff/inputs[[k]]$n) * 
        #                                           model_out[[k]][pos])), log = TRUE),
        #         stats::dexp(resid[!pos],
        #              1 / (inputs[[k]]$par[1] + (inputs[[k]]$par[3] * 
        #                     sqrt(inputs[[k]]$n_eff/inputs[[k]]$n) * 
        #                     model_out[[k]][!pos])), log = TRUE))
        # 
        # pda.errors[[k]] <- sum(SS, na.rm = TRUE) 
        # SSdb[[k]] <- sum(SS, na.rm = TRUE) 
        
        # heteroskedastic slopes, slope varies with magnitude of the flux 
        # inflated by sqrt(n/neff) because var is 2b^2 for laplacian likelihood
        beta_p <- (inputs[[k]]$par[1] + inputs[[k]]$par[2] * model_out[[k]][pos]) * sqrt(inputs[[k]]$n/inputs[[k]]$n_eff)  
        beta_n <- (inputs[[k]]$par[1] + inputs[[k]]$par[3] * model_out[[k]][!pos])* sqrt(inputs[[k]]$n/inputs[[k]]$n_eff)
        
        # there might not be a negative slope if non-negative variable, assign zero, move on
        suppressWarnings(if(length(beta_n) == 0) beta_n <- 0)
        
        # weigh down log-likelihood calculation with neff
        # if we had one beta value (no heteroscadasticity), we could've multiply n_eff*beta
        # now need to multiply every term with n_eff/n 
        SS_p <- - (inputs[[k]]$n_eff/inputs[[k]]$n) * log(beta_p) - resid[[1]][pos]/beta_p
        SS_n <- - (inputs[[k]]$n_eff/inputs[[k]]$n) * log(beta_n) - resid[[1]][!pos]/beta_n
        suppressWarnings(if(length(SS_n) == 0) SS_n <- 0)
        pda.errors[[k]] <- sum(SS_p, SS_n, na.rm = TRUE)
        SSdb[[k]] <- pda.errors[[k]]
        
    } else if (settings$assim.batch$inputs[[k]]$likelihood == "multipGauss") { 
      # multiplicative Gaussian
      
      SS <- rep(NA, length(bias.terms))
      for(b in seq_along(SS)){
        SS[b] <- sum((bias.terms[bc,][b] * model_out[[k]] - inputs[[k]]$obs)^2, na.rm = TRUE)
      }
      
      bc <- bc + 1
      pda.errors[[k]] <- SS 
      SSdb[[k]]       <- log(SS)
      
    } else { # Gaussian
      
      SS <- sum((model_out[[k]] - inputs[[k]]$obs)^2, na.rm = TRUE)
      
      pda.errors[[k]] <- SS 
      SSdb[[k]]       <- log(SS)
      
    }
  
    
  } # for-loop
  
  ## insert sufficient statistics in database
  if (!is.null(con)) {

    # BETY requires sufficient statistics to be associated with inputs, so only proceed
    # for inputs with valid input ID (i.e., not the -1 dummy id).
    # Note that analyses requiring sufficient statistics to be stored therefore require
    # inputs to be registered in BETY first.
    
    # TODO : insert multiple SS per unique run, input when it is allowed on DB
    db.input.ind <- which(sapply(inputs, function(x) x$input.id) != -1)
    for (k in db.input.ind) {
      
      PEcAn.DB::db.query(
        paste0("INSERT INTO likelihoods ",
               "(run_id, variable_id, input_id, ",
               " loglikelihood, n_eff)",
               "values ('",
               run.id, "', '",    inputs[[k]]$variable.id, "', '", inputs[[k]]$input.id, "', '",
               SSdb[[k]], "', '", inputs[[k]]$n_eff, "')"
        ),
        con)
    }
  }

  return(pda.errors)
  
} # pda.calc.error


##' Calculate Likelihoods for PDA
##'
##' @title Calculate Likelihoods for PDA
##' @param pda.errors calculated errors
##' @param llik.fn list of likelihood fcns
##' @param llik.par parameters to be passed llik functions
##'
##' @return Total log likelihood (i.e., sum of log likelihoods for each dataset)
##'
##' @author Ryan Kelly, Istem Fer
##' @export
pda.calc.llik <- function(pda.errors, llik.fn, llik.par) {
  
  n.var <- length(pda.errors)
  
  LL.vec <- numeric(n.var)
  
  for (k in seq_len(n.var)) {
    
    j <- k %% length(llik.fn)
    if(j==0) j <- length(llik.fn)
        
    LL.vec[k] <- llik.fn[[j]](pda.errors[k], llik.par[[k]])
  }
  
  LL.total <- sum(LL.vec)

  return(LL.total)
} # pda.calc.llik


##' Calculate likelihood parameters
##'
##' @title pda.calc.llik.par
##' 
##' @param settings list
##' @param n named vector, sample sizes of inputs
##' @param error.stats list, Sufficient Statistics 
##' @param hyper.pars list, hyperparameters
##' 
##' @author Istem Fer
##' @export

pda.calc.llik.par <-function(settings, n, error.stats, hyper.pars){
  
  llik.par <- list()
  
  for(k in seq_along(error.stats)){
    
    j <- k %% length(settings$assim.batch$inputs)
    if(j==0) j <- length(settings$assim.batch$inputs)
    
    llik.par[[k]] <- list()
    
    if (settings$assim.batch$inputs[[j]]$likelihood == "Gaussian" |
        settings$assim.batch$inputs[[j]]$likelihood == "multipGauss") {
      

        llik.par[[k]]$par <- stats::rgamma(1, hyper.pars[[k]]$parama + n[k]/2, 
                                    hyper.pars[[k]]$paramb + error.stats[k]/2)

        names(llik.par[[k]]$par) <- paste0("tau.", names(n)[k])

    }
    llik.par[[k]]$n     <- n[k]
    
  }

  return(llik.par)
  
} # pda.calc.llik.par


