##' Autocorelation correction and efficient sample size calculation on latent process
##'
##' What we're trying to do is to calculate the autocorrelation of the latent state, after attempting to "remove" the observation error. 
##' The first step is thus to estimate the latent state using a simple 'process free' state-space model (e.g. random walk).
##'
##' @title Calculate N_eff 
##' @param inputs list
##' @param recalculate repeat neff calculation or not
##'
##' @return inputs list, updated inputs with n_eff
##'
##' @author Istem Fer
##' @export
pda.neff.calc <- function(inputs, recalculate = FALSE){
  
  
  for(i in seq_along(inputs)){
    
    n   <- inputs[[i]]$n
    
    if(!is.null(inputs[[i]]$n_eff) && !recalculate){
      next
    }
    # for now we're doing autocorrelation correction on flux data only
    # NEE, LE, FC
    flux.vars <- c(297, 298, 1000000042)
    if(inputs[[i]]$variable.id %in% flux.vars){
      
      rho      <- pda.autocorr.calc(inputs[[i]], "heteroskedastic.laplacian")
      n_eff    <- n*(1-rho)/(1+rho)
      inputs[[i]]$n_eff <- n_eff
      
    }else{
      # assume n_eff is the as n
      inputs[[i]]$n_eff <- n
    }
  }
  
  return(inputs)
  
} # pda.neff.calc

##' @title autocorrelation correction
##' @param input list that contains time-series data vector and parameters for heteroskedastic.laplacian
##' @param model data model type, for flux data heteroskedastic laplacian, normal is an example 
##'
##' @return rho AR(1)
##'
##' @author Istem Fer
##' @export
pda.autocorr.calc <- function(input, model = "heteroskedastic.laplacian"){
  
  
  if(model == "heteroskedastic.laplacian"){
    
    HLModel = "
    model{
    
    #### Data Model
    for(i in 1:n){
      alphas[i] <- ifelse(x[i]<0, alpha_n, alpha_p)
      y[i] ~ ddexp(x[i],alpha_0 + alphas[i]*x[i])
    }
    
    #### Process Model
    for(i in 2:n){
      x[i] ~ dnorm(x[i-1], tau_add)
    }
    
    #### Priors
    x[1] ~ dnorm(x_ic,tau_ic)
    tau_add ~ dgamma(a_add,r_add)
    }
    "
    
    obs     <- c(input$obs)
    alpha_0 <- input$par[[1]] 
    alpha_p <- input$par[[2]] 
    alpha_n <- input$par[[3]]
    
    # converting back C fluxes from  kg C m-2 s-1 to umol C m-2 s-1 
    # reduces the code and makes model fitting easier
    if(input$variable.id %in% c(297, 1000000042)){
      obs <- PEcAn.utils::misc.convert(obs, "kg C m-2 s-1", "umol C m-2 s-1")
      AMFq  <- rep(0, length(obs))
      flags <- TRUE
      AMF.params <- PEcAn.uncertainty::flux.uncertainty(obs, AMFq, flags, bin.num = 20)
      alpha_0 <- AMF.params$intercept[[1]]
      alpha_p <- AMF.params$slopeP[[1]]
      alpha_n <- AMF.params$slopeN[[1]]
    }
    
    data <- list(y = obs, n = length(obs), 
                 x_ic = 1, tau_ic = 1e-2, a_add = 1, r_add = 1, 
                 alpha_0 = alpha_0, 
                 alpha_p = alpha_p, 
                 alpha_n = alpha_n) 
    
    nchain = 3
    init <- list()
    for(i in 1:nchain){
      y.samp    <- sample(obs, length(obs), replace=TRUE)
      init[[i]] <- list(tau_add=1/stats::var(diff(y.samp), na.rm=TRUE)) 
    }
    
    j.model   <- rjags::jags.model(file     = textConnection(HLModel),
                            data     = data,
                            inits    = init,
                            n.chains = 3)

    
    
  }else if(model == "gaussian"){
    # Gaussian
    
    GaussianModel = "
    model{

    #### Data Model
    for(i in 1:n){
      y[i] ~ dnorm(x[i],tau_obs)
    }

    #### Process Model
    for(i in 2:n){
      x[i] ~ dnorm(x[i-1], tau_add)
    }

    #### Priors
    x[1] ~ dnorm(x_ic,tau_ic)
    tau_obs ~ dgamma(a_obs,r_obs)
    tau_add ~ dgamma(a_add,r_add)
    }
    "
    
    obs  <- unlist(input$obs)
    
    data <- list(y = obs, n = length(obs),
                 x_ic =  1, tau_ic = 1e-2,
                 a_add = 1, r_add = 1, 
                 a_obs = 1, r_obs = 1)
    
    nchain = 3
    init <- list()
    for(i in 1:nchain){
      y.samp    <- sample(obs,length(obs),replace=TRUE)
      init[[i]] <- list(tau_add=1/stats::var(diff(y.samp), na.rm=TRUE), 
                        tau_obs=1/stats::var(diff(y.samp), na.rm=TRUE))
    }
    
    j.model   <- rjags::jags.model(file     = textConnection(GaussianModel),
                            data     = data,
                            inits    = init,
                            n.chains = 3)
    
  }else{
    PEcAn.logger::logger.error(model, "is not data available as data model.")
  }
  
  jags.out <- rjags::coda.samples(
    model          = j.model,
    variable.names = c("x"),
    n.iter         = 5000,
    thin           = 100)
  
  
  out <- as.matrix(jags.out)
  median.out <- apply(out, 2, stats::median)
  
  ar  <- stats::acf(median.out)
  rho <- as.numeric(ar$acf[2])
  return(rho)
  
} # pda.autocorr.calc

