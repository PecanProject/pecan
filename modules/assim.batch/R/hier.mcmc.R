##' Hierarchical MCMC
##'
##' @title Hierarchical MCMC using emulator
##' @param settings  a pecan settings list
##' @param gp.stack list of GPs
##' @param nstack list of number of observations, currently not used
##' @param nmcmc number of MCMC iterations
##' @param rng_orig range of knots
##' @param jmp0 initial jump vars
##' @param mu_site_init initial parameter values per site
##' @param nparam number of parameters
##' @param nsites number of sites
##' @param llik.fn list of likelihood functions
##' @param prior.fn.all list of prior functions
##' @param prior.ind.all indices of targeted parameters
##'
##'
##' @author Istem Fer
##' @export
##' 

########### hierarchical MCMC function with Gibbs ##############


hier.mcmc <- function(settings, gp.stack, nstack = NULL, nmcmc, rng_orig,
                     jmp0, mu_site_init, nparam, nsites, llik.fn, prior.fn.all, prior.ind.all){
  
  pos.check <- sapply(settings$assim.batch$inputs, `[[`, "ss.positive")
  
  if(length(unlist(pos.check)) == 0){
    # if not passed from settings assume none
    pos.check <- rep(FALSE, length(settings$assim.batch$inputs))
  }else if(length(unlist(pos.check)) != length(settings$assim.batch$inputs)){
    # maybe one provided, but others are forgotten
    # check which ones are provided in settings
    from.settings <- sapply(seq_along(pos.check), function(x) !is.null(pos.check[[x]]))
    tmp.check <- rep(FALSE, length(settings$assim.batch$inputs))
    # replace those with the values provided in the settings
    tmp.check[from.settings] <- as.logical(unlist(pos.check))
    pos.check <- tmp.check
  }else{
    pos.check <- as.logical(pos.check)
  }
  
  ################################################################
  #
  #      mu_site    : site level parameters (nsite x nparam)
  #      tau_site   : site level precision (nsite x nsite)
  #      mu_global  : global parameters (nparam)
  #      tau_global : global precision matrix (nparam x nparam)
  #
  ################################################################
  
  
  
  ###### (hierarchical) global mu priors
  #
  #      mu_global_mean      : prior mean vector
  #      mu_global_sigma     : prior covariance matrix
  #      mu_global_tau       : prior precision matrix
  #
  #      mu_global ~ MVN (mu_global_mean, mu_global_tau)
  
  # approximate a normal dist
  mu_init_samp <- matrix(NA, ncol = nparam, nrow = 1000)
  for(ps in seq_along(prior.ind.all)){
    prior.quantiles <- eval(prior.fn.all$rprior[[prior.ind.all[ps]]], list(n = 1000))
    mu_init_samp[,ps] <- prior.quantiles
  }
  
  # mean hyperprior
  mu_global_mean  <- apply(mu_init_samp, 2, mean)
  # sigma/tau hyperprior
  mu_global_sigma <- stats::cov(mu_init_samp)
  mu_global_tau   <- solve(mu_global_sigma)
  
  ## initialize mu_global (nparam)
  mu_global <- mvtnorm::rmvnorm(1, mu_global_mean, mu_global_sigma)
  
  
  ######  (hierarchical) global tau priors
  #
  #      tau_global_df        : Wishart degrees of freedom
  #      tau_global_sigma     : Wishart scale matrix
  #
  #      tau_global ~ W (tau_global_df, tau_global_sigma)
  #      sigma_global <- solve(tau_global)
  #
  
  # sigma_global hyperpriors
  sigma_global_df     <- nparam + 1 # test results with nparam since it is the least informative
  sigma_global_scale  <- mu_global_sigma/sigma_global_df 
  
  # initialize sigma_global (nparam x nparam)
  sigma_global <-  MCMCpack::riwish(sigma_global_df, sigma_global_scale)

  # initialize jcov.arr (jump variances per site)
  jcov.arr <-  array(NA_real_, c(nparam, nparam, nsites))
  for(j in seq_len(nsites)) jcov.arr[,,j] <- jmp0
  
  # prepare mu_site (nsite x nparam)
  mu_site_new  <- matrix(NA_real_, nrow = nsites, ncol= nparam)
  
  # start
  mu_site_curr <- matrix(rep(mu_site_init, nsites), ncol=nparam, byrow = TRUE)
  
  # values for each site will be accepted/rejected in themselves
  currSS  <- sapply(seq_len(nsites), function(v) PEcAn.emulator::get_ss(gp.stack[[v]], mu_site_curr[v,], pos.check))
  # force it to be nvar x nsites matrix
  currSS  <- matrix(currSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
  currllp <- lapply(seq_len(nsites), function(v) PEcAn.assim.batch::pda.calc.llik.par(settings, nstack[[v]], currSS[,v]))
  
  # storage
  mu_site_samp      <-  array(NA_real_, c(nmcmc, nparam, nsites))
  mu_global_samp    <-  matrix(NA_real_, nrow = nmcmc, ncol= nparam)
  sigma_global_samp <-  array(NA_real_, c(nmcmc, nparam, nparam))
  
  musite.accept.count    <- rep(0, nsites)
  
  adapt_orig <- settings$assim.batch$jump$adapt
  settings$assim.batch$jump$adapt <- adapt_orig * nsites
  
  ########################## Start MCMC ########################
  
  for(g in 1:nmcmc){
    
    # jump adaptation step
    if ((g > 2) && ((g - 1) %% settings$assim.batch$jump$adapt == 0)) {
      
      # update site level jvars
      params.recent <- mu_site_samp[(g - settings$assim.batch$jump$adapt):(g - 1), , ]
      #colnames(params.recent) <- names(x0)
      settings$assim.batch$jump$adapt <- adapt_orig
      jcov.list <- lapply(seq_len(nsites), function(v) pda.adjust.jumps.bs(settings, jcov.arr[,,v], musite.accept.count[v], 
                                                                           params.recent[seq(v, adapt_orig * nsites, by=12), , v]))
      jcov.list <- lapply(jcov.list, lqmm::make.positive.definite, tol=1e-12)
      jcov.arr  <- abind::abind(jcov.list, along=3)
      musite.accept.count <- rep(0, nsites)  # Reset counter
      settings$assim.batch$jump$adapt <- adapt_orig * nsites
    }
    
    
    ########################################
    # gibbs update tau_global | mu_global, mu_site
    #
    # W(tau_global | mu_global, mu_site) ~ MVN( mu_site | mu_global, tau_global) * W(tau_global | tau_df, tau_V)
    # 
    #
    # using MVN-Wishart conjugacy
    # prior hyperparameters:     tau_global_df, tau_global_sigma
    # posterior hyperparameters: tau_global_df_gibbs, tau_global_sigma_gibbs
    #
    # update:
    # tau_global ~ W(tau_global_df_gibbs, tau_global_sigma_gibbs)
    
    
    sigma_global_df_gibbs <- sigma_global_df + nsites

    
    pairwise_deviation <- apply(mu_site_curr, 1, function(r) r - t(mu_global))
    sum_term <- pairwise_deviation %*% t(pairwise_deviation)
    
    sigma_global_scale_gibbs <- sigma_global_scale + sum_term
    
    # update sigma
    sigma_global   <- MCMCpack::riwish(sigma_global_df_gibbs, sigma_global_scale_gibbs) # across-site covariance
    
    
    
    ########################################
    # update mu_global | mu_site, tau_global
    #
    # MVN(mu_global | mu_site, tau_global) ~ MVN( mu_site | mu_global, tau_global) * W(tau_global | tau_df, tau_V)
    #
    # mu_global ~ MVN(global_mu, global_Sigma)
    #
    # mu_global     : global parameters
    # global_mu     : precision weighted average between the data (mu_site) and prior mean (mu_f)
    # global_Sigma  : sum of mu_site and mu_f precision      
    #
    # Dietze, 2017, Eqn 13.6
    # mu_global ~ MVN(solve((nsites * sigma_global) + P_f_inv)) * ((nsites * sigma_global) + P_f_inv * mu_f),  
    #                 solve((nsites * sigma_global) + P_f_inv))
    
    # prior hyperparameters      : mu_global_mean, mu_global_sigma
    # posterior hyperparameters  : mu_global_mean_gibbs, mu_global_sigma_gibbs
    #
    # update:
    # mu_global ~ MVN(mu_global_mean_gibbs, mu_global_sigma_gibbs)
    
    # calculate mu_global_sigma_gibbs from prior hyperparameters and tau_global
    mu_global_sigma_gibbs <- solve(mu_global_tau + nsites * solve(sigma_global))
  
    
    mu_site_bar     <- apply(mu_site_curr, 2, mean)
    
    # calculate mu_global_mean_gibbs from prior hyperparameters, mu_site_means and tau_global
    mu_global_mean_gibbs <- mu_global_sigma_gibbs %*% 
      (mu_global_tau %*% mu_global_mean + ((nsites*solve(sigma_global)) %*% mu_site_bar))
    
    # update mu_global
    mu_global <- mvtnorm::rmvnorm(1, mu_global_mean_gibbs, mu_global_sigma_gibbs) # new prior mu to be used below for prior prob. calc.
    
  
    # site level M-H
    ########################################
    
    # propose new site parameter vectors
    thissite <- g %% nsites
    if(thissite == 0) thissite <- nsites
    proposed <- TruncatedNormal::rtmvnorm(1, 
                                   mu    = mu_site_curr[thissite,], 
                                   sigma = jcov.arr[,,thissite],
                                   lb    = rng_orig[,1],
                                   ub    = rng_orig[,2])

    mu_site_new <- matrix(rep(proposed, nsites),ncol=nparam, byrow = TRUE)
    
    # re-predict current SS
    currSS <- sapply(seq_len(nsites), function(v) PEcAn.emulator::get_ss(gp.stack[[v]], mu_site_curr[v,], pos.check))
    currSS <- matrix(currSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
    
    # calculate posterior
    currLL    <- sapply(seq_len(nsites), function(v) PEcAn.assim.batch::pda.calc.llik(currSS[,v], llik.fn, currllp[[v]]))
    # use new priors for calculating prior probability
    currPrior <- mvtnorm::dmvnorm(mu_site_curr, mu_global, sigma_global, log = TRUE)
    currPost  <- currLL + currPrior
    
    # calculate jump probabilities
    currHR <- sapply(seq_len(nsites), function(v) {
      TruncatedNormal::dtmvnorm(mu_site_curr[v,], mu_site_new[v,], jcov.arr[,,v],
                         lb = rng_orig[,1],
                         ub = rng_orig[,2], log = TRUE, B = 1e2)
    })
    
    # predict new SS
    newSS <- sapply(seq_len(nsites), function(v) PEcAn.emulator::get_ss(gp.stack[[v]], mu_site_new[v,], pos.check))
    newSS <- matrix(newSS, nrow = length(settings$assim.batch$inputs), ncol = nsites)
    
    # calculate posterior
    newllp   <- lapply(seq_len(nsites), function(v) PEcAn.assim.batch::pda.calc.llik.par(settings, nstack[[v]], newSS[,v]))
    newLL    <- sapply(seq_len(nsites), function(v) PEcAn.assim.batch::pda.calc.llik(newSS[,v], llik.fn, newllp[[v]]))
    # use new priors for calculating prior probability
    newPrior <- mvtnorm::dmvnorm(mu_site_new, mu_global, sigma_global, log = TRUE)
    newPost  <- newLL + newPrior
    
    # calculate jump probabilities
    newHR <- sapply(seq_len(nsites), function(v) {
      TruncatedNormal::dtmvnorm(mu_site_new[v,], mu_site_curr[v,], jcov.arr[,,v],
                         lb = rng_orig[,1],
                         ub = rng_orig[,2], log = TRUE, B = 1e2)
    })
    
    # Accept/reject with MH rule
    ar <- PEcAn.emulator::is.accepted(currPost + currHR, newPost + newHR)
    mu_site_curr[ar, ] <- mu_site_new[ar, ]
    musite.accept.count[thissite] <- musite.accept.count[thissite] + ar[thissite]
    
    
    mu_site_samp[g, , seq_len(nsites)] <- t(mu_site_curr)[,seq_len(nsites)]
    mu_global_samp[g,]       <- mu_global  # 100% acceptance for gibbs
    sigma_global_samp[g, , ] <- sigma_global # 100% acceptance for gibbs
    
    if(g %% 500 == 0) PEcAn.logger::logger.info(g, "of", nmcmc, "iterations")
  }
  
  return(list(mu_site_samp = mu_site_samp, mu_global_samp = mu_global_samp, sigma_global_samp = sigma_global_samp,
              musite.accept.count = musite.accept.count))
} # hier.mcmc