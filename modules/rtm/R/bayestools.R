#' Generic log-likelihood generator for RTMs
rtm_loglike <- function(nparams, model, observed, lag.max = 0.01, ...) {
    fail_ll <- -1e10
    stopifnot(nparams >= 1, nparams %% 1 == 0, is.function(model), is.numeric(observed))
    n_obs <- length(observed)
    out <- function(x) {
        rtm_params <- x[seq_len(nparams)]
        rsd <- x[nparams + 1]
        mod <- model(rtm_params, ...)
        if (any(is.na(mod))) return(fail_ll)
        err <- mod - observed
        ss <- sum(err * err)
        sigma2 <- rsd * rsd
        n_eff <- neff(err, lag.max = lag.max)
        sigma2eff <- sigma2 * n_obs / n_eff
        ll <- -0.5 * (n_obs * log(sigma2eff) + ss / sigma2eff)
        if (is.na(ll)) return(fail_ll)
        return(ll)
    }
    return(out)
}

#' Check convergence of BayesianTools output
bt_check_convergence <- function(samples, threshold = 1.1, use_CI = TRUE, use_mpsrf = TRUE) {
    i <- ifelse(use_CI, 2, 1)
    gelman <- BayesianTools::gelmanDiagnostics(samples)
    if (use_mpsrf) {
        gelman_vec <- c(gelman$psrf[,i], mpsrf = gelman$mpsrf)
    } else {
        gelman_vec <- gelman$psrf[,i]
    }
    exceeds <- gelman_vec > threshold
    if (any(exceeds)) {
        exceeds_vec <- gelman_vec[exceeds]
        exceeds_char <- sprintf('%s: %.2f', names(exceeds_vec), exceeds_vec)
        exceeds_str <- paste(exceeds_char, collapse = '; ')
        message('The following parameters exceed threshold: ', exceeds_str)
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' Quick BayesianTools prior creator for PROSPECT model 
#'
#' @export
prospect_bt_prior <- function(version, custom_prior = list()) {
    col_names <- c('param_name', 'distn', 'parama', 'paramb', 'lower')
    prior_default_list <- list(N = list('N', 'norm', 1.4, 0.8, 1),
                               Cab = list('Cab', 'lnorm', log(40), 0.9, 0),
                               Car = list('Car', 'lnorm', log(10), 1.1, 0),
                               Cbrown = list('Cbrown', 'lnorm', log(1), 1.1, 0),
                               Cw = list('Cw', 'lnorm', log(0.01), 1, 0),
                               Cm = list('Cm', 'lnorm', log(0.009), 1, 0),
                               residual = list('residual', 'lnorm', log(0.001), 2.5, 0)
                               )    
    prior_list <- modifyList(prior_default_list, custom_prior)
    prior_df_all <- do.call(rbind.data.frame, prior_list)
    colnames(prior_df_all) <- col_names
    default_params <- defparam(paste0('prospect_', tolower(version)))
    use_names <- c(names(default_params), 'residual')
    prior_df <- prior_df_all[prior_df_all[['param_name']] %in% use_names,]
    prior <- PEcAn.assim.batch::pda.create.btprior(prior_df)
    return(prior)
}

#' Perform Bayesian inversion using BayesianTools package
#' 
#' @export
invert_bt <- function(observed, model, prior, custom_settings = list()) {

    default_settings <- list(common = list(), 
                             init = list(iterations = 10000),
                             loop = list(iterations = 2000),
                             other = list(sampler = 'DEzs', 
                                          use_mpsrf = FALSE,
                                          min_samp = 1000))

    if (length(custom_settings) > 0) {
        for (s in seq_along(default_settings)) {
            s_name <- names(default_settings)[s]
            if (s_name %in% names(custom_settings)) {
                settings[[s_name]] <- modifyList(default_settings[[s_name]], 
                                                 custom_settings[[s_name]])
            } else {
                settings[[s_name]] <- default_settings[[s_name]]
            }
        } 
    } else {
        settings <- default_settings
    }

    use_mpsrf <- settings[['other']][['use_mpsrf']]
    min_samp <- settings[['other']][['min_samp']]
    lag.max <- settings[['other']][['lag.max']]

    stopifnot('prior' %in% class(prior))
    test_samp <- prior$sampler()
    param_names <- names(test_samp)
    nparams <- length(test_samp[param_names != 'residual'])
    loglike <- rtm_loglike(nparams = nparams, 
                           model = model, 
                           observed = observed, 
                           lag.max = lag.max)


    setup <- BayesianTools::createBayesianSetup(likelihood = loglike, 
                                                prior = prior, 
                                                names = param_names)


    init_settings <- modifyList(settings[['common']], settings[['init']])
    samples <- BayesianTools::runMCMC(bayesianSetup = setup, 
                                      sampler = settings[['other']][['sampler']], 
                                      settings = init_settings)
    converged <- bt_check_convergence(samples = samples, use_mpsrf = settings[['other']][['use_mpsrf']])

    loop_settings <- modifyList(settings[['common']], settings[['loop']])

    last_iter <- 1
    current_iter <- 

    while(!(converged && enough_samples)) {
        samples <- BayesianTools::runMCMC(samples, sampler = sampler, settings = loop_settings)
        converged <- bt_check_convergence(samples = samples, use_mpsrf = settings[['other']][['use_mpsrf']])
        if (converged) {
            coda_samples <- BayesianTools::getSample(samples, coda = TRUE)
            burned_samples <- PEcAn.assim.batch::autoburnin(coda_samples, return.burnin = TRUE, method = 'gelman.plot')
            if (burned_samples$burnin == 1) next
            n_samples <- coda::niter(burned_samples$samples)
            enough_samples <- n_samples > min_samp
            if (!enough_samples) {
                message(n_samples, ' samples after burnin is less than target ', min_samp, 
                        '. Resuming sampling.')
            }
        }
    }
    return(samples)
}

