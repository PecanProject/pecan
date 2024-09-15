#' Generic log-likelihood generator for RTMs
#'
#' @param nparams number of parameters in model
#' @param model function to minimize
#' @param observed vector of observations
#' @param lag.max passed to [neff()]
#' @param verbose logical: print extra diagnostics during run?
#' @param ... additional arguments passed to model function
#'
rtm_loglike <- function(nparams, model, observed, lag.max = NULL, verbose = TRUE, ...) {
  fail_ll <- -1e10
  stopifnot(nparams >= 1, nparams %% 1 == 0, is.function(model), is.numeric(observed))
  n_obs <- length(observed)
  out <- function(x) {
    rtm_params <- x[seq_len(nparams)]
    rsd <- x[nparams + 1]
    mod <- model(rtm_params, ...)
    if (any(is.na(mod))) {
      if (verbose) message(sum(is.na(mod)), " NA values in model output. Returning loglike = ", fail_ll)
      return(fail_ll)
    }
    err <- mod - observed
    ss <- sum(err * err)
    sigma2 <- rsd * rsd
    # Effective sample size normalization.
    # Turning this on gives weird results.
    # n_eff <- neff(err, lag.max = lag.max)
    # sigma2eff <- sigma2 * n_obs / n_eff
    ll <- -0.5 * (n_obs * log(sigma2) + ss / sigma2)
    if (is.na(ll)) {
      if (verbose) {
        message("Log likelihood is NA. Returning loglike = ", fail_ll)
        message("Mean error: ", mean(err))
        message("Sum of squares: ", ss)
        message("Sigma2 = ", sigma2)
        message("LL = ", ll)
      }
      return(fail_ll)
    }
    return(ll)
  }
  return(out)
}

# Check convergence of BayesianTools output
bt_check_convergence <- function(samples, threshold = 1.1, use_CI = TRUE, use_mpsrf = TRUE) {
  i <- ifelse(use_CI, 2, 1)
  gelman <- try(BayesianTools::gelmanDiagnostics(samples))
  if (inherits(gelman, "try-error")) {
    message('Error trying to calculate gelman diagnostic. Assuming no convergence')
    return(FALSE)
  }
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
#' @param custom_prior List containing `param_name`, `distn`, `parama`, `paramb`, and `lower`
#' @inheritParams prospect
#' @export
prospect_bt_prior <- function(version, custom_prior = list()) {
    col_names <- c('param_name', 'distn', 'parama', 'paramb', 'lower')
    prior_default_list <- list(
        N = list('N', 'norm', 1.4, 0.8, 1),
        Cab = list('Cab', 'lnorm', log(40), 0.9, 0),
        Car = list('Car', 'lnorm', log(10), 1.1, 0),
        Canth = list('Canth', 'lnorm', log(10), 1.1, 0),
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
#' Use samplers from the BayesianTools package to fit models to data. Like
#' `invert.auto`, this will continue to run until convergence is achieved
#' (based on Gelman diagnostic) _and_ the result has enough samples (as
#' specified by the user; see Details).
#'
#' @details `custom_settings` is a list of lists, containing the following:
#'  * `common` -- BayesianTools settings common to both the initial and subsequent samples.
#'  * `init` -- BayesianTools settings for just the first round of sampling.
#'  This is most common for the initial number of iterations, which is the
#'  minimum expected for convergence.
#'  * `loop` -- BayesianTools settings for iterations inside the convergence
#'  checking `while` loop. This is most commonly for setting a smaller
#'  iteration count than in `init`.
#'  * `other` -- Miscellaneous (non-BayesianTools) settings, including:
#'      - `sampler` -- String describing which sampler to use. Default is `DEzs`
#'      - `use_mpsrf` -- Use the multivariate PSRF to check convergence.
#'      Default is `FALSE` because it may be an excessively conservative
#'      diagnostic.
#'      - `min_samp` -- Minimum number of samples after burnin before stopping.
#'      Default is 5000.
#'      - `max_iter` -- Maximum total number of iterations. Default is 1e6.
#'      - `lag.max` -- Maximum lag to use for autocorrelation normalization.
#'      Default is `10 * log10(n)` (same as `stats::acf` function).
#'      - `save_progress` -- File name for saving samples between loop
#'      iterations. If `NULL` (default), do not save progress samples.
#'      - `threshold` -- Threshold for Gelman PSRF convergence diagnostic. Default is 1.1.
#'      - `verbose_loglike` -- Diagnostic messages in log likelihood output. Default is TRUE.
#'
#' See the BayesianTools sampler documentation for what can go in the `BayesianTools` settings lists.
#' @param observed Vector of observations. Ignored if `loglike` is not `NULL`.
#' @param model Function called by log-likelihood. Must be `function(params)`
#' and return a vector equal to `length(observed)` or `nrow(observed)`. Ignored 
#' if `loglike` is not `NULL`.
#' @param prior BayesianTools prior object.
#' @param custom_settings Nested settings list. See Details.
#' @param loglike Custom log likelihood function. If `NULL`, use [rtm_loglike()] 
#' with provided `observed` and `model`.
#' @export
invert_bt <- function(observed, model, prior, custom_settings = list(), loglike = NULL) {

  default_settings <- list(
    common = list(),
    init = list(iterations = 10000),
    loop = list(iterations = 2000),
    other = list(
      sampler = 'DEzs',
      use_mpsrf = FALSE,
      min_samp = 5000,
      max_iter = 1e6,
      lag.max = NULL,
      save_progress = NULL,
      threshold = 1.1,
      verbose_loglike = TRUE
    )
  )

  if (length(custom_settings) > 0) {
    settings <- list()
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
  max_iter <- settings[['other']][['max_iter']]
  save_progress <- settings[['other']][['save_progress']]
  threshold <- settings[['other']][['threshold']]
  verbose_loglike <- settings[['other']][['verbose_loglike']]

  if (!is.null(save_progress)) {
    # `file.create` returns FALSE if target directory doesn't exist.
    stopifnot(file.create(save_progress))
  }
  stopifnot(inherits(prior, "prior"))
  test_samp <- prior$sampler()
  param_names <- names(test_samp)
  if (is.null(param_names)) {
    warning("Parameters are not named! Unable to check validity.")
  } else {
    if (!("residual" %in% param_names)) {
      stop("One of the parameters must be `residual`.")
    }
  }
  nparams <- length(test_samp[param_names != 'residual'])
  if (is.null(loglike)) {
    loglike <- rtm_loglike(
      nparams = nparams,
      model = model,
      observed = observed,
      lag.max = lag.max,
      verbose = verbose_loglike
    )
  }

  setup <- BayesianTools::createBayesianSetup(
    likelihood = loglike,
    prior = prior,
    names = param_names
  )


  init_settings <- modifyList(settings[['common']], settings[['init']])
  stop_iter <- init_settings[["iterations"]]
  if (is.null(stop_iter)) {
    stop_iter <- 10000
    warning('init_settings$iterations is not set. Using ', stop_iter, '.')
  }
  message('Running initial ', stop_iter, ' iterations.')
  samples <- BayesianTools::runMCMC(
    bayesianSetup = setup,
    sampler = settings[['other']][['sampler']],
    settings = init_settings
  )
  if (!is.null(save_progress)) {
    saveRDS(object = samples, file = save_progress)
  }
  converged <- bt_check_convergence(samples = samples, threshold = threshold, use_mpsrf = use_mpsrf)

  loop_settings <- modifyList(settings[['common']], settings[['loop']])

  next_iter <- loop_settings[['iterations']]
  if (is.null(next_iter)) {
    next_iter <- 2000
    warning('loop_settings$iterations is not set. Using ', next_iter, '.')
  }

  while (!(converged && enough_samples)) {
    start_iter <- stop_iter + 1
    stop_iter <- stop_iter + next_iter
    if (start_iter > max_iter) {
      warning('Next start iteration (', start_iter, ') greater than maximum iteration count (', max_iter, ') ',
              'but convergence has not been achieved. ',
              'Terminating sampling and returning results as is.')
      break
    }
    message('Running ', next_iter, ' more iterations (', start_iter, ' to ', stop_iter, ').')
    samples <- BayesianTools::runMCMC(samples, sampler = sampler, settings = loop_settings)
    if (!is.null(save_progress)) {
      saveRDS(object = samples, file = save_progress)
    }
    converged <- bt_check_convergence(samples = samples, threshold = threshold, use_mpsrf = use_mpsrf)
    if (converged) {
      coda_samples <- BayesianTools::getSample(samples, coda = TRUE)
      burned_samples <- PEcAn.assim.batch::autoburnin(coda_samples, threshold = threshold,
                                                      return.burnin = TRUE, method = 'gelman.plot')
      if (burned_samples$burnin == 1) {
        message('PEcAn.assim.batch::autoburnin reports convergence has not been achieved. ',
                'Resuming sampling.')
        converged <- FALSE
        next
      }
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

