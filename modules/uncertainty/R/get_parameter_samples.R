#' Generate parameter samples from priors and MCMC posteriors (pure function)
#'
#' This is the pure computation core extracted from \code{get.parameter.samples()}.
#' It takes pre-loaded R objects as input and returns all computed samples
#' directly, with no file I/O and no database calls.
#'
#' For the original disk-based wrapper, see \code{\link{get.parameter.samples}}.
#'
#' @param pft_names character vector of PFT names
#' @param prior_distns_list list of data frames (one per PFT), each with
#'   columns: distn, parama, paramb, n. Row names are trait names.
#' @param trait_mcmc_list list of trait MCMC results (one per PFT).
#'   Each element should be a named list of \code{coda::mcmc.list} objects,
#'   or NULL if no MCMC results exist for that PFT.
#' @param ensemble.size integer. Number of runs in model ensemble.
#' @param ens.sample.method character. One of "halton", "sobol", "torus",
#'   "lhc", "uniform".
#' @param sa_quantiles numeric vector of quantiles for sensitivity analysis,
#'   or NULL to skip SA sampling.
#' @param do_ensemble logical. Whether to generate ensemble samples.
#' @param independent logical. TRUE means parameters were fitted individually
#'   (standard MA) and can be resampled independently. FALSE means they were
#'   fitted together (e.g. PDA) and correlations should be preserved.
#'
#' @return named list with five elements:
#' \describe{
#'   \item{trait.samples}{Named list by PFT, then by trait. Each leaf is a
#'     numeric vector of parameter samples.}
#'   \item{sa.samples}{Sensitivity analysis samples (list), or empty list
#'     if \code{sa_quantiles} is NULL.}
#'   \item{ensemble.samples}{Ensemble samples (list), or empty list if
#'     \code{do_ensemble} is FALSE.}
#'   \item{runs.samples}{Reserved for future use (empty list).}
#'   \item{env.samples}{Environmental samples (empty list, populated by
#'     downstream code if needed).}
#' }
#'
#' @examples
#' \dontrun{
#' priors <- data.frame(
#'   distn  = "norm",
#'   parama = 20,
#'   paramb = 5,
#'   n      = 50,
#'   row.names = "SLA",
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- get_parameter_samples(
#'   pft_names         = "temperate.Hardwood",
#'   prior_distns_list = list(priors),
#'   ensemble.size     = 100,
#'   do_ensemble       = TRUE
#' )
#'
#' str(result$trait.samples)
#' str(result$ensemble.samples)
#' }
#'
#' @seealso \code{\link{get.parameter.samples}} for the backward-compatible
#'   wrapper that handles file I/O and database lookups.
#'
#' @author David LeBauer, Shawn Serbin, Istem Fer, Om Kapale
#' @export
get_parameter_samples <- function(pft_names,
                                       prior_distns_list,
                                       trait_mcmc_list = NULL,
                                       ensemble.size = 1,
                                       ens.sample.method = "uniform",
                                       sa_quantiles = NULL,
                                       do_ensemble = TRUE,
                                       independent = TRUE) {

  # ---- Input validation ----
  if (is.list(pft_names)) {
    pft_names <- unlist(pft_names)
  }
  stopifnot(
    is.character(pft_names),
    length(pft_names) > 0,
    is.list(prior_distns_list),
    length(prior_distns_list) == length(pft_names)
  )
  if (!is.null(trait_mcmc_list)) {
    stopifnot(
      is.list(trait_mcmc_list),
      length(trait_mcmc_list) == length(pft_names)
    )
  }

  # Force numeric for compatibility with downstream sampling code
  ensemble.size <- as.numeric(ensemble.size)

  # ---- Initialize output containers ----
  trait.samples    <- list()
  sa.samples       <- list()
  ensemble.samples <- list()
  env.samples      <- list()
  runs.samples     <- list()
  param.names      <- list()

  # ---- Process each PFT ----
  for (i in seq_along(pft_names)) {
    pft.name     <- pft_names[i]
    prior.distns <- prior_distns_list[[i]]
    trait.mcmc   <- if (!is.null(trait_mcmc_list)) trait_mcmc_list[[i]] else NULL

    # Determine which traits have priors defined
    if (!is.null(prior.distns)) {
      priors <- rownames(prior.distns)
    } else {
      priors <- NULL
    }

    # Determine sample count: use MCMC chain length if available, else 20000
    if (!is.null(trait.mcmc) && length(trait.mcmc) > 0) {
      param.names[[i]] <- names(trait.mcmc)
      names(param.names)[i] <- pft.name

      samples.num <- min(
        sapply(trait.mcmc, function(x) nrow(as.matrix(x)))
      )

      ## Report which traits use MA results vs priors
      if (length(param.names[[i]]) > 0) {
        PEcAn.logger::logger.info(
          "PFT", pft.name, "has MCMC samples for:\n",
          paste0(param.names[[i]], collapse = "\n ")
        )
      }
      if (!is.null(priors) && !all(priors %in% param.names[[i]])) {
        PEcAn.logger::logger.info(
          "PFT", pft.name, "will use prior distributions for:\n",
          paste0(priors[!priors %in% param.names[[i]]], collapse = "\n ")
        )
      }
    } else {
      param.names[[i]] <- list()
      samples.num <- 20000
      PEcAn.logger::logger.info("No MCMC results for PFT", pft.name)
      if (!is.null(priors)) {
        PEcAn.logger::logger.info(
          "PFT", pft.name, "will use prior distributions for",
          priors
        )
      }
    }

    # If no priors defined, fall back to MCMC param names
    if (is.null(priors)) priors <- param.names[[i]]

    PEcAn.logger::logger.info("using ", samples.num, " samples per trait")

    n_priors <- length(priors)
    if (n_priors == 0) {
      PEcAn.logger::logger.info("No traits to sample for PFT", pft.name)
      next
    }

    # Generate quantile matrix for prior-based sampling
    q_samples <- .generate_quantile_samples(
      n      = samples.num,
      dim    = n_priors,
      method = ens.sample.method
    )
    # Ensure q_samples is always a matrix (some methods return vector when dim=1)
    if (!is.matrix(q_samples) && is.numeric(q_samples)) {
      q_samples <- matrix(q_samples, ncol = 1)
    }

    # Sample each trait: from MCMC if available, otherwise from prior
    for (prior in priors) {
      if (prior %in% param.names[[i]]) {
        # Extract MCMC samples across all chains
        samples <- trait.mcmc[[prior]] |>
          purrr::map(\(x) x[, "beta.o"]) |>
          unlist() |>
          as.matrix()
      } else {
        # Sample from prior distribution using quantile matrix
        samples <- PEcAn.priors::get.sample(
          prior.distns[prior, ],
          samples.num,
          q_samples[, priors == prior]
        )
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
  }

  # ---- Handle independence flag ----
  # When samples are independent (fitted individually), set param.names to NULL.
  # This is important downstream: when param.names is not NULL,
  # get.ensemble.samples() preserves correlations between MCMC parameters.
  if (independent) {
    param.names <- NULL
  }

  # ---- Sensitivity analysis samples ----
  if (!is.null(sa_quantiles)) {
    # get.quantiles() expects a settings-style list with $quantile elements.
    # When called from the pure function, sa_quantiles is a numeric vector,
    # so we handle both cases.
    if (is.numeric(sa_quantiles)) {
      quantiles <- sort(unique(c(sa_quantiles, 0.5)))
    } else {
      quantiles <- PEcAn.utils::get.quantiles(sa_quantiles)
    }
    PEcAn.logger::logger.info(
      "\n Selected Quantiles: ",
      PEcAn.utils::vecpaste(round(quantiles, 3))
    )
    sa.samples <- PEcAn.utils::get.sa.sample.list(
      pft = trait.samples, env = env.samples, quantiles = quantiles
    )
  }

  # ---- Ensemble samples ----
  if (do_ensemble) {
    if (ensemble.size == 1) {
      ## Run at median if only one run in ensemble
      ensemble.samples <- PEcAn.utils::get.sa.sample.list(
        pft = trait.samples, env = env.samples, quantiles = 0.5
      )
    } else if (ensemble.size > 1) {
      ## Subset trait.samples to ensemble size
      ensemble.samples <- get.ensemble.samples(
        ensemble.size, trait.samples,
        env.samples, ens.sample.method, param.names
      )
    }
  }

  return(list(
    trait.samples    = trait.samples,
    sa.samples       = sa.samples,
    ensemble.samples = ensemble.samples,
    runs.samples     = runs.samples,
    env.samples      = env.samples
  ))
}


#' Generate quasi-random or random quantile sample matrix
#'
#' Internal helper that produces an n x dim matrix of values in [0, 1]
#' using the specified sampling method.
#'
#' @param n integer, number of samples
#' @param dim integer, number of dimensions (traits)
#' @param method character, sampling method
#' @return matrix of dimension n x dim with values in [0, 1]
#' @noRd
.generate_quantile_samples <- function(n, dim, method = "uniform") {
  if (dim == 0) return(matrix(nrow = 0, ncol = 0))

  switch(method,
    "halton"  = randtoolbox::halton(n = n, dim = dim),
    "sobol"   = randtoolbox::sobol(n = n, dim = dim, scrambling = 3),
    "torus"   = randtoolbox::torus(n = n, dim = dim),
    "lhc"     = PEcAn.emulator::lhc(t(matrix(0:1, ncol = dim, nrow = 2)), n),
    "uniform" = matrix(stats::runif(n * dim), n, dim),
    {
      PEcAn.logger::logger.info(
        "Method ", method,
        " has not been implemented yet, using uniform random sampling"
      )
      matrix(stats::runif(n * dim), n, dim)
    }
  )
}
