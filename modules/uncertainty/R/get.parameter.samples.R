#' Convert priors / MCMC samples to parameter sample chains
#'
#' Loads prior distributions and MCMC results from disk, generates parameter
#' samples for ensemble and sensitivity analysis runs, and saves results
#' to \code{samples.Rdata}.
#'
#' This is the backward-compatible wrapper. Delegates computation to
#' \code{\link{get_parameter_samples}}.
#'
#' @param settings PEcAn settings object
#' @param ensemble.size number of runs in model ensemble
#' @param posterior.files list of filenames to read from
#' @param ens.sample.method one of "halton", "sobol", "torus", "lhc", "uniform"
#' @param save_to_disk logical. If TRUE (default), saves samples.Rdata.
#'
#' @return named list with: trait.samples, sa.samples, ensemble.samples, runs.samples, env.samples
#'
#' @export
#'
#' @author David LeBauer, Shawn Serbin, Istem Fer, Om Kapale
#' @importFrom rlang %||%
get.parameter.samples <- function(settings,
                                  ensemble.size = 1,
                                  posterior.files = rep(NA, length(settings$pfts)),
                                  ens.sample.method = "uniform",
                                  save_to_disk = TRUE) {
  .Deprecated("get_parameter_samples")

  ### Identify PFTs in the input settings.xml file
  pfts <- settings$pfts
  pft.names <- list()
  outdirs <- list()

  if (length(pfts) != length(posterior.files)) {
    PEcAn.logger::logger.error(
      "settings$pfts and posterior.files should be the same length"
    )
  }

  ## Open database connection
  con <- NULL
  if (!is.null(settings$database$bety)) {
    con <- try(PEcAn.DB::db.open(settings$database$bety))
    on.exit(try(PEcAn.DB::db.close(con), silent = TRUE), add = TRUE)
    if (inherits(con, "try-error")) {
      con <- NULL
      PEcAn.logger::logger.warn(
        "We were not able to successfully establish a connection with Bety"
      )
    }
  } else {
    PEcAn.logger::logger.info(
      "No database connection parameters provided.",
      "Will not use Bety for parameter lookup."
    )
    con <- NULL
  }

  # If we fail to connect to DB then we set to NULL
  if (inherits(con, "try-error")) {
    con <- NULL
    PEcAn.logger::logger.warn(
      "We were not able to successfully establish a connection with Bety "
    )
  }

  for (i.pft in seq_along(pfts)) {
    # If no name given, use string "NULL" to warn user
    pft.names[i.pft] <- pfts[[i.pft]]$name %||% "NULL"

    ### Get output directory info
    if (!is.null(pfts[[i.pft]]$outdir)) {
      outdirs[i.pft] <- pfts[[i.pft]]$outdir
    } else {
      outdirs[i.pft] <- unique(
        PEcAn.DB::dbfile.check(
          type = "Posterior",
          container.id = pfts[[i.pft]]$posteriorid,
          con = con
        )$file_path
      )
    }
  } ### End of for loop to extract pft names

  PEcAn.logger::logger.info("Selected PFT(s): ", pft.names)

  ## Generate empty list arrays for output.
  prior_distns_list <- vector("list", length(pft.names))
  trait_mcmc_list   <- vector("list", length(pft.names))

  # flag determining whether samples are independent
  # (e.g. when params fitted individually)
  independent <- TRUE

  ## Load PFT priors and posteriors
  for (i in seq_along(pft.names)) {
    ## Load posteriors using unified loader
    ## Detects posterior type by content (not filename).
    ## Monte Carlo samples take precedence over distribution summaries.
    posterior <- load.posteriors(
      posterior.file = posterior.files[i],
      outdir = unlist(outdirs[i]),
      posteriorid = settings$pfts[[i]]$posteriorid,
      con = con,
      hostname = settings$host$name
    )

    if (!is.null(posterior$prior.distns)) {
      prior_distns_list[[i]] <- posterior$prior.distns
    }

    if (!is.null(posterior$trait.mcmc)) {
      trait_mcmc_list[[i]] <- posterior$trait.mcmc
      ma.results <- TRUE
      # Joint posteriors (e.g. from PDA) should preserve correlations
      if (posterior$is.joint) {
        independent <- FALSE
      }
    } else {
      ma.results <- FALSE
      # trait_mcmc_list[[i]] stays NULL
    }
  } ### End for loop

  ## ---- Delegate to pure function ----
  result <- get_parameter_samples(
    pft_names         = pft.names,
    prior_distns_list = prior_distns_list,
    trait_mcmc_list   = trait_mcmc_list,
    ensemble.size     = ensemble.size,
    ens.sample.method = ens.sample.method,
    sa_quantiles      = settings$sensitivity.analysis$quantiles, # which is NULL if no SA requested
    do_ensemble       = "ensemble" %in% names(settings),
    independent       = independent
  )

  ## ---- Save to disk for backward compatibility ----
  if (save_to_disk) {
    ensemble.samples <- result$ensemble.samples
    trait.samples    <- result$trait.samples
    sa.samples       <- result$sa.samples
    runs.samples     <- result$runs.samples
    env.samples      <- result$env.samples
    save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples,
         file = file.path(settings$outdir, "samples.Rdata"))
  }

  invisible(result)
}