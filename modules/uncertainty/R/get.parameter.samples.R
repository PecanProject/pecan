#' Convert priors / MCMC samples to parameter sample chains
#'
#' Loads posterior distributions and MCMC chain results from disk, generates
#' parameter samples for ensemble and sensitivity analysis runs, and optionally
#' saves results to `samples.Rdata`. This is the backward-compatible wrapper
#' that delegates computation to \code{\link[PEcAn.uncertainty]{get_parameter_samples}}.
#'
#' @details
#' **Upstream contract (reads from each PFT's `outdir`):**
#' \describe{
#'   \item{`post.distns.Rdata` or `prior.distns.Rdata`}{Posterior (or prior)
#'     distribution summaries produced by \code{run.meta.analysis.pft}. A data
#'     frame with columns `distn`, `parama`, `paramb`, `n`.}
#'   \item{`trait.mcmc.Rdata`}{(Optional) MCMC chain samples from the
#'     meta-analysis. Named list of `mcmc.list` objects, one per trait.
#'     If present, samples are drawn from the chains directly; otherwise,
#'     independent samples are drawn from `post.distns`.}
#' }
#'
#' **File-based side effects (saved to `settings$outdir`):**
#' \describe{
#'   \item{`samples.Rdata`}{When `outdir` is non-`NULL` (the default), bundles 5 objects:
#'     \itemize{
#'       \item `trait.samples` — Named list (PFT -> trait -> numeric vector of
#'         length `iterations`). Raw MCMC or prior-sampled values.
#'       \item `sa.samples` — Named list (PFT -> matrix\[n_quantiles x
#'         n_traits\]). Quantile-based samples for sensitivity analysis.
#'       \item `ensemble.samples` — Named list (PFT -> data frame\[ensemble.size
#'         x n_traits\]). Subsampled parameter sets for ensemble runs.
#'       \item `env.samples` — Currently empty list (reserved for
#'         environmental samples).
#'       \item `runs.samples` — Currently empty list (reserved for run
#'         metadata).
#'     }}
#' }
#'
#' **Downstream contract:** `samples.Rdata` is loaded by \code{run.write.configs}
#' (in `PEcAn.workflow`) to generate model configuration files. It is also
#' loaded by \code{\link[PEcAn.uncertainty]{get.results}} and \code{\link[PEcAn.uncertainty]{run.sensitivity.analysis}} to retrieve
#' sample metadata for post-processing. This implicit file-based coupling is
#' a refactoring target.
#'
#' @param settings PEcAn settings object
#' @param ensemble.size number of runs in model ensemble
#' @param posterior.files list of filenames to read from
#' @param ens.sample.method one of `"halton"`, `"sobol"`, `"torus"`, `"lhc"`,
#'   `"uniform"`
#' @param outdir character path; directory to write `samples.Rdata` to for
#'   provenance. Defaults to `settings$outdir`, preserving the legacy
#'   always-save behaviour for existing callers (none of which pass it).
#'   Pass `outdir = NULL` to skip the save entirely; the pure
#'   `get_parameter_samples()` never writes to disk regardless.
#'
#' @return Named list with 5 elements: `trait.samples`, `sa.samples`,
#'   `ensemble.samples`, `runs.samples`, `env.samples`. Returned invisibly.
#'
#' @md
#'
#' @author David LeBauer, Shawn Serbin, Istem Fer, Om Kapale
#' @importFrom rlang %||%
#'
#' @export
get.parameter.samples <- function(settings,
                                  ensemble.size = 1,
                                  posterior.files = rep(NA, length(settings$pfts)),
                                  ens.sample.method = "uniform",
                                  outdir = settings$outdir) {
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
    } else if (!is.null(con)) {
      outdirs[i.pft] <- unique(
        PEcAn.DB::dbfile.check(
          type = "Posterior",
          container.id = pfts[[i.pft]]$posteriorid,
          con = con
        )$file_path
      )
    } # else do nothing: outdirs[i.pft] stays NULL and load.posteriors will handle it

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

  ## ---- Save to disk for provenance (opt-in via `outdir`) ----
  # `outdir` defaults to `settings$outdir`, so existing callers (none of which
  # pass it) write `samples.Rdata` exactly as before. Pass `outdir = NULL` to
  # skip the save for a purely in-memory call; the pure get_parameter_samples()
  # never writes to disk at all.
  if (!is.null(outdir)) {
    ensemble.samples <- result$ensemble.samples
    trait.samples    <- result$trait.samples
    sa.samples       <- result$sa.samples
    runs.samples     <- result$runs.samples
    env.samples      <- result$env.samples
    save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples,
         file = file.path(outdir, "samples.Rdata"))
  }

  invisible(result)
}