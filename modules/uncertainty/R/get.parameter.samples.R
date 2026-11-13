#' Convert priors / MCMC samples to parameter sample chains
#'
#' Loads posterior distributions and MCMC chain results from disk, generates
#' parameter samples for ensemble and sensitivity analysis runs, and optionally
#' saves results to `samples.Rdata`. This is the deprecated wrapper
#' that delegates computation to \code{\link[PEcAn.uncertainty]{get_parameter_samples}}.
#'
#' @details
#' **Upstream contract (reads from each PFT's `outdir`):**
#' \describe{
#'   \item{`post.distns.Rdata` or `prior.distns.Rdata`}{Posterior (or prior)
#'     distribution summaries produced by \code{run.meta.analysis.pft}. A data
#'     frame with columns `distn`, `parama`, `paramb`, `n`.}
#'   \item{`trait.mcmc.Rdata`}{(Optional) posterior MCMC samples.
#'     Named list of `mcmc.list` objects, one per trait.
#'     If present, samples are drawn from the chains directly; otherwise,
#'     independent samples are drawn from `post.distns`.}
#' }
#'
#' **File-based side effects (saved to `settings$outdir`):**
#' \describe{
#'   \item{`samples.Rdata`}{When `outdir` is non-`NULL` (the default), bundles 5 objects:
#'     \itemize{
#'       \item `trait.samples` - Named list (PFT -> trait -> numeric vector of
#'         length `iterations`). Raw MCMC or prior-sampled values.
#'       \item `sa.samples` - Named list (PFT -> matrix\[n_quantiles x
#'         n_traits\]). Quantile-based samples for sensitivity analysis.
#'       \item `ensemble.samples` - Named list (PFT -> data frame\[ensemble.size
#'         x n_traits\]). Subsampled parameter sets for ensemble runs.
#'       \item `env.samples` - Currently empty list (reserved for
#'         environmental samples).
#'       \item `runs.samples` - Currently empty list (reserved for run
#'         metadata).
#'     }}
#' }
#'
#' **Downstream contract:** The workflow passes the parameter bundle directly to
#' \code{run.write.configs} in memory. That writer can also load `samples.Rdata`
#' when no bundle is supplied. \code{\link[PEcAn.uncertainty]{get.results}} can
#' read the saved file when ensemble-specific samples are unavailable, and
#' \code{\link[PEcAn.uncertainty]{run.sensitivity.analysis}} reads it for traits.
#'
#' @param settings PEcAn settings object
#' @param ensemble.size number of runs in model ensemble
#' @param posterior.files list of filenames to read from
#' @param ens.sample.method marginal sampling method: `"halton"`, `"sobol"`,
#'   `"lhc"`, `"random"`, or `"uniform"`.
#' @param outdir directory for `samples.Rdata`, defaulting to `settings$outdir`.
#'   Use `NULL` to return samples without saving a file.
#' @param independent Logical scalar. If `FALSE` (default), preserve
#'   dependence among supplied MCMC parameters. If `TRUE`, sample each
#'   parameter separately, ignoring supplied dependence.
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
                                  outdir = settings$outdir,
                                  independent = FALSE) {
  stopifnot(is.logical(independent), length(independent) == 1L, !is.na(independent))
  .Deprecated("get_parameter_samples")

  ### Identify PFTs in the input settings.xml file
  pfts <- settings$pfts
  if (length(pfts) != length(posterior.files)) {
    PEcAn.logger::logger.error(
      "settings$pfts and posterior.files should be the same length"
    )
  }

  ## Load priors and posteriors (shared loader; see load_pft_posteriors)
  loaded <- load_pft_posteriors(settings, posterior.files)
  pft.names         <- loaded$pft_names
  prior_distns_list <- loaded$prior_distns_list
  trait_mcmc_list   <- loaded$trait_mcmc_list

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
