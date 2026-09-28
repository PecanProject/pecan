##' Complete meta-analysis workflow for a single plant functional type (PFT)
##'
##' @param trait_data (list) Named list of trait data. List item names must be
##' trait names (consistent with `priors` argument). List values are
##' `data.frame`s with the following required columns:
##'  `name`, `mean` `statname`, `stat`, `greenhouse`, `n`,
##'  `site_id`, `specie_id`, `citation_id`, `cultivar_id`,
##'  `date`, `time`, `control`
##' @param priors (list) Named list of priors
##' @param iterations (integer) Number of sampler iterations for MCMC analysis
##' @param outdir (character; default = `tempdir() / "pecan-meta-analysis"`)
##'    Path to directory where outputs will be stored.
##' @param pft_name (character; default = NA) Name of PFT (for logging purposes).
##' @param random (boolean; default = TRUE) Should random effects be used?
##' @param use_ghs (boolean; default = TRUE) If TRUE, do not exclude greenhouse data
##' @param gamma_tau (numeric; default = 0.01) Prior on gamma tau parameter
##' @inheritParams pecan.ma
##' @inheritParams pecan.ma.summary
##'
##' @return (list) List of trait meta-analysis results, including:
##'    - `trait.mcmc`: MCMC samples
##'    - `post.distns`: Posterior distributions
##'    - `jagged.data`: "JAGS-ified" input data (after GHG screen, if applied)
##'
##' @export
meta_analysis_standalone <- function(
  trait_data,
  priors,
  iterations,
  outdir = file.path(tempdir(), "pecan-meta-analysis"),
  pft_name = NA_character_,
  random = TRUE,
  threshold = 1.2,
  use_ghs = TRUE,
  gamma_tau = 0.01
) {
  stopifnot(
    is.list(trait_data),
    is.logical(use_ghs),
    is.numeric(gamma_tau),
    gamma_tau > 0
  )

  # Create output directory if it doesn't already exist
  dir.create(outdir, showWarnings = FALSE)

  jagged_data <- lapply(trait_data, PEcAn.MA::jagify, use_ghs = use_ghs)

  if (!use_ghs) {
    # check if any data left after excluding greenhouse
    all_trait_check <- vapply(jagged_data, nrow, numeric(1))
    if (any(all_trait_check == 0)) {
      nodat <- which(all_trait_check == 0)
      jagged_data[nodat] <- NULL
      PEcAn.logger::logger.info(
        paste(
          "No more data left after excluding greenhouse data",
          "for the following traits:"
        ),
        paste(names(all_trait_check)[nodat], collapse = ", ")
      )
    }
  }

  ## Check that data is consistent with prior
  errors <- character()
  warnings <- character()
  for (trait in names(jagged_data)) {
    data_median <- stats::median(jagged_data[[trait]][, "Y"])
    prior       <- priors[trait, ]
    check       <- check_consistent(data_median, prior)
    if (all(check)) {
      next
    }
    if (check[["no_error"]]) {
      warnings <- c(warnings, trait)
    }
    errors <- c(errors, trait)
  }
  if (length(warnings) > 0) {
    msg <- paste0(
      "The following traits *might* be inconsistent with priors: ",
      paste(warnings, collapse = ", ")
    )
    PEcAn.logger::logger.warn(msg)
  }
  if (length(errors) > 0) {
    msg <- paste0(
      "The following traits are inconsistent with priors: ",
      paste(errors, collapse = ", ")
    )
    PEcAn.logger::logger.error(msg)
    stop(msg)
  }

  ## Average trait data
  trait_average <- vapply(
    jagged_data,
    function(x) mean(x[["Y"]], na.rm = TRUE),
    numeric(1)
  )

  ## Set gamma distribution prior
  prior_variances <- as.data.frame(rep(1, nrow(priors)))
  row.names(prior_variances) <- row.names(priors)
  prior_variances[names(trait_average), ] <- 0.001 * trait_average ^ 2
  prior_variances["seedling_mortality", 1] <- 1
  taupriors <- list(
    tauA = gamma_tau,
    tauB = apply(prior_variances, 1, function(x) min(gamma_tau, x))
  )

  ### Run the meta-analysis
  trait_mcmc <- pecan.ma(jagged_data,
                         priors,
                         taupriors,
                         j.iter = iterations,
                         outdir = outdir,
                         random = random)

  ### Check that meta-analysis posteriors are consistent with priors
  errors <- character()
  warnings <- character()
  for (trait in names(trait_mcmc)) {
    post_median <- stats::median(as.matrix(trait_mcmc[[trait]][, "beta.o"]))
    prior       <- priors[trait, ]
    check       <- check_consistent(post_median, prior)
    if (all(check)) {
      next
    }
    if (check[["no_error"]]) {
      warnings <- c(warnings, trait)
    }
    errors <- c(errors, trait)
  }
  if (length(warnings) > 0) {
    msg <- paste0(
      "The following posteriors *might* be inconsistent with priors: ",
      paste(warnings, collapse = ", ")
    )
    PEcAn.logger::logger.warn(msg)
  }
  if (length(errors) > 0) {
    msg <- paste0(
      "The following posteriors are inconsistent with priors: ",
      paste(errors, collapse = ", ")
    )
    PEcAn.logger::logger.error(msg)
    stop(msg)
  }

  # Generate summaries and diagnostics, discard samples if trait failed to
  # converge
  trait_mcmc <- pecan.ma.summary(trait_mcmc, pft_name, outdir, threshold)
  post_distns <- approx.posterior(trait_mcmc, priors, jagged_data, outdir)

  return(list(
    trait.mcmc = trait_mcmc,
    post.distns = post_distns,
    jagged.data = jagged_data
  ))
}

#' Check that a data value is consistent with its prior
#'
#' @param point (numeric) Data value to check
#' @param p_error (numeric) Probability value outside of which we raise an error
#' @param p_warning (numeric) Probability value outside of which we raise a warning
#' @inheritParams p.point.in.prior
#'
#' @return (c(no_error = <boolean>, no_warning = <boolean>))
check_consistent <- function(point, prior,
                             p_error = 5e-04, p_warning = 0.025) {
  stopifnot(p_warning >= p_error)
  p_data <- p.point.in.prior(point = point, prior = prior)
  if ((p_data >= p_warning) && (p_data <= 1 - p_warning)) {
    return(c(no_error = TRUE, no_warning = TRUE))
  }
  if ((p_data >= p_error) && (p_data <= 1 - p_error)) {
    return(c(no_error = TRUE, no_warning = FALSE))
  }
  return(c(no_error = FALSE, no_warning = FALSE))
}

#' Run Bayesian meta-analysis for a single PFT (file-based wrapper)
#'
#' Thin wrapper around \code{\link[PEcAn.MA]{meta_analysis_standalone}} that
#' reads trait data and priors either directly from in-memory R objects (when
#' `trait_data` and `prior_distns` are provided) or from `.Rdata` files in
#' `pft$outdir` (the legacy path). Results are always saved back to
#' `pft$outdir` for scientific provenance and registered in BETYdb.
#'
#' @details
#' **Input contract:** Trait data and priors enter via one of two paths:
#' \describe{
#'   \item{In-memory (preferred for new code)}{Pass both `trait_data` and
#'     `prior_distns` as function arguments. Disk loading is skipped entirely.
#'     Either both arguments must be provided or both must be `NULL` —
#'     passing only one is an error.}
#'   \item{Disk-based (legacy)}{When both `trait_data` and `prior_distns`
#'     are `NULL`, the function reads:
#'     \itemize{
#'       \item `trait.data.Rdata` — Named list of trait data frames produced
#'         by \code{\link[PEcAn.DB]{get.trait.data.pft}}.
#'       \item `prior.distns.Rdata` — Data frame of prior distributions
#'         produced by \code{\link[PEcAn.DB]{get.trait.data.pft}}.
#'     }
#'     loaded from `pft$outdir`.}
#' }
#'
#' **File-based side effects (saved to `pft$outdir`):**
#' \describe{
#'   \item{`trait.mcmc.Rdata`}{Contains `trait.mcmc`: a named list of
#'     `mcmc.list` objects (one per trait) with posterior MCMC samples from
#'     JAGS. Each element has columns `beta.o` (overall mean) and optionally
#'     `sd.o` (overall SD).}
#'   \item{`post.distns.MA.Rdata`}{Contains `post.distns`: a data frame with
#'     one row per trait and columns `distn`, `parama`, `paramb`, `n`
#'     summarizing the fitted posterior distribution.}
#'   \item{`post.distns.Rdata`}{Symlink to `post.distns.MA.Rdata`.}
#'   \item{`jagged.data.Rdata`}{Contains `jagged.data`: a named list of data
#'     frames (one per trait) formatted for use in the JAGS meta-analysis
#'     model (see \code{\link[PEcAn.MA]{jagify}}).}
#' }
#'
#' These saves are unconditional: calling this wrapper is the provenance
#' opt-in mechanism. Callers that need a purely in-memory analysis with no
#' filesystem side effects should call
#' \code{\link[PEcAn.MA]{meta_analysis_standalone}} directly.
#'
#' **Downstream contract:** The files `trait.mcmc.Rdata` and
#' `post.distns.Rdata` are expected by `PEcAn.uncertainty::get.parameter.samples()`
#' (in `PEcAn.uncertainty`), which loads them to generate ensemble and
#' sensitivity analysis samples. The same objects are now also attached to
#' the returned `pft` list (see Value below), so callers wired to the
#' modular pipeline can consume them without re-reading the `.Rdata` files.
#'
#' **Note:** The core computation is performed by
#' \code{\link[PEcAn.MA]{meta_analysis_standalone}}, which accepts and returns
#' R objects directly — see its documentation for the pure-function interface.
#'
#' @param pft (list) PFT list object, as defined in settings. Must include the
#'  following: `outdir`, `name`, `posteriorid`
#' @param dbfiles (character) directory where previous results are found
#' @param dbcon (DBI connection object) BETY database connection object
#' @param update (boolean; default = FALSE) If `TRUE`, replace existing
#'   posteriors with new ones
#' @param trait_data (named list; default = `NULL`) Optional in-memory trait
#'   data with the same structure as the contents of `trait.data.Rdata`. When
#'   non-`NULL`, must be provided alongside `prior_distns` and the `.Rdata`
#'   files in `pft$outdir` are not read.
#' @param prior_distns (data frame; default = `NULL`) Optional in-memory prior
#'   distributions with the same structure as the contents of
#'   `prior.distns.Rdata`. When non-`NULL`, must be provided alongside
#'   `trait_data` and the `.Rdata` files in `pft$outdir` are not read.
#' @param return_data (boolean; default = FALSE) If `TRUE`, attach `trait.mcmc`,
#'   `post.distns`, and `jagged.data` to the returned `pft` for in-memory
#'   chaining. Defaults to `FALSE` to preserve legacy behavior — attaching
#'   these objects to a `pft` that is embedded in a settings object would
#'   inflate the settings and can break serialization.
#'
#' @return The `pft` list with three additional elements attached, or `NA`
#'   if no trait data are available for this PFT. The returned `pft` list
#'   contains:
#'   \describe{
#'     \item{`name`}{(character) PFT name, e.g. `"temperate.deciduous"`.}
#'     \item{`outdir`}{(character) Path to directory where output files are
#'       stored (trait data, priors, posteriors, MCMC samples).}
#'     \item{`posteriorid`}{(integer) Row ID of the posterior record in
#'       BETYdb's `posteriors` table.}
#'     \item{`constants`}{(named list, optional) Trait values to treat as
#'       fixed constants, bypassing the meta-analysis.}
#'     \item{`trait.mcmc`}{(named list) `mcmc.list` objects, one per trait.}
#'     \item{`post.distns`}{(data frame) Fitted posterior distributions.}
#'     \item{`jagged.data`}{(named list) JAGS-formatted data frames.}
#'   }
#'   File-based side effects in `pft$outdir` are preserved for backward
#'   compatibility and scientific provenance.
#'
#' @inheritParams meta_analysis_standalone
run.meta.analysis.pft <- function(pft, iterations,
                                  random = TRUE, threshold = 1.2,
                                  dbfiles, dbcon,
                                  use_ghs = TRUE, update = FALSE,
                                  trait_data = NULL,
                                  prior_distns = NULL,
                                  return_data = FALSE) {

  # Attach MA outputs to the pft only when the caller opts in. Default FALSE
  # preserves legacy behavior: when a pft is folded back into a settings
  # object, attaching large mcmc/jagged objects would bloat settings and can
  # break serialization. Modular callers that consume results in-memory pass
  # return_data = TRUE. (Same opt-in pattern as get.trait.data.pft, #3978.)
  attach_results <- function(pft, trait.mcmc, post.distns, jagged.data = NULL) {
    if (!return_data) {
      return(pft)
    }
    pft$trait.mcmc  <- trait.mcmc
    pft$post.distns <- post.distns
    if (!is.null(jagged.data)) {
      pft$jagged.data <- jagged.data
    }
    pft
  }

  # Validate the in-memory inputs: both must be provided together or both
  # must be NULL. Mixing the two modes (e.g. updated trait data against
  # stale priors loaded from disk) is almost always a mistake, so we catch
  # it loudly rather than silently falling back to a partial disk load.
  trait_provided <- !is.null(trait_data)
  prior_provided <- !is.null(prior_distns)
  if (trait_provided != prior_provided) {
    PEcAn.logger::logger.severe(
      "`trait_data` and `prior_distns` must both be provided together,",
      "or both must be NULL.",
      "Got: trait_data =", if (trait_provided) "<provided>" else "NULL",
      ", prior_distns =", if (prior_provided) "<provided>" else "NULL"
    )
  }
  in_memory_inputs <- trait_provided

  # check to see if get.trait was executed (only required for the disk path —
  # in-memory callers have already supplied the data we'd otherwise load)
  if (!in_memory_inputs) {
    if (!file.exists(file.path(pft$outdir, "trait.data.Rdata")) ||
        !file.exists(file.path(pft$outdir, "prior.distns.Rdata"))) {
      PEcAn.logger::logger.severe("Could not find output from get.trait for", pft$name)
      return(NA)
    }
  }

  # check to see if run.meta.analysis can be skipped
  if (file.exists(file.path(pft$outdir, "trait.mcmc.Rdata")) &&
      file.exists(file.path(pft$outdir, "post.distns.Rdata")) &&
      update != TRUE) {
    PEcAn.logger::logger.info("Assuming get.trait copied results already")

    # Legacy default (return_data = FALSE): return the bare pft without reading
    # anything back from disk.
    if (!return_data) {
      return(pft)
    }

    # return_data = TRUE: load the cached results and attach them so the
    # modular chain gets the same return contract on a cache-hit as it does
    # on a fresh analysis.
    mcmc_env <- new.env()
    load(file.path(pft$outdir, "trait.mcmc.Rdata"), envir = mcmc_env)
    post_env <- new.env()
    load(file.path(pft$outdir, "post.distns.Rdata"), envir = post_env)

    # jagged.data is optional: older posteriors copied via get.trait.data.pft
    # may not include this file, so attach only if it exists on disk.
    jagged.data <- NULL
    jagged_path <- file.path(pft$outdir, "jagged.data.Rdata")
    if (file.exists(jagged_path)) {
      jagged_env <- new.env()
      load(jagged_path, envir = jagged_env)
      jagged.data <- jagged_env[["jagged.data"]]
    }

    return(attach_results(pft, mcmc_env[["trait.mcmc"]],
                          post_env[["post.distns"]], jagged.data))
  }

  # make sure there is a posteriorid
  if (is.null(pft$posteriorid)) {
    PEcAn.logger::logger.severe("Make sure to pass in pft list from get.trait. Missing posteriorid for", pft$name)
    return(NA)
  }

  # make sure random and use_ghs is logical, and threshold is numeric
  # when someone re-reads xml and continues from meta.analysis these can cause bugs (especially the threshold bug is very subtle)
  random    <- as.logical(random)
  use_ghs   <- as.logical(use_ghs)
  threshold <- as.numeric(threshold)

  # get list of existing files so they get ignored saving
  old.files <- list.files(path = pft$outdir)

  PEcAn.logger::logger.info("-------------------------------------------------------------------")
  PEcAn.logger::logger.info(" Running meta.analysis for PFT:", pft$name)
  PEcAn.logger::logger.info("-------------------------------------------------------------------")

  ## Resolve trait data and priors: prefer in-memory objects when provided,
  ## otherwise fall back to loading from pft$outdir (legacy path).
  if (in_memory_inputs) {
    PEcAn.logger::logger.debug(
      "Using in-memory `trait_data` and `prior_distns`;",
      "skipping load() from", pft$outdir
    )
    trait.data <- trait_data
    prior.distns <- prior_distns
  } else {
    ## Load trait data for PFT
    trait_env <- new.env()
    load(file.path(pft$outdir, "trait.data.Rdata"), envir = trait_env)
    prior_env <- new.env()
    load(file.path(pft$outdir, "prior.distns.Rdata"), envir = prior_env)
    trait.data <- trait_env[["trait.data"]]
    prior.distns <- prior_env[["prior.distns"]]
  }

  if (length(trait.data) == 0) {
    PEcAn.logger::logger.info("no trait data for PFT", pft$name, "\n so no meta-analysis will be performed")
    return(NA)
  }

  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ma_result <- meta_analysis_standalone(
    trait_data = trait.data,
    priors = prior.distns,
    iterations = iterations,
    pft_name = pft[["name"]],
    outdir = pft[["outdir"]],
    random = random,
    threshold = threshold,
    use_ghs = use_ghs
  )

  ## Save the jagged.data object, replaces previous madata.Rdata object
  ## First 6 columns are equivalent and direct inputs into the meta-analysis

  # NOTE: `save` saves R objects under their names in the current environment,
  # so you cannot just do `save(ma_result[["jagged.data"]])` -- that will throw
  # an error.
  # TODO: We should really use `saveRDS` / `readRDS` for this everywhere...but
  # for now, this is a workaround.
  jagged.data <- ma_result[["jagged.data"]]
  save(jagged.data, file = file.path(pft$outdir, "jagged.data.Rdata"))
  rm(jagged.data)

  ### Save the meta.analysis output
  trait.mcmc <- ma_result[["trait.mcmc"]]
  save(trait.mcmc, file = file.path(pft$outdir, "trait.mcmc.Rdata"))
  rm(trait.mcmc)

  dist_MA_path <- file.path(pft$outdir, "post.distns.MA.Rdata")
  post.distns <- ma_result[["post.distns"]]
  save(post.distns, file = dist_MA_path)
  rm(post.distns)

  dist_path <- file.path(pft$outdir, "post.distns.Rdata")

  # Symlink to post.distns.Rdata (no 'MA' identifier)
  if (file.exists(dist_path)) {
    file.remove(dist_path)
  }
  file.symlink(dist_MA_path, dist_path)

  ### save and store in database all results except those that were there already
  for (file in list.files(path = pft$outdir)) {
    # Skip file if it was there already, or if it's a symlink (like the post.distns.Rdata link above)
    if (file %in% old.files || nchar(Sys.readlink(file.path(pft$outdir, file))) > 0) {
      next
    }
    filename <- file.path(pathname, file)
    file.copy(file.path(pft$outdir, file), filename)
    PEcAn.DB::dbfile.insert(pathname, file, "Posterior", pft$posteriorid, dbcon)
  }

  # Attach analysis results to the returned pft (when return_data = TRUE) so
  # downstream functions (e.g. get.parameter.samples) can consume them
  # in-memory without re-reading the .Rdata files we just wrote for provenance.
  # The rm() calls above only removed the local aliases — the data is still
  # alive in `ma_result`.
  return(attach_results(pft, ma_result[["trait.mcmc"]],
                        ma_result[["post.distns"]], ma_result[["jagged.data"]]))
} # run.meta.analysis.pft

##--------------------------------------------------------------------------------------------------##
##' Run meta-analysis across all PFTs
##'
##' Iterates over a list of PFTs and runs \code{\link[PEcAn.MA]{run.meta.analysis.pft}} for each
##' one. This is the main entry point called by \code{\link[PEcAn.MA]{runModule.run.meta.analysis}}.
##'
##' This will use the following items from settings:
##' - `settings$pfts`
##' - `settings$database$bety`
##' - `settings$database$dbfiles`
##' - `settings$meta.analysis$update`
##'
##' @param pfts the list of pfts to get traits for
##' @param database database connection parameters
##' @param update logical: Rerun the meta-analysis if result files already exist?
##' @param threshold Gelman-Rubin convergence diagnostic, passed on to
##'  \code{\link{pecan.ma.summary}}
##' @inheritParams meta_analysis_standalone
##' @inheritParams run.meta.analysis.pft
##'
##' @return Invisibly, a list (one element per input PFT) of the values
##'   returned by \code{\link{run.meta.analysis.pft}}. Each element is either
##'   the input `pft` list with `trait.mcmc`, `post.distns`, and `jagged.data`
##'   attached, or `NA` when no meta-analysis was performed for that PFT.
##'   Provenance files (`trait.mcmc.Rdata`, `post.distns.Rdata`, etc.) are
##'   also written to each `pft$outdir` as a side effect.
##' @export
##' @author Shawn Serbin, David LeBauer
run.meta.analysis <- function(pfts, iterations, random = TRUE, threshold = 1.2, dbfiles, database, use_ghs = TRUE , update = FALSE, return_data = FALSE) {
  # process all pfts
  dbcon <- PEcAn.DB::db.open(database)
  on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)

  result <- lapply(pfts, run.meta.analysis.pft, iterations = iterations, random = random,
                   threshold = threshold, dbfiles = dbfiles, dbcon = dbcon, use_ghs = use_ghs, update = update,
                   return_data = return_data)
  invisible(result)
} # run.meta.analysis.R
## ==================================================================================================#
#' Run meta-analysis on all PFTs in a (list of) PEcAn settings
#'
##' @param settings a PEcAn settings or MultiSettings object
##' @return Invisibly, the list of PFTs returned by \code{run.meta.analysis},
##'   each with `trait.mcmc`, `post.distns`, and `jagged.data` attached (or
##'   `NA` for PFTs with no trait data). MA results are also saved to
##'   `settings$pft$outdir` as a side effect.
##' @export
runModule.run.meta.analysis <- function(settings) {
  if (PEcAn.settings::is.MultiSettings(settings)) {
    pfts <- list()
    pft.names <- character(0)
    for (i in seq_along(settings)) {
      pfts.i      <- settings[[i]]$pfts
      pft.names.i <- sapply(pfts.i, function(x) x$name)
      ind         <- which(pft.names.i %in% setdiff(pft.names.i, pft.names))
      pfts        <- c(pfts, pfts.i[ind])
      pft.names   <- sapply(pfts, function(x) x$name)
    }

    PEcAn.logger::logger.info(paste0("Running meta-analysis on all PFTs listed by any Settings object in the list: ",
                       paste(pft.names, collapse = ", ")))

    result <- run.meta.analysis(
      pfts,
      settings$meta.analysis$iter,
      settings$meta.analysis$random.effects$on,
      settings$meta.analysis$threshold,
      settings$database$dbfiles,
      settings$database$bety,
      settings$meta.analysis$random.effects$use_ghs
    )
  } else if (PEcAn.settings::is.Settings(settings)) {
      result <- run.meta.analysis(
        settings$pfts,
        settings$meta.analysis$iter,
        settings$meta.analysis$random.effects$on,
        settings$meta.analysis$threshold,
        settings$database$dbfiles,
        settings$database$bety,
        settings$meta.analysis$random.effects$use_ghs,
        update = settings$meta.analysis$update
      )
  } else {
    stop("runModule.run.meta.analysis only works with Settings or MultiSettings")
  }
  invisible(result)
} # runModule.run.meta.analysis

##--------------------------------------------------------------------------------------------------#
##' compare point to prior distribution
##'
##' used to compare data to prior, meta analysis posterior to prior
##' @title find quantile of point within prior distribution
##' @param point quantile of given prior to return
##' @param prior list of distn, parama, paramb
##' @return result of `p<distn>(point, parama, paramb)`
##' @author David LeBauer
p.point.in.prior <- function(point, prior) {
  out <- do.call(paste0("p", prior$distn),
                 list(point, prior$parama, prior$paramb))
  return(out)
} # p.point.in.prior