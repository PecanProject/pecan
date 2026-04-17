#' Complete meta-analysis workflow for a single plant functional type (PFT)
#'
#' @param trait_data (list) Named list of trait data. List item names must be
#' trait names (consistent with `priors` argument). List values are
#' `data.frame`s with the following required columns:
#'  `name`, `mean` `statname`, `stat`, `greenhouse`, `n`,
#'  `site_id`, `specie_id`, `citation_id`, `cultivar_id`,
#'  `date`, `time`, `control`
#' @param priors (list) Named list of priors
#' @param iterations (integer) Number of sampler iterations for MCMC analysis
#' @param outdir (character; default = `tempdir() / "pecan-meta-analysis"`)
#'    Path to directory where outputs will be stored.
#' @param pft_name (character; default = NA) Name of PFT (for logging purposes).
#' @param random (boolean; default = TRUE) Should random effects be used?
#' @param use_ghs (boolean; default = TRUE) If TRUE, do not exclude greenhouse data
#' @param gamma_tau (numeric; default = 0.01) Prior on gamma tau parameter
#' @inheritParams pecan.ma
#' @inheritParams pecan.ma.summary
#'
#' @return (list) List of trait meta-analysis results, including:
#'    - `trait.mcmc`: MCMC samples
#'    - `post.distns`: Posterior distributions
#'    - `jagged.data`: "JAGS-ified" input data (after GHG screen, if applied)
#'
#' @export
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

#' "Workflow" version of run.meta.analysis.pft
#'
#' Thin wrapper around `meta_analysis_standalone` that also reads/writes files 
#' and registers results in the PEcAn database. 
#'
#' @param pft (list) PFT list object, as defined in settings. Must include the
#'  following: `outdir`, `name`, `posteriorid`
#' @param dbfiles (character) directory where previous results are found
#' @param dbcon (DBI connection object) BETY database connection object
#' @param update (boolean; default = TRUE) If `TRUE`, replace existing
#'  posteriors with new ones
#'
#' @inheritParams meta_analysis_standalone
run.meta.analysis.pft <- function(pft, iterations, random = TRUE, threshold = 1.2, dbfiles, dbcon, use_ghs = TRUE, update = FALSE) {
  # check to see if get.trait was executed
  if (!file.exists(file.path(pft$outdir, "trait.data.Rdata")) || 
      !file.exists(file.path(pft$outdir, "prior.distns.Rdata"))) {
    PEcAn.logger::logger.severe("Could not find output from get.trait for", pft$name)
    return(NA)
  }
  
  # check to see if run.meta.analysis can be skipped
  if (file.exists(file.path(pft$outdir, "trait.mcmc.Rdata")) && 
      file.exists(file.path(pft$outdir, "post.distns.Rdata")) && 
      update != TRUE) {
    PEcAn.logger::logger.info("Assuming get.trait copied results already")
    return(pft)
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
  
  ## Load trait data for PFT
  trait_env <- new.env()
  load(file.path(pft$outdir, "trait.data.Rdata"), envir = trait_env)
  prior_env <- new.env()
  load(file.path(pft$outdir, "prior.distns.Rdata"), envir = prior_env)
  
  if (length(trait_env$trait.data) == 0) {
    PEcAn.logger::logger.info("no trait data for PFT", pft$name, "\n so no meta-analysis will be performed")
    return(NA)
  }
  
  # create path where to store files
  pathname <- file.path(dbfiles, "posterior", pft$posteriorid)
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)

  ma_result <- meta_analysis_standalone(
    trait_data = trait_env[["trait.data"]],
    priors = prior_env[["prior.distns"]],
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
} # run.meta.analysis.pft

##--------------------------------------------------------------------------------------------------##
##' Run meta analysis
##'
##' This will use the following items from settings:
##' - settings$pfts
##' - settings$database$bety
##' - settings$database$dbfiles
##' - settings$meta.analysis$update
##'
##' @param pfts the list of pfts to get traits for
##' @param database database connection parameters
##' @param update logical: Rerun the meta-analysis if result files already exist?
##' @param threshold Gelman-Rubin convergence diagnostic, passed on to
##'   \code{\link{pecan.ma.summary}}
##' @inheritParams meta_analysis_standalone
##' @inheritParams run.meta.analysis.pft
##'
##' @return nothing, as side effect saves \code{trait.mcmc} created by
##' \code{\link{pecan.ma}} and post.distns created by
##' \code{\link{approx.posterior}(trait.mcmc, ...)}  to trait.mcmc.Rdata
##'   and post.distns.Rdata, respectively
##' @export
##' @author Shawn Serbin, David LeBauer
run.meta.analysis <- function(pfts, iterations, random = TRUE, threshold = 1.2, dbfiles, database, use_ghs = TRUE , update = FALSE) {
  # process all pfts
  dbcon <- PEcAn.DB::db.open(database)
  on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)

  result <- lapply(pfts, run.meta.analysis.pft, iterations = iterations, random = random, 
                   threshold = threshold, dbfiles = dbfiles, dbcon = dbcon, use_ghs = use_ghs, update = update)
} # run.meta.analysis.R
## ==================================================================================================#
#' Run meta-analysis on all PFTs in a (list of) PEcAn settings
#'
##' @param settings a PEcAn settings or MultiSettings object
##' @return list of PFTs, invisibly;
##'  saves MA results to `settings$pft$outdir` as a side effect
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
    
    run.meta.analysis(
      pfts,
      settings$meta.analysis$iter,
      settings$meta.analysis$random.effects$on,
      settings$meta.analysis$threshold,
      settings$database$dbfiles,
      settings$database$bety,
      settings$meta.analysis$random.effects$use_ghs
    )
  } else if (PEcAn.settings::is.Settings(settings)) {
      run.meta.analysis(
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
