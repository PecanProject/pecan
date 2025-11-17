#' Convert priors / MCMC samples to chains that can be sampled
#'   for model parameters
#'
#' @param settings PEcAn settings object
#' @param ensemble.size number of runs in model ensemble
#' @param posterior.files Either:
#'   - a vector/list of length `length(settings$pfts)` with paths to posterior/ prior
#'     distribution `.Rdata` files (backwards compatible behavior), OR
#'   - a list of length `length(settings$pfts)`, where each element is itself a list
#'     that may contain named elements `distribution` (path to `post.distns.Rdata` or
#'     `prior.distns.Rdata`) and `mcmc` (path to `trait.mcmc*.Rdata`). When provided,
#'     these files are used directly and PEcAn will not fall back to `pft$outdir`
#'     to look for inputs. This avoids mixing read-only inputs in output directories.
#' @param ens.sample.method one of "halton", "sobol", "torus", "lhc", "uniform"
#' @export
#'
#' @author David LeBauer, Shawn Serbin, Istem Fer
#' @importFrom rlang %||%
get.parameter.samples <- function(settings,
                                  ensemble.size = 1,
                                  posterior.files = rep(NA, length(settings$pfts)),
                                  ens.sample.method = "uniform") {
  ### Identify PFTs in the input settings.xml file
  pfts <- settings$pfts
  pft.names <- list()
  outdirs <- list()

  # Normalize to list for consistent handling (backwards compatible)
  posterior_list <- as.list(posterior.files)
  if (length(pfts) != length(posterior_list)) {
    PEcAn.logger::logger.error(
      "settings$pfts and posterior.files should be the same length"
    )
  }

  ## Open database connection
  con <- try(PEcAn.DB::db.open(settings$database$bety))
  on.exit(try(PEcAn.DB::db.close(con), silent = TRUE), add = TRUE)

  # If we fail to connect to DB then we set to NULL
  if (inherits(con, "try-error")) {
    con <- NULL
    PEcAn.logger::logger.warn(
      "We were not able to successfully establish a connection with Bety "
    )
  }

for (i.pft in seq_along(pfts)) {
    # If no name given, use string "NULL" to warn user
    pft.names[i.pft] <- settings$pfts[[i.pft]]$name %||% "NULL"

    ### Get output directory info (used for outputs and legacy fallbacks only)
    if (!is.null(settings$pfts[[i.pft]]$outdir)) {
      outdirs[i.pft] <- settings$pfts[[i.pft]]$outdir
    } else {
      outdirs[i.pft] <- unique(
        PEcAn.DB::dbfile.check(
          type = "Posterior",
          container.id = settings$pfts[[i.pft]]$posteriorid,
          con = con
        )$file_path
      )
    }
  } ### End of for loop to extract pft names

  PEcAn.logger::logger.info("Selected PFT(s): ", pft.names)

  ## Generate empty list arrays for output.
  trait.samples <- sa.samples <- ensemble.samples <- env.samples <- list()
  runs.samples <- param.names <- list()

  # flag determining whether samples are independent
  # (e.g. when params fitted individually)
  independent <- TRUE

  ## Load PFT priors and posteriors
  for (i in seq_along(pft.names)) {
    distns <- new.env()

    # Per-PFT overrides (may be NA, a string path, or a list with
    #   $distribution and/or $mcmc entries)
    pf_spec <- posterior_list[[i]]
    dist_file <- NA
    mcmc_file_arg <- NA
    if (is.list(pf_spec)) {
      dist_file <- pf_spec$distribution %||% NA
      mcmc_file_arg <- pf_spec$mcmc %||% NA
    } else {
      dist_file <- pf_spec
    }

    ## Load distribution (posterior/ prior) file
    if (!is.na(dist_file)) {
      # Load specified file
      load(dist_file, envir = distns)
      if (is.null(distns$prior.distns) && !is.null(distns$post.distns)) {
        distns$prior.distns <- distns$post.distns
      }
    } else {
      # Default to most recent posterior in the workflow,
      # or the prior if there is none
      fname <- file.path(outdirs[i], "post.distns.Rdata")
      if (file.exists(fname)) {
        load(fname, envir = distns)
        distns$prior.distns <- distns$post.distns
      } else {
        load(file.path(outdirs[i], "prior.distns.Rdata"), envir = distns)
      }
    }

    ### Load trait mcmc data (if exists, either from MA or PDA)
    if (!is.na(mcmc_file_arg)) {
      # Explicit path provided -> trust it and do not fall back to outdir
      if (!file.exists(mcmc_file_arg)) {
        PEcAn.logger::logger.severe("Specified MCMC file does not exist: ", mcmc_file_arg)
      }
      ma.results <- TRUE
      load(mcmc_file_arg, envir = distns)
      if (grepl("mcmc.pda", mcmc_file_arg)) independent <- FALSE
    } else if (!is.null(settings$pfts[[i]]$posteriorid) && !is.null(con)) {
      # first check if there are any files associated with posterior ids
      files <- PEcAn.DB::dbfile.check("Posterior",
        settings$pfts[[i]]$posteriorid,
        con, settings$host$name,
        return.all = TRUE
      )
      tid <- grep("trait.mcmc.*Rdata", files$file_name)
      if (length(tid) > 0) {
        trait.mcmc.file <- file.path(files$file_path[tid], files$file_name[tid])
        ma.results <- TRUE
        load(trait.mcmc.file, envir = distns)

        # PDA samples are fitted together, to preserve correlations downstream
        # let workflow know they should go together
        if (grepl("mcmc.pda", trait.mcmc.file)) independent <- FALSE
        # NOTE: Global MA samples will also be together, right?
      } else {
        PEcAn.logger::logger.info(
          "No trait.mcmc file is associated with this posterior ID."
        )
        ma.results <- FALSE
      }
    } else if ("trait.mcmc.Rdata" %in% dir(unlist(outdirs[i]))) {
      PEcAn.logger::logger.info(
        "Defaulting to trait.mcmc file in the pft directory."
      )
      ma.results <- TRUE
      load(file.path(outdirs[i], "trait.mcmc.Rdata"), envir = distns)
    } else {
      ma.results <- FALSE
    }

    pft.name <- unlist(pft.names[i])

    ### When no ma for a trait, sample from prior
    ### Trim all chains to shortest mcmc chain, else 20000 samples
    if (!is.null(distns$prior.distns)) {
      priors <- rownames(distns$prior.distns)
    } else {
      priors <- NULL
    }
    if (!is.null(distns$trait.mcmc)) {
      param.names[[i]] <- names(distns$trait.mcmc)
      names(param.names)[i] <- pft.name

      samples.num <- min(
        sapply(distns$trait.mcmc, function(x) nrow(as.matrix(x)))
      )

      ## report which traits use MA results, which use priors
      if (length(param.names[[i]]) > 0) {
        PEcAn.logger::logger.info(
          "PFT", pft.names[i], "has MCMC samples for:\n",
          paste0(param.names[[i]], collapse = "\n ")
        )
      }
      if (!all(priors %in% param.names[[i]])) {
        PEcAn.logger::logger.info(
          "PFT", pft.names[i], "will use prior distributions for:\n",
          paste0(priors[!priors %in% param.names[[i]]], collapse = "\n ")
        )
      }
    } else {
      param.names[[i]] <- list()
      samples.num <- 20000
      PEcAn.logger::logger.info("No MCMC results for PFT", pft.names[i])
      PEcAn.logger::logger.info(
        "PFT", pft.names[i], "will use prior distributions for",
        priors
      )
    }
    if (is.null(priors)) priors <- param.names[[i]]

    PEcAn.logger::logger.info("using ", samples.num, "samples per trait")
    if (ens.sample.method == "halton") {
      q_samples <- randtoolbox::halton(n = samples.num, dim = length(priors))
    } else if (ens.sample.method == "sobol") {
      q_samples <- randtoolbox::sobol(
        n = samples.num,
        dim = length(priors),
        scrambling = 3
      )
    } else if (ens.sample.method == "torus") {
      q_samples <- randtoolbox::torus(n = samples.num, dim = length(priors))
    } else if (ens.sample.method == "lhc") {
      q_samples <- PEcAn.emulator::lhc(
        t(matrix(0:1, ncol = length(priors), nrow = 2)),
        samples.num
      )
    } else if (ens.sample.method == "uniform") {
      q_samples <- matrix(
        stats::runif(samples.num * length(priors)),
        samples.num,
        length(priors)
      )
    } else {
      PEcAn.logger::logger.info(
        "Method ", ens.sample.method,
        " has not been implemented yet, using uniform random sampling"
      )
      # uniform random
      q_samples <- matrix(
        stats::runif(samples.num * length(priors)),
        samples.num,
        length(priors)
      )
    }
    for (prior in priors) {
      if (prior %in% param.names[[i]]) {
        samples <- distns$trait.mcmc[[prior]] %>%
          purrr::map(~ .x[, "beta.o"]) %>%
          unlist() %>%
          as.matrix()
      } else {
        samples <- PEcAn.priors::get.sample(
          distns$prior.distns[prior, ],
          samples.num,
          q_samples[, priors == prior]
        )
      }
      trait.samples[[pft.name]][[prior]] <- samples
    }
  } ### End for loop

  # if samples are independent, set param.names to NULL
  # this is important for downstream, when param.names is not NULL
  # MCMC will be sampled accordingly
  if (independent) {
    param.names <- NULL
  }

  if ("sensitivity.analysis" %in% names(settings)) {
    ### Get info on the quantiles to be run in the sensitivity analysis
    ### (if requested)
    quantiles <- PEcAn.utils::get.quantiles(
      settings$sensitivity.analysis$quantiles
    )
    ### Get info on the years to run the sensitivity analysis (if requested)
    sa.years <- data.frame(
      sa.start = settings$sensitivity.analysis$start.year,
      sa.end = settings$sensitivity.analysis$end.year
    )

    PEcAn.logger::logger.info(
      "\n Selected Quantiles: ",
      PEcAn.utils::vecpaste(round(quantiles, 3))
    )

    ### Generate list of sample quantiles for SA run
    sa.samples <- PEcAn.utils::get.sa.sample.list(
      pft = trait.samples, env = env.samples,
      quantiles = quantiles
    )
  }
  if ("ensemble" %in% names(settings)) {
    if (ensemble.size == 1) {
      ## run at median if only one run in ensemble
      ensemble.samples <- PEcAn.utils::get.sa.sample.list(
        pft = trait.samples,
        env = env.samples,
        quantiles = 0.5
      )
    } else if (ensemble.size > 1) {
      ## subset the trait.samples to ensemble size using Halton sequence
      ensemble.samples <- get.ensemble.samples(
        ensemble.size, trait.samples,
        env.samples, ens.sample.method, param.names
      )
    }
  }

  save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples,
    file = file.path(settings$outdir, "samples.Rdata")
  )
} # get.parameter.samples
