#' Write model-specific run scripts and configuration files
#'
#' @md
#' Generates run scripts and configuration files for all analyses (ensemble
#' and/or sensitivity analysis) specified in the provided settings. Delegates
#' the model-specific config writing to the appropriate `write.config.*`
#' function (e.g. `write.config.ED2`, `write.config.SIPNET`).
#'
#' @details
#' **Upstream contract (reads from `settings$outdir`):**
#' \describe{
#'   \item{`samples.Rdata`}{Produced by \code{\link[PEcAn.uncertainty]{get.parameter.samples}}.
#'     Contains 5 bundled objects: `trait.samples`,
#'     `sa.samples`, `ensemble.samples`, `runs.samples`, `env.samples`.
#'     This function loads `trait.samples` and `sa.samples` to build
#'     model configuration files. If `input_design` contains a `param`
#'     column, `ensemble.samples` is rebuilt by subsetting `trait.samples`
#'     according to the design indices.}
#' }
#'
#' **File-based side effects (saved to `settings$outdir`):**
#' \describe{
#'   \item{`sensitivity.samples.<ensemble_id>.Rdata`}{Contains `sa.run.ids`
#'     (named list of run IDs per PFT/trait/quantile), `sa.ensemble.id`,
#'     `sa.samples`, `pft.names`, and `trait.names`. Saved when sensitivity
#'     analysis is configured.}
#'   \item{`ensemble.samples.<ensemble_id>.Rdata`}{Contains `ens.run.ids`
#'     (vector of run IDs), `ens.ensemble.id`, `ens.samples`, `pft.names`,
#'     and `trait.names`. Saved when ensemble is configured.}
#'   \item{`runs_manifest.csv`}{A CSV table tracking all runs created,
#'     appended across ensemble and SA analyses.}
#' }
#'
#' **Downstream contract:** The `sensitivity.samples.*.Rdata` and
#' `ensemble.samples.*.Rdata` files are loaded by \code{\link[PEcAn.uncertainty]{get.results}}
#'  to match model outputs to their corresponding
#'  parameter sets. This implicit file-based coupling is a refactoring target.
#'
#' The default value for `posterior.files` is NA, in which case the
#'    most recent posterior or prior (in that order) for the workflow is used.
#'    When specified, `posterior.files` should be a vector of filenames with one
#'    entry for each PFT. Specify filenames with no path; PFT outdirs will be
#'    appended. This forces use of only files within this workflow, to avoid
#'    confusion.
#'
#' @param settings a PEcAn settings list
#' @param ensemble.size number of ensemble runs
#' @param input_design Input design data.frame coordinating input files across
#'   runs. Contains columns for each sampled input (met, param, etc.) with row
#'   indices, as documented in \code{\link[PEcAn.workflow]{runModule.run.write.configs}}.
#' @param write should the runs be written to the database?
#' @param posterior.files Filenames for posteriors for drawing samples for
#'   ensemble and sensitivity analysis (e.g. `post.distns.Rdata`, or
#'   `prior.distns.Rdata`).
#' @param overwrite logical: Replace output files that already exist?
#'
#' @return The `settings` list (invisibly), updated with ensemble IDs for SA
#'   and ensemble analysis (e.g. `settings$sensitivity.analysis$ensemble.id`,
#'   `settings$ensemble$ensemble.id`).
#' @export
#'
#' @author David LeBauer, Shawn Serbin, Ryan Kelly, Mike Dietze, Akash B V

run.write.configs <- function(settings, ensemble.size, input_design, write = TRUE,
                              posterior.files = rep(NA, length(settings$pfts)),
                              overwrite = TRUE) {

  # Validate that input_design matches ensemble.size for ensemble runs
  # Note: for SA, ensemble.size is not meaningful; SA design size is determined by
  # number of (pft, trait, quantile) combinations
  if (!is.null(input_design) && "ensemble" %in% names(settings)) {
    if (nrow(input_design) != ensemble.size) {
      stop(
        "input_design has ", nrow(input_design), " rows, but ensemble.size is ",
        ensemble.size, ".The design matrix must have exactly one row per run."
      )
    }
  }
                              
  ## Skip database connection if no Bety params given or write is False
  # Historical note: Conceptually it'd be cleaner to skip when all of
  # settings$database is NULL. But many scripts in the wild call
  # prepare.settings(), which insists on creating a database section to put
  # settings$database$dbfiles into.
  # Checking only for Bety parameters prevents the dbfiles entry from causing
  # undesired connection attempts. 
  if (!isTRUE(write) && is.null(settings$database$bety)) {
    PEcAn.logger::logger.info("Not writing this run to database, so database connection skipped")
    con <- NULL # Set con to NULL to avoid errors in subsequent code
  } else if (is.null(settings$database$bety)) {
    PEcAn.logger::logger.error(
      "Database is NULL but writing is enabled. Provide valid database settings in pecan.xml."
    )
    stop("Database connection required but settings$database is NULL.")
  } else {
    tryCatch(
      {
        con <- PEcAn.DB::db.open(settings$database$bety)
        on.exit(PEcAn.DB::db.close(con), add = TRUE)
      },
      error = function(e) {
        PEcAn.logger::logger.severe(
          "Connection requested, but failed to open with the following error: ",
          conditionMessage(e)
        )
      }
    )
  }

  ## Which posterior to use?
  for (i in seq_along(settings$pfts)) {
    ## if posterior.files is specified us that
    if (is.na(posterior.files[i])) {
      ## otherwise, check to see if posteriorid exists
      if (!is.null(settings$pfts[[i]]$posteriorid)) {
        # TODO: sometimes `files` is a 0x0 tibble and other operations with it fail.
        files <- PEcAn.DB::dbfile.check("Posterior",
          settings$pfts[[i]]$posteriorid,
          con, settings$host$name,
          return.all = TRUE
        )
        pid <- grep("post.distns.*Rdata", files$file_name) ## is there a posterior file?
        if (length(pid) == 0) {
          pid <- grep("prior.distns.Rdata", files$file_name) ## is there a prior file?
        }
        if (length(pid) > 0) {
          posterior.files[i] <- file.path(files$file_path[pid], files$file_name[pid])
        } ## otherwise leave posteriors as NA
      }
      ## otherwise leave NA and get.parameter.samples will look for local
    } else {
      ## does posterior.files point to a directory instead of a file?
      if (utils::file_test("-d", posterior.files[i])) {
        pfiles <- dir(posterior.files[i], pattern = "post.distns.*Rdata", full.names = TRUE)
        if (length(pfiles) > 1) {
          pid <- grep("post.distns.Rdata", pfiles)
          if (length(pid > 0)) {
            pfiles <- pfiles[grep("post.distns.Rdata", pfiles)]
          } else {
            PEcAn.logger::logger.error(
              "run.write.configs: could not uniquely identify posterior files within",
              posterior.files[i]
            )
          }
          posterior.files[i] <- pfiles
        }
      }
      ## also, double check PFT outdir exists
      if (is.null(settings$pfts[[i]]$outdir) || is.na(settings$pfts[[i]]$outdir)) {
        ## no outdir
        settings$pfts[[i]]$outdir <- file.path(settings$outdir, "pfts", settings$pfts[[i]]$name)
      }
    } ## end else
  } ## end for loop over pfts

  ## Sample parameters
  model <- settings$model$type
  scipen <- getOption("scipen")
  options(scipen = 12)

  samples.file <- file.path(settings$outdir, "samples.Rdata")
  if (file.exists(samples.file)) {
    existing_data <- new.env()
    load(samples.file, envir = existing_data) ## loads ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples
    trait.samples <- existing_data$trait.samples
    sa.samples <- existing_data$sa.samples
    
    # build ensemble.samples only for ensemble runs
    # SA runs use sa.samples directly (quantile-based), not ensemble.samples
    if ("ensemble" %in% names(settings) && 
        !is.null(input_design) && 
        "param" %in% colnames(input_design)) {
      trait_sample_indices <- input_design[["param"]]
      ensemble.samples <- list()
      for (pft in names(trait.samples)) {
        pft_traits <- trait.samples[[pft]]
        ensemble.samples[[pft]] <- as.data.frame(
          lapply(
            names(pft_traits),
            function(trait) pft_traits[[trait]][trait_sample_indices]
          )
        )
        names(ensemble.samples[[pft]]) <- names(pft_traits)
      }
    } else {
      # use pre-generated samples
      ensemble.samples <- existing_data$ensemble.samples
    }
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required by the run.write.configs function")
  }

  ## remove previous runs.txt
  if (overwrite && file.exists(file.path(settings$rundir, "runs.txt"))) {
    PEcAn.logger::logger.warn("Existing runs.txt file will be removed.")
    unlink(file.path(settings$rundir, "runs.txt"))
  }

  PEcAn.utils::load.modelpkg(model)

  ## Check for model-specific write configs

  my.write.config <- paste0("write.config.", model)
  if (!exists(my.write.config)) {
    PEcAn.logger::logger.error(
      my.write.config,
      "does not exist, please make sure that the model package contains a function called",
      my.write.config
    )
  }

  ## Prepare for model output.  Clean up any old config files (if exists)
  # TODO: shouldn't this check if the files exist before removing them?
  my.remove.config <- paste0("remove.config.", model)
  if (exists(my.remove.config)) {
    do.call(my.remove.config, args = list(settings$rundir, settings))
  }

  # TODO RK : need to write to runs_inputs table

  # Save names
  pft.names <- names(trait.samples)
  trait.names <- lapply(trait.samples, names)

  # Initialize the Manifest Dataframe
  run_manifest_df <- data.frame()

  ### NEED TO IMPLEMENT: Load Environmental Priors and Posteriors

  ### Sensitivity Analysis
  if ("sensitivity.analysis" %in% names(settings)) {
    ### Write out SA config files
    PEcAn.logger::logger.info("\n ----- Writing model config files for sensitivity run ----")
    sa.runs <- PEcAn.uncertainty::write.sa.configs(
      defaults = settings$pfts,
      quantile.samples = sa.samples,
      settings = settings,
      model = model,
      input_design = input_design,
      write.to.db = write
    )

    # collect manifest data
    if ("manifest" %in% names(sa.runs)) {
      run_manifest_df <- rbind(run_manifest_df, sa.runs$manifest)
    }

    # Store output in settings and output variables
    sa.run.ids <- sa.runs$runs
    settings$sensitivity.analysis$ensemble.id <- sa.ensemble.id <- sa.runs$ensemble.id

    # Save sensitivity analysis info
    fname <- PEcAn.uncertainty::sensitivity.filename(settings, "sensitivity.samples", "Rdata",
      all.var.yr = TRUE, pft = NULL
    )
    save(sa.run.ids, sa.ensemble.id, sa.samples, pft.names, trait.names, file = fname)
  } ### End of SA

  ### Write ENSEMBLE
  if ("ensemble" %in% names(settings)) {
    ens.runs <- PEcAn.uncertainty::write.ensemble.configs(
      defaults = settings$pfts,
      ensemble.size = ensemble.size,
      ensemble.samples = ensemble.samples,
      settings = settings,
      model = model,
      input_design = input_design,
      write.to.db = write
    )

    # collect manifest data
    if ("manifest" %in% names(ens.runs)) {
      run_manifest_df <- rbind(run_manifest_df, ens.runs$manifest)
    }

    # Store output in settings and output variables
    ens.run.ids <- ens.runs$runs
    settings$ensemble$ensemble.id <- ens.ensemble.id <- ens.runs$ensemble.id
    ens.samples <- ensemble.samples # rename just for consistency

    # Save ensemble analysis info
    fname <- PEcAn.uncertainty::ensemble.filename(settings, "ensemble.samples", "Rdata", all.var.yr = TRUE)
    save(ens.run.ids, ens.ensemble.id, ens.samples, pft.names, trait.names, file = fname)
  } else {
    PEcAn.logger::logger.info("not writing config files for ensemble, settings are NULL")
  } ### End of Ensemble

  PEcAn.logger::logger.info("###### Finished writing model run config files #####")
  PEcAn.logger::logger.info("config files samples in ", file.path(settings$outdir, "run"))

  # write runs manifest
  manifest.file <- file.path(settings$outdir, "runs_manifest.csv")

  # always write manifest (even if empty) so downstream knows workflow completed
  utils::write.table(run_manifest_df,
                     file = manifest.file,
                     sep = ",",
                     row.names = FALSE,
                     col.names = overwrite || !file.exists(manifest.file),
                     append = !overwrite)
  
  PEcAn.logger::logger.info("Run manifest written to ", manifest.file)

  options(scipen = scipen)
  return(invisible(settings))
}
