#' Reads output of sensitivity analysis runs
#'
#'
#' @return dataframe with one col per quantile analysed and one row per trait,
#'  each cell is a list of AGB over time
#' @param traits model parameters included in the sensitivity analysis
#' @param quantiles quantiles selected for sensitivity analysis
#' @param pecandir specifies where pecan writes its configuration files
#' @param outdir directory with model output to use in sensitivity analysis
#' @param pft.name name of PFT used in sensitivity analysis (Optional)
#' @param start.year first year to include in sensitivity analysis
#' @param end.year last year to include in sensitivity analysis
#' @param variable variables to be read from model output
#' @param per.pft flag to determine whether we want SA on pft-specific variables
#' @param sa.run.ids list of run ids to read.
#'   If NULL, will look in `pecandir` for a file named `samples.Rdata`
#'   and read from that
#' @export
#' @author Ryan Kelly, David LeBauer, Rob Kooper, Mike Dietze, Istem Fer, Akash B V
read.sa.output <- function(traits, quantiles, pecandir, outdir, pft.name = "",
                           start.year, end.year, variable, sa.run.ids = NULL,
                           per.pft = FALSE) {

  # Load Manifest
  manifest_file <- file.path(pecandir, "runs_manifest.csv")
  if (!file.exists(manifest_file)) {
    PEcAn.logger::logger.severe("runs_manifest.csv not found in ", pecandir)
  }
  manifest <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)

  sa.output <- matrix(nrow = length(quantiles),
                      ncol = length(traits),
                      dimnames = list(quantiles, traits))

  expr <- variable$expression
  variables <- variable$variables

  for (trait in traits) {
    for (quantile in quantiles) {
      # We look for the row that matches the current pft, trait, and quantile.
      subset_df <- manifest[
        manifest$type == "Sensitivity" & 
        manifest$pft_name == pft.name &
        manifest$trait == trait &
        as.character(manifest$quantile) == as.character(quantile), 
      ]

      if (nrow(subset_df) == 1) {
         run.id <- subset_df$run_id
      } else if (nrow(subset_df) > 1) {
         PEcAn.logger::logger.warn("Multiple runs found for", trait, quantile, "- using the last one.")
         run.id <- utils::tail(subset_df$run_id, 1)
      } else {
         PEcAn.logger::logger.warn("No run found in manifest for", trait, quantile)
         next # Skip this quantile
      }

      if (is.null(run.id) || is.na(run.id)) {
         PEcAn.logger::logger.warn("Run ID invalid or missing for", trait, quantile)
         next
      }

      pass_pft <- if (isTRUE(per.pft)) pft.name else NULL

      # TODO: If adding time-based filtering, consider dataframe = TRUE
      # See benchmark module for usage example
      
      # Pass ALL variables at once to avoid repeated file opening. And call read.output once
      out.tmp <- PEcAn.utils::read.output(
        runid = run.id,
        outdir = file.path(outdir, run.id),
        start.year = start.year, end.year = end.year,
        variables = variables,
        pft.name = pass_pft
      )
      
      # Assign loaded variables to local environment for expression evaluation
      for (var in names(out.tmp)) {
        assign(var, out.tmp[[var]])
      }

      # derivation & aggregation
      out <- eval(parse(text = expr))

      sa.output[quantile, trait] <- mean(out, na.rm = TRUE)

    } ## end loop over quantiles
    PEcAn.logger::logger.info("reading sensitivity analysis output for model run at ", quantiles, "quantiles of trait", trait)
  } ## end loop over traits
  return(as.data.frame(sa.output))
} # read.sa.output



#' Write sensitivity analysis config files
#'
#' Writes config files for use in sensitivity analysis.
#'
#' @param defaults named list with default parameter values
#' @param quantile.samples list of lists supplied by \link[PEcAn.utils]{get.sa.samples}
#' @param settings list of settings
#' @param model name of model to be run
#' @param clean logical: Delete any existing contents of the directory specified
#'   by \code{settings$rundir} before writing to it?
#' @param write.to.db logical: Record this run to BETY? If TRUE, uses connection
#'   settings specified in \code{settings$database}
#' @param input_design data.frame coordinating input files across runs
#'
#' @return list, containing $runs = data frame of runids,
#'  and $ensemble.id = the ensemble ID for these runs.
#'  Also writes sensitivity analysis configuration files as a side effect
#' @export
#' @author David LeBauer, Carl Davidson, Akash B V
write.sa.configs <- function(defaults, quantile.samples, settings, model,
                             clean = FALSE, write.to.db = TRUE, input_design = NULL) {
  scipen <- getOption("scipen")
  options(scipen = 12)
  my.write.config <- paste("write.config.", model, sep = "")

  if (write.to.db) {
    con <- try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
    if (inherits(con, "try-error")) {
      con <- NULL
    } else {
      on.exit(PEcAn.DB::db.close(con), add = TRUE)
    }
  } else {
    con <- NULL
  }

  # Get the workflow id
  if ("workflow" %in% names(settings)) {
    workflow.id <- settings$workflow$id
  } else {
    workflow.id <- -1
  }

  # find all inputs that have an id
  inputs <- names(settings$run$inputs)
  inputs <- inputs[grepl(".id$", inputs)]

  runs <- data.frame()
  manifest_df <- data.frame(
    run_id = character(),
    site_id = character(),
    pft_name = character(),
    trait = character(),
    quantile = character(),
    type = character(),
    stringsAsFactors = FALSE
  )

  # Reading the site.pft specific tags from xml
  site.pfts.vec <- as.character(unlist(settings$run$site$site.pft))

  if (!is.null(site.pfts.vec)) {
    # find the name of pfts defined in the body of pecan.xml
    defined.pfts <- as.character(unlist(purrr::map(settings$pfts, "name")))
    # subset ensemble samples based on the pfts that are specified in the site
    # and they are also sampled from.
    if (length(which(site.pfts.vec %in% defined.pfts)) > 0) {
      quantile.samples <- quantile.samples [site.pfts.vec[which(site.pfts.vec %in% defined.pfts)]]
    }
    # warn if there is a pft specified in the site but it's not defined in the pecan xml.
    if (length(which(!(site.pfts.vec %in% defined.pfts))) > 0) {
      PEcAn.logger::logger.warn(
        "The following pfts are specified for the siteid ",
        settings$run$site$id,
        " but they are not defined as a pft in pecan.xml:",
        site.pfts.vec[which(!(site.pfts.vec %in% defined.pfts))]
      )
    }
  }


  ## write median run
  MEDIAN <- "50"
  median.samples <- list()
  for (i in seq_along(quantile.samples)) {
    median.samples[[i]] <- quantile.samples[[i]][MEDIAN, , drop  = FALSE]
  }
  names(median.samples) <- names(quantile.samples)

  if (!is.null(con)) {
    # Note: ignores any existing run or ensemble ids in settings
    ensemble.id <- PEcAn.DB::db.query(paste0(
      "INSERT INTO ensembles (runtype, workflow_id) ",
      "VALUES ('sensitivity analysis', ", format(workflow.id, scientific = FALSE), ") ",
      "RETURNING id"), con = con)[["id"]]

    paramlist <- paste0(
      "quantile=MEDIAN,trait=all,pft=",
      paste(lapply(settings$pfts, function(x) x[["name"]]), sep = ",")
    )
    run.id <- PEcAn.DB::db.query(paste0(
      "INSERT INTO runs ",
      "(model_id, site_id, start_time, finish_time, outdir, ensemble_id, parameter_list) ",
      "values ('",
        settings$model$id, "', '",
        settings$run$site$id, "', '",
        settings$run$start.date, "', '",
        settings$run$end.date, "', '",
        settings$run$outdir, "', ",
        ensemble.id, ", '",
        paramlist, "') ",
      "RETURNING id"), con = con)[["id"]]

    # associate posteriors with ensembles
    for (pft in defaults) {
      PEcAn.DB::db.query(
        paste0(
          "INSERT INTO posteriors_ensembles (posterior_id, ensemble_id) ",
          "values (", pft$posteriorid, ", ", ensemble.id, ")"),
        con = con
      )
    }

    # associate inputs with runs
    if (!is.null(inputs)) {
      for (x in inputs) {
        PEcAn.DB::db.query(
          paste0(
            "INSERT INTO inputs_runs (input_id, run_id) ",
            "values (", settings$run$inputs[[x]], ", ", run.id, ")"),
          con = con
        )
      }
    }
  } else {
    run.id <- PEcAn.utils::get.run.id("SA", "median", site.id = settings$run$site$id)
    # Use SA ensemble id if provided, or an arbitrary unique value if not
    # Note: Since write.sa.configs is called separately for each site,
    # a multisite run with no ID provided gives each site its own ensemble id!
    ensemble.id <- settings$sensitivity.analysis$ensemble.id %||% rlang::hash(settings)
  }
  medianrun <- run.id

  manifest_df <- rbind(manifest_df, data.frame(
    run_id = medianrun,
    site_id = settings$run$site$id,
    pft_name = "NA",
    trait = "NA",
    quantile = MEDIAN,
    type = "Sensitivity",
    stringsAsFactors = FALSE
  ))

  # create folders (cleaning up old ones if needed)
  if (clean) {
    unlink(file.path(settings$rundir, run.id))
    unlink(file.path(settings$modeloutdir, run.id))
  }
  dir.create(file.path(settings$rundir, run.id), recursive = TRUE)
  dir.create(file.path(settings$modeloutdir, run.id), recursive = TRUE)

  # Apply input design coordination for median run
  median_settings <- settings
  if (!is.null(input_design)) {
    # Coordinate inputs for median run (use first row)
    for (input_tag in colnames(input_design)) {
      if (input_tag != "param" && !is.null(median_settings$run$inputs[[input_tag]]$path)) {
        input_paths <- median_settings$run$inputs[[input_tag]]$path
        # Assume list structure (consistent with write.ensemble.configs)
        if (length(input_paths) > 1) {
          input_index <- input_design[[input_tag]][1]
          median_settings$run$inputs[[input_tag]]$path <- input_paths[[input_index]]
        }
      }
    }
  }

  median_input_info <- ""
  for (input_tag in names(median_settings$run$inputs)) {
    input_data <- median_settings$run$inputs[[input_tag]]
    # At SA stage, path is ALWAYS a resolved string (thanks to input design)
    if (!is.null(input_data) && !is.null(input_data$path)) {
      median_input_info <- paste0(median_input_info,
                                  format(input_tag, width = 12, justify = "left"),
                                  ": ",
                                  input_data$path,
                                  "\n")
    }
  }

  # write run information to disk TODO need to print list of pft names and trait
  # names
  cat("runtype     : sensitivity analysis\n",
      "workflow id : ", workflow.id, "\n",
      "ensemble id : ", ensemble.id, "\n",
      "pft name    : ALL PFT", "\n",
      "quantile    : MEDIAN\n",
      "trait       : ALL TRAIT", "\n",
      "run id      : ", run.id, "\n",
      "model       : ", model, "\n",
      "model id    : ", median_settings$model$id, "\n",
      "site        : ", median_settings$run$site$name, "\n",
      "site  id    : ", median_settings$run$site$id, "\n",
      median_input_info,
      "start date  : ", median_settings$run$start.date, "\n",
      "end date    : ", median_settings$run$end.date, "\n",
      "hostname    : ", median_settings$host$name, "\n",
      "rundir      : ", file.path(median_settings$host$rundir, run.id), "\n",
      "outdir      : ", file.path(median_settings$host$outdir, run.id), "\n",
      file = file.path(median_settings$rundir, run.id, "README.txt"),
      sep = "")


  # write configuration
  do.call(my.write.config, args = list(defaults = defaults,
                                       trait.values = median.samples,
                                       settings = median_settings,
                                       run.id = run.id))
  cat(
    run.id,
    file = file.path(median_settings$rundir, "runs.txt"),
    sep = "\n",
    append = TRUE
  )

  run_index <- 1

  ## loop over pfts
  runs <- list()
  for (pft_idx in seq_along(names(quantile.samples))) {
    pftname <- names(quantile.samples)[pft_idx]
    if (pftname == "env") {
      next
    }

    traits <- colnames(quantile.samples[[pft_idx]])
    quantiles.str <- rownames(quantile.samples[[pft_idx]])

    runs[[pftname]] <- data.frame()

    ## loop over variables
    for (trait in traits) {
      for (quantile.str in quantiles.str) {
        if (quantile.str == MEDIAN) {
          runs[[pftname]][MEDIAN, trait] <- medianrun
        } else {
          quantile <- as.numeric(quantile.str) / 100
          trait.samples <- median.samples
          trait.samples[[pft_idx]][trait] <- quantile.samples[[pft_idx]][quantile.str, trait, drop = FALSE]

          if (!is.null(con)) {
            paramlist <- paste0("quantile=", quantile.str, ",trait=", trait, ",pft=", pftname)
            insert_result <- PEcAn.DB::db.query(
              paste0(
                "INSERT INTO runs (",
                  "model_id, site_id, start_time, finish_time, outdir,",
                  " ensemble_id, parameter_list) ",
                "values ('",
                  settings$model$id, "', '",
                  settings$run$site$id, "', '",
                  settings$run$start.date, "', '",
                  settings$run$end.date, "', '",
                  settings$run$outdir, "', ",
                  ensemble.id, ", '",
                  paramlist,
                "') ",
                "RETURNING id"),
              con = con
            )
            run.id <- insert_result[["id"]]

            # associate posteriors with ensembles
            for (pft in defaults) {
              PEcAn.DB::db.query(
                paste0(
                  "INSERT INTO posteriors_ensembles (posterior_id, ensemble_id)",
                  "values (", pft$posteriorid, ", ", ensemble.id, ");"
                ),
                con = con
              )
            }

            # associate inputs with runs
            if (!is.null(inputs)) {
              for (x in inputs) {
                PEcAn.DB::db.query(
                  paste0(
                    "INSERT INTO inputs_runs (input_id, run_id) ",
                    "values (", settings$run$inputs[[x]], ", ", run.id, ");"),
                  con = con
                )
              }
            }
          } else {
            run.id <- PEcAn.utils::get.run.id(
              run.type = "SA",
              index = round(quantile, 3),
              trait = trait,
              pft.name = names(trait.samples)[pft_idx],
              site.id = settings$run$site$id
            )
          }
          runs[[pftname]][quantile.str, trait] <- run.id

          manifest_df <- rbind(manifest_df, data.frame(
            run_id = run.id,
            site_id = settings$run$site$id,
            pft_name = pftname,
            trait = trait,
            quantile = quantile.str,
            type = "Sensitivity",
            stringsAsFactors = FALSE
          ))

          # Increment run counter
          run_index <- run_index + 1

          # create folders (cleaning up old ones if needed)
          if (clean) {
            unlink(file.path(settings$rundir, run.id))
            unlink(file.path(settings$modeloutdir, run.id))
          }
          dir.create(file.path(settings$rundir, run.id), recursive = TRUE)
          dir.create(file.path(settings$modeloutdir, run.id), recursive = TRUE)

          # Apply input design coordination for SA runs
          settings_copy <- settings
          if (!is.null(input_design)) {
            for (input_tag in colnames(input_design)) {
              if (input_tag != "param" && !is.null(settings_copy$run$inputs[[input_tag]]$path)) {
                input_paths <- settings_copy$run$inputs[[input_tag]]$path
                if (length(input_paths) > 1) {
                  input_index <- input_design[[input_tag]][run_index]
                  settings_copy$run$inputs[[input_tag]]$path <- input_paths[[input_index]]
                }
              }
            }
          }

          # Build dynamic input info string for SA run README
          sa_input_info <- ""
          for (input_tag in names(settings_copy$run$inputs)) {
            input_data <- settings_copy$run$inputs[[input_tag]]
            if (!is.null(input_data) && !is.null(input_data$path)) {
              sa_input_info <- paste0(sa_input_info,
                                      format(input_tag, width = 12, justify = "left"),
                                      ": ",
                                      input_data$path,
                                      "\n")
            }
          }

          # write SA run information to disk
          cat("runtype     : sensitivity analysis\n",
              "workflow id : ", workflow.id, "\n",
              "ensemble id : ", ensemble.id, "\n",
              "pft name    : ", names(trait.samples)[pft_idx], "\n",
              "quantile    : ", quantile.str, "\n",
              "trait       : ", trait, "\n",
              "run id      : ", run.id, "\n",
              "model       : ", model, "\n",
              "model id    : ", settings_copy$model$id, "\n",
              "site        : ", settings_copy$run$site$name, "\n",
              "site  id    : ", settings_copy$run$site$id, "\n",
              sa_input_info,
              "start date  : ", settings_copy$run$start.date, "\n",
              "end date    : ", settings_copy$run$end.date, "\n",
              "hostname    : ", settings_copy$host$name, "\n",
              "rundir      : ", file.path(settings_copy$host$rundir, run.id), "\n",
              "outdir      : ", file.path(settings_copy$host$outdir, run.id), "\n",
              file = file.path(settings_copy$rundir, run.id, "README.txt"),
              sep = "")


          # write configuration
          do.call(my.write.config, args = list(defaults = defaults,
                                               trait.values = trait.samples,
                                               settings = settings_copy,
                                               run.id))
          cat(
            run.id,
            file = file.path(settings_copy$rundir, "runs.txt"),
            sep = "\n",
            append = TRUE
          )
        }
      }
    }
  }

  options(scipen = scipen)
  return(invisible(list(runs = runs, ensemble.id = ensemble.id, manifest = manifest_df)))
} # write.sa.configs
