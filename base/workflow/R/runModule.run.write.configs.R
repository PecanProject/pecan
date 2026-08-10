#' Generate model-specific run configuration files for one or more PEcAn runs
#'
#' This function serves as the orchestration layer between PEcAn workflows and
#' the config-writing machinery. It generates appropriate input designs
#' (ensemble and/or SA) if not provided. For MultiSettings, it generates designs once
#' from the first site then shares across all sites for consistent sampling. Finally,
#' it delegates to \code{\link{run.write.configs}} for actual config generation.
#' The input design determines how parameter samples and input files (met, soil,
#' etc.) are coordinated across runs. Ensemble designs typically use random or
#' quasi-random sampling, while SA designs hold non-parameter inputs constant
#' (OAT methodology).
#'
#' @param settings a PEcAn Settings or MultiSettings object
#' @param overwrite logical: Replace config files if they already exist?
#' @param input_design Optional. The parameter/input design for the runs,
#'   normally the full result of \code{generate_joint_ensemble_design()}: a list
#'   with \code{design_matrix} (a data.frame whose \code{param} column selects rows of
#'   \code{trait.samples}/\code{ensemble.samples}, plus optional columns named
#'   for \code{settings$run$inputs} tags such as \code{met} or \code{soil}) and
#'   \code{samples} (the parameter bundle those indices point into). Can be:
#'   \itemize{
#'     \item The \code{list(design_matrix, samples)} returned by
#'           \code{generate_joint_ensemble_design()}
#'     \item \code{NULL} to generate the design and samples internally from
#'           \code{settings}
#'   }
#'   A bare design data.frame is not accepted: its \code{param} indices are only
#'   meaningful together with the samples they index into, so the design and its
#'   samples must travel together.
#' @return A modified settings object, invisibly
#'
#'
#' @importFrom dplyr %>%
#' @importFrom rlang %||%
#' @export


runModule.run.write.configs <- function(settings,
                                        overwrite = TRUE,
                                        input_design = NULL) {

  if (PEcAn.settings::is.MultiSettings(settings)) {
    if (overwrite && file.exists(file.path(settings$rundir, "runs.txt"))) {
      PEcAn.logger::logger.warn("Existing runs.txt file will be removed.")
      unlink(file.path(settings$rundir, "runs.txt"))
    }

    # prepare designs once for all sites (consistent sampling)
    designs <- .prepare_input_designs(settings[1], input_design)

    return(PEcAn.settings::papply(settings,
                                  runModule.run.write.configs,
                                  overwrite = FALSE,
                                  input_design = designs))

  } else if (PEcAn.settings::is.Settings(settings)) {
    if (is.null(settings$ensemble$samplingspace$parameters$method)) {
      settings$ensemble$samplingspace$parameters$method <- "uniform"
    }

    # prepare designs (may already be normalized from MultiSettings)
    designs <- .prepare_input_designs(settings, input_design)

    # check to see if there are posterior.files tags under pft
    posterior.files <- settings$pfts %>%
      purrr::map_chr("posterior.files", .default = NA_character_)

    # track overwrite state: first call uses overwrite param, subsequent appends
    current_overwrite <- overwrite

    # start with original settings for final merge
    settings_final <- settings

    # ---------------- SENSITIVITY ANALYSIS CALL ----------------------
    if ("sensitivity.analysis" %in% names(settings) && !is.null(designs$sensitivity)) {

      PEcAn.logger::logger.info("Writing configs for Sensitivity Analysis...")

      # create settings with ONLY sensitivity.analysis (no ensemble)
      settings_sa <- settings
      settings_sa$ensemble <- NULL

      settings_sa_out <- PEcAn.workflow::run.write.configs(
        settings = settings_sa,
        ensemble.size = 1,  # SA doesn't use ensemble.size, but required param
        write = isTRUE(settings$database$bety$write),
        posterior.files = posterior.files,
        overwrite = current_overwrite,
        input_design = designs$sensitivity,
        samples = designs$samples
      )

      # capture SA ensemble.id
      settings_final$sensitivity.analysis <- settings_sa_out$sensitivity.analysis
      # also capture any pft$outdir modifications
      settings_final$pfts <- settings_sa_out$pfts

      # subsequent calls should append, not overwrite
      current_overwrite <- FALSE
    }

    # ------------------- ENSEMBLE CALL ----------------------
    if ("ensemble" %in% names(settings) && !is.null(designs$ensemble)) {

      PEcAn.logger::logger.info("Writing configs for Ensemble...")

      # create settings with ONLY ensemble (no sensitivity.analysis)
      settings_ens <- settings
      settings_ens$sensitivity.analysis <- NULL

      # determine ensemble size from design
      ensemble_size <- if (!is.null(designs$ensemble)) {
        nrow(designs$ensemble)
      } else {
        settings$ensemble$size %||% 1
      }

      settings_ens_out <- PEcAn.workflow::run.write.configs(
        settings = settings_ens,
        ensemble.size = ensemble_size,
        write = isTRUE(settings$database$bety$write),
        posterior.files = posterior.files,
        overwrite = current_overwrite,
        input_design = designs$ensemble,
        samples = designs$samples
      )

      # capture ensemble.id
      settings_final$ensemble <- settings_ens_out$ensemble
      # capture pft$outdir if SA didn't already
      if (!"sensitivity.analysis" %in% names(settings)) {
        settings_final$pfts <- settings_ens_out$pfts
      }
    }

    return(invisible(settings_final))

  } else {
    stop("runModule.run.write.configs only works with Settings or MultiSettings")
  }
}


#' Prepare input designs for ensemble and sensitivity analysis
#'
#' Normalizes and generates input design matrices. This helper ensures
#' consistent handling of the various input_design formats and
#' auto-generates designs when needed.
#'
#' @param settings A single PEcAn settings object
#' @param input_design Input design specification (see \code{runModule.run.write.configs})
#' @return A list with \code{ensemble} and \code{sensitivity} entries (each a
#'   data.frame or NULL) and a \code{samples} entry holding the parameter bundle
#'   used for the run. When the run has an ensemble or sensitivity analysis the
#'   bundle is always resolved (reused from a supplied design, or sampled here)
#'   and written to \code{samples.Rdata} for the downstream analysis steps.
#'
#' @details
#' Input handling rules:
#' \itemize{
#'   \item If \code{input_design} is already a list with
#'         \code{ensemble}/\code{sensitivity} keys (e.g. threaded from a
#'         MultiSettings parent), return as-is.
#'   \item If \code{input_design} is the \code{list(design_matrix, samples)}
#'         returned by a design generator, use that design and \code{samples}
#'         as the bundle (no resampling). \code{X} is accepted as the older
#'         name for the design. The design goes to whichever run the settings
#'         describe: a settings object carrying only a sensitivity analysis
#'         takes it as the SA design, otherwise it is the ensemble design.
#'         Running both means two calls, one settings object each.
#'   \item If \code{input_design} is a bare data.frame (a design without its
#'         samples), raise an error: the design's \code{param} indices only match
#'         the samples they were drawn with.
#'   \item If \code{NULL}, sample the bundle here and generate designs via
#'         \code{generate_joint_ensemble_design} and/or
#'         \code{generate_OAT_SA_design} as the settings require.
#' }
#'
#' @keywords internal

.prepare_input_designs <- function(settings, input_design) {

  # Already-normalized designs (e.g. threaded from a MultiSettings parent):
  # pass straight through. samples.Rdata was already written by the top-level
  # call that produced this list, and its samples travel on `$samples`.
  if (is.list(input_design) && !is.data.frame(input_design) &&
      any(c("ensemble", "sensitivity") %in% names(input_design))) {
    return(input_design)
  }

  designs <- list(ensemble = NULL, sensitivity = NULL, samples = NULL)
  supplied_samples <- NULL
  need_ensemble <- "ensemble" %in% names(settings)
  need_sa       <- "sensitivity.analysis" %in% names(settings)
  ensemble_size <- settings$ensemble$size %||% 1

  # Interpret a caller-supplied design. A design's `param` column only indexes
  # into the samples it was drawn with, so a design must arrive together with
  # those samples; otherwise it would be silently paired with a fresh, mismatched
  # resample. We therefore accept the full generate_joint_ensemble_design()
  # result (a list with design_matrix and samples) and reject a bare design.
  if (!is.null(input_design)) {
    # Generators return the design as `design_matrix`. `X` is the older name for
    # the same matrix, kept so existing callers keep working, and is what
    # sensitivity sets on a sobol object.
    supplied_design <- if (is.list(input_design) && !is.data.frame(input_design)) {
      input_design[["design_matrix"]] %||% input_design[["X"]]
    } else {
      NULL
    }

    if (!is.null(supplied_design) && !is.null(input_design$samples)) {
      # Route the design to the run these settings describe. Running an ensemble
      # and a sensitivity analysis means two calls, one settings object each, so
      # a settings object carrying only a sensitivity analysis means the design
      # that came with it is the SA design.
      if (need_sa && !need_ensemble) {
        designs$sensitivity <- supplied_design
      } else {
        designs$ensemble <- supplied_design
      }
      supplied_samples <- input_design$samples
    } else if (is.data.frame(input_design)) {
      PEcAn.logger::logger.severe(
        "input_design was supplied without its parameter samples.",
        "Pass the full generate_joint_ensemble_design() result",
        "(a list with `design_matrix` and `samples`) so the design's `param`",
        "indices match the samples, or leave input_design = NULL to generate",
        "both together."
      )
    } else {
      PEcAn.logger::logger.severe(
        "Unrecognized input_design format. Expected NULL or the",
        "list(design_matrix, samples) returned by generate_joint_ensemble_design()."
      )
    }
  }

  # Resolve the parameter bundle once for the whole run. Reuse the supplied
  # samples when the caller provided a design; otherwise sample now. This is
  # independent of where the design came from, which is exactly why it must also
  # run when a design was supplied.
  if (need_ensemble || need_sa) {
    samples <- supplied_samples
    if (is.null(samples)) {
      posterior.files <- rep(NA, length(settings$pfts))
      loaded <- PEcAn.uncertainty::load_pft_posteriors(settings, posterior.files)
      samples <- PEcAn.uncertainty::get_parameter_samples(
        pft_names         = loaded$pft_names,
        prior_distns_list = loaded$prior_distns_list,
        trait_mcmc_list   = loaded$trait_mcmc_list,
        ensemble.size     = ensemble_size,
        ens.sample.method = settings$ensemble$samplingspace$parameters$method %||% "uniform",
        sa_quantiles      = settings$sensitivity.analysis$quantiles,
        do_ensemble       = need_ensemble,
        independent       = loaded$independent
      )
    }

    # Persist samples.Rdata for the downstream analysis steps that still read it
    # (run.sensitivity.analysis, run.ensemble.analysis, get.results, SDA),
    # whether the design was supplied or generated below.
    ensemble.samples <- samples$ensemble.samples
    trait.samples    <- samples$trait.samples
    sa.samples       <- samples$sa.samples
    runs.samples     <- samples$runs.samples
    env.samples      <- samples$env.samples
    save(ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples,
         file = file.path(settings$outdir, "samples.Rdata"))

    designs$samples <- samples
  }

  # Deprecation: internal design generation is going away. Passing the design
  # explicitly will become the required path, one call per run.
  if (is.null(designs$ensemble) && need_ensemble) {
    PEcAn.logger::logger.warn(
      "Generating the ensemble design internally is deprecated and will be",
      "removed. Pass input_design explicitly as the list(design_matrix,",
      "samples) returned by generate_joint_ensemble_design()."
    )
  }
  if (is.null(designs$sensitivity) && need_sa) {
    PEcAn.logger::logger.warn(
      "Generating the sensitivity analysis design internally is deprecated and",
      "will be removed. Generate it with generate_OAT_SA_design() and pass it",
      "as input_design, in its own call with a settings object that has no",
      "ensemble in it."
    )
  }

  # Generate the ensemble design only when the caller did not supply one,
  # handing over the resolved samples so the generator does not resample.
  if (is.null(designs$ensemble) && need_ensemble) {
    design_result <- PEcAn.uncertainty::generate_joint_ensemble_design(
      settings      = settings,
      ensemble_size = ensemble_size,
      samples       = designs$samples
    )
    designs$ensemble <- design_result$design_matrix %||% design_result$X
  }

  # Generate the SA design if needed, threading the SA samples so the generator
  # uses them directly instead of re-reading samples.Rdata via the deprecated path.
  if (is.null(designs$sensitivity) && need_sa) {
    design_result <- PEcAn.uncertainty::generate_OAT_SA_design(
      settings,
      samples = designs$samples
    )
    designs$sensitivity <- design_result$design_matrix %||% design_result$X
  }

  return(designs)
}