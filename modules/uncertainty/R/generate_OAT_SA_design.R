#' Generate One-At-a-Time (OAT) input design for sensitivity analysis
#'
#' Creates an input design matrix for sensitivity analysis where non-parameter
#' inputs (met, IC, soil, etc.) are held constant while parameters vary
#' one-at-a-time across quantiles. This differs from ensemble design, where all
#' inputs vary together.
#'
#' Parameter samples are drawn in memory via \code{\link{load_pft_posteriors}}
#' and \code{\link{get_parameter_samples}} (mirroring
#' \code{\link{generate_joint_ensemble_design}}), or reused when a \code{samples}
#' bundle is supplied. The design is built from the quantile-based
#' \code{sa.samples} within that bundle.
#'
#' @param settings PEcAn settings object. Uses \code{settings$pfts},
#'   \code{settings$sensitivity.analysis$quantiles} (the SA quantiles, when
#'   sampling here), and \code{settings$ensemble$samplingspace} (the input types
#'   that form the design columns).
#' @param samples Optional pre-computed parameter samples (a list containing at
#'   least \code{sa.samples}, as returned by \code{\link{get_parameter_samples}}).
#'   When supplied these are used directly; when \code{NULL} (default) they are
#'   sampled in memory.
#'
#' @return A list with \code{design_matrix}, a data.frame with one row per SA run
#'   and one column per input type (the \code{param} column holds sequential run
#'   indices, every other column is held at 1), \code{X}, the same matrix under
#'   its older name, and \code{samples}, the parameter bundle used.
#'
#' @author Akash B V, Om Kapale
#' @importFrom rlang %||%
#' @export
generate_OAT_SA_design <- function(settings, samples = NULL) {

  # Generate parameter samples in memory (or use the ones passed in), mirroring
  # generate_joint_ensemble_design. A sensitivity analysis needs the quantile-
  # based sa.samples, so we request those and skip the ensemble draw.
  if (is.null(samples)) {
    posterior.files <- settings$pfts %>%
      purrr::map_chr("posterior.files", .default = NA_character_)
    loaded <- load_pft_posteriors(settings, posterior.files)
    samples <- get_parameter_samples(
      pft_names         = loaded$pft_names,
      prior_distns_list = loaded$prior_distns_list,
      trait_mcmc_list   = loaded$trait_mcmc_list,
      ensemble.size     = settings$ensemble$size %||% 1,
      ens.sample.method = settings$ensemble$samplingspace$parameters$method %||% "uniform",
      sa_quantiles      = settings$sensitivity.analysis$quantiles,
      do_ensemble       = FALSE,
      independent       = loaded$independent
    )
  }

  sa_samples <- samples$sa.samples

  if (is.null(sa_samples) || length(sa_samples) == 0) {
    PEcAn.logger::logger.severe(
      "sa.samples are empty.",
      "Ensure sensitivity.analysis quantiles are configured in settings."
    )
  }

  # Total number of SA runs: 1 median run plus, for each PFT,
  # (n traits) * (n non-median quantiles).
  MEDIAN <- "50"
  num_sa_runs <- 1

  for (pft_name in names(sa_samples)) {
    if (pft_name == "env") next

    pft_samples <- sa_samples[[pft_name]]
    n_traits <- ncol(pft_samples)
    quantile_names <- rownames(pft_samples)
    n_non_median <- sum(quantile_names != MEDIAN)

    num_sa_runs <- num_sa_runs + (n_traits * n_non_median)
  }

  # Input types come from the sampling space; parameters map to the "param" column.
  samp <- settings$ensemble$samplingspace
  input_types <- names(samp)
  input_types[input_types == "parameters"] <- "param"

  if (!"param" %in% input_types) {
    input_types <- c("param", input_types)
  }

  # OAT design: the param column carries sequential indices matching the SA run
  # order, and every other input column is held constant at 1 (the first input
  # file), so each run isolates a single parameter.
  design_list <- list()

  for (input_type in input_types) {
    if (input_type == "param") {
      design_list[[input_type]] <- seq_len(num_sa_runs)
    } else {
      design_list[[input_type]] <- rep(1L, num_sa_runs)
    }
  }

  design_matrix <- data.frame(design_list)

  return(list(design_matrix = design_matrix, X = design_matrix, samples = samples))
}