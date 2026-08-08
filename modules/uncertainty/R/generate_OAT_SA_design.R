#' Generate One-At-a-Time (OAT) input design for sensitivity analysis
#'
#' Creates an input design matrix for sensitivity analysis where non-parameter
#' inputs (met, IC, soil, etc.) are held constant while parameters vary
#' one-at-a-time across quantiles. This differs from ensemble design, where all
#' inputs vary together.
#'
#' @details
#' ## Settings requirements
#'
#' This function directly uses:
#' \itemize{
#'   \item \code{settings$pfts} - List of PFTs (extracts \code{posterior.files})
#'   \item \code{settings$ensemble$samplingspace} - Input types to include in design
#'   \item \code{settings$sensitivity.analysis$quantiles} - SA quantiles, when
#'     sampling here rather than reusing a supplied bundle
#' }
#'
#' When \code{samples = NULL}, \code{\link{load_pft_posteriors}} additionally
#' uses \code{settings$database$bety} and \code{settings$host$name} for the
#' optional posterior lookup.
#'
#' ## OAT design logic
#' For sensitivity analysis, we must isolate the effect of each
#' parameter by holding all other inputs constant. The param column contains
#' sequential indices (1, 2, 3, ...) matching the SA run order in
#' \code{write.sa.configs}. All other columns (met, ic, soil, etc.) are set to 1,
#' meaning the first input file is always used.
#'
#' ## Where the samples come from
#' Parameter samples are drawn in memory via \code{\link{load_pft_posteriors}}
#' and \code{\link{get_parameter_samples}} (mirroring
#' \code{\link{generate_joint_ensemble_design}}), or reused when a \code{samples}
#' bundle is supplied. Nothing is read from or written to \code{samples.Rdata}.
#' The design is built from the quantile-based \code{sa.samples} in that bundle.
#'
#' @param settings PEcAn settings object. See details for required elements.
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
#' @examples
#' \dontrun{
#' # Generate the SA design, sampling parameters in memory
#' sa_design <- generate_OAT_SA_design(settings)
#'
#' # View the design matrix
#' print(sa_design$design_matrix)
#' #   param met ic soil
#' # 1     1   1  1    1   # Median run
#' # 2     2   1  1    1   # trait1 @ q=2.3%
#' # 3     3   1  1    1   # trait1 @ q=15.9%
#' # 4     4   1  1    1   # trait1 @ q=84.1%
#' # ...
#'
#' # Reuse an established set of samples instead of drawing new ones
#' sa_design <- generate_OAT_SA_design(settings, samples = samples)
#' }
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

  # calculate total number of SA runs
  # 1 median + (traits * non-median quantiles) per PFT
  MEDIAN <- "50"
  num_sa_runs <- 1 # start with median run

  for (pft_name in names(sa_samples)) {
    if (pft_name == "env") next

    pft_samples <- sa_samples[[pft_name]]
    n_traits <- ncol(pft_samples)
    quantile_names <- rownames(pft_samples)
    n_non_median <- sum(quantile_names != MEDIAN)

    # add runs for this pft: (traits) * (non-median quantiles)
    num_sa_runs <- num_sa_runs + (n_traits * n_non_median)
  }

  # get input types from samplingspace
  samp <- settings$ensemble$samplingspace
  input_types <- names(samp)
  input_types[input_types == "parameters"] <- "param"

  if (!"param" %in% input_types) {
    input_types <- c("param", input_types)
  }

  # build design matrix
  # key difference from ensemble design:
  # - ensemble: all columns get random/quasi-random indices
  # - SA (OAT): param column = sequential index, ALL other columns = 1
  #
  # the "1" means: use the FIRST (and only) input file for that type.
  # this ensures all SA runs use the SAME met, same ic, etc.

  design_list <- list()

  for (input_type in input_types) {
    if (input_type == "param") {
      # sequential indices map to SA run order
      #   1 = median run
      #   2 = first (pft, trait, quantile) combination
      #   3 = second (pft, trait, quantile) combination
      #   ...
      design_list[[input_type]] <- seq_len(num_sa_runs)
    } else {
      # all other inputs constant (always use first input file)
      design_list[[input_type]] <- rep(1L, num_sa_runs)
    }
  }

  design_matrix <- data.frame(design_list)

  return(list(design_matrix = design_matrix, X = design_matrix, samples = samples))
}