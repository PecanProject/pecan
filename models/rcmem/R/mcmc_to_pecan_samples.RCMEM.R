#' Convert synchronized MCMC draws to PEcAn samples
#'
#' @param mcmc Long table with Parameter, Chain, Iteration, value, pft.
#' @param ensemble_size Number of draws to select.
#' @param seed Optional random seed.
#'
#' @return List containing PEcAn samples and selected draw manifest.
#' @export
mcmc_to_pecan_samples.RCMEM <- function(
    mcmc,
    ensemble_size,
    seed = NULL
) {
  
  required <- c("Parameter", "Chain", "Iteration", "value", "pft")
  
  if (!is.data.frame(mcmc) ||
      !all(required %in% names(mcmc))) {
    stop("MCMC input must contain: ",
         paste(required, collapse = ", "))
  }
  
  if (length(ensemble_size) != 1L ||
      !is.numeric(ensemble_size) ||
      !is.finite(ensemble_size) ||
      ensemble_size < 1 ||
      ensemble_size != floor(ensemble_size)) {
    stop("ensemble_size must be a positive integer.")
  }
  
  if (nrow(mcmc) == 0L ||
      anyNA(mcmc[, required]) ||
      !is.numeric(mcmc$value) ||
      any(!is.finite(mcmc$value)) ||
      any(!nzchar(as.character(mcmc$pft))) ||
      any(!nzchar(as.character(mcmc$Parameter)))) {
    stop("MCMC table contains missing or invalid values.")
  }
  
  # Check unique parameter observations
  duplicates <- mcmc |>
    dplyr::count(
      .data$pft, .data$Parameter,
      .data$Chain, .data$Iteration
    ) |>
    dplyr::filter(.data$n > 1L)
  
  if (nrow(duplicates) > 0L) {
    stop("Duplicate PFT/parameter/chain/iteration records.")
  }
  
  # Number of unique PFT-parameter combinations
  n_expected <- mcmc |>
    dplyr::distinct(.data$pft, .data$Parameter) |>
    nrow()
  
  # Only use draws with every required PFT parameter
  complete_draws <- mcmc |>
    dplyr::count(
      .data$Chain, .data$Iteration,
      name = "n_parameters"
    ) |>
    dplyr::filter(.data$n_parameters == n_expected) |>
    dplyr::select(.data$Chain, .data$Iteration) |>
    dplyr::arrange(.data$Chain, .data$Iteration)
  
  if (nrow(complete_draws) < ensemble_size) {
    stop(
      "Requested ", ensemble_size,
      " draws, but only ", nrow(complete_draws),
      " complete joint draws are available."
    )
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Select one shared set of posterior draws
  selected_draws <- complete_draws |>
    dplyr::slice_sample(n = ensemble_size) |>
    dplyr::mutate(
      ensemble_id = dplyr::row_number(),
      .before = 1
    )
  
  # Apply the selected draws to every PFT
  selected <- selected_draws |>
    dplyr::left_join(
      mcmc,
      by = c("Chain", "Iteration")
    )
  
  # Convert selected MCMC draws into PFT-specific data frames
  wide <- selected |>
    dplyr::select(ensemble_id, pft, Parameter, value) |>
    tidyr::pivot_wider(
      id_cols = c(ensemble_id, pft),
      names_from = Parameter,
      values_from = value
    ) |>
    dplyr::arrange(pft, ensemble_id)
  
  # Split by PFT
  pft_groups <- wide |>
    dplyr::group_by(pft)
  
  ensemble_samples <- pft_groups |>
    dplyr::group_split(.keep = FALSE) |>
    stats::setNames(
      dplyr::group_keys(pft_groups)$pft
    ) |>
    lapply(function(x) {
      
      # Remove ensemble index
      x <- dplyr::select(x, -ensemble_id)
      
      # Remove parameters belonging to other PFTs
      x <- x[, !vapply(x, function(z) all(is.na(z)), logical(1)),
             drop = FALSE]
      
      as.data.frame(x)
    })
  # Verify alignment
  stopifnot(
    all(vapply(
      ensemble_samples,
      nrow,
      integer(1)
    ) == ensemble_size)
  )
  
  # Convert PFT data frames into lists of parameter vectors
  trait_samples <- lapply(
    ensemble_samples,
    function(x) {
      as.list(x)
    }
  )
  
  # Assemble PEcAn-compatible sample bundle
  samples <- list(
    trait.samples = trait_samples,
    sa.samples = list(),
    ensemble.samples = ensemble_samples,
    runs.samples = list(),
    env.samples = list()
  )
  
  list(
    samples = samples,
    draw_manifest = selected_draws
  )
}
