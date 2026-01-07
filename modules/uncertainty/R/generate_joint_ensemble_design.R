#' Generate joint ensemble design for parameter sampling
#' Creates a joint ensemble design that maintains parameter correlations across
#' all sites in a multi-site run. This function generates sample indices that
#' are shared across sites to ensure consistent parameter sampling.
#'
#' @param settings A PEcAn settings object containing ensemble configuration
#' @param ensemble_size Integer specifying the number of ensemble members
#' Since the `input_design` will only be generated once for the entire model run,
#' the only situation, where we might want to recycle the existing `ensemble_samples`,
#' is when we split and submit the larger SDA runs (e.g., 8,000 sites) into 
#' smaller SDA experiments (e.g., 100 sites per job), where we want to keep using 
#' the same parameters rather than creating new parameters for each job.
#' @param sobol for activating sobol
#' @return  A list containing ensemble samples and indices
#'   If `sobol = TRUE`, the list will be a `sensitivity::soboljansen()` 
#'   result and will contain the components documented therein.
#' @export

generate_joint_ensemble_design <- function(settings,
                                           ensemble_size,
                                           sobol = FALSE) {
  if (sobol) {
    ensemble_size <- as.numeric(ensemble_size) * 2
  }
  ens.sample.method <- settings$ensemble$samplingspace$parameters$method
  design_list <- list()
  sampled_inputs <- list()
  posterior.files <- settings$pfts %>%
    purrr::map_chr("posterior.files", .default = NA_character_)
  samp <- settings$ensemble$samplingspace
  parents <- lapply(samp, "[[", "parent")
  order <- names(samp)[
    lapply(parents, function(tr) which(names(samp) %in% tr)) %>%
      unlist()
  ]
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]

  # loop over inputs.
  for (i in seq_along(samp.ordered)) {
    input_tag <- names(samp.ordered)[i]
    parent_name <- samp.ordered[[i]]$parent

    parent_ids <- if (!is.null(parent_name)) {
      sampled_inputs[[parent_name]]
    } else {
      NULL
    }

    input_result <- PEcAn.uncertainty::input.ens.gen(
      settings = settings,
      ensemble_size = ensemble_size,
      input = input_tag,
      method = samp.ordered[[i]]$method,
      parent_ids = parent_ids
    )

    sampled_inputs[[input_tag]] <- input_result$ids
    design_list[[input_tag]] <- input_result$ids
  }
  # Sample parameters if we don't have it.
  if (!file.exists(file.path(settings$outdir, "samples.Rdata"))) {
    PEcAn.uncertainty::get.parameter.samples(
      settings,
      ensemble.size = ensemble_size,
      posterior.files,
      ens.sample.method)
  }
  # Here we assumed the length of parameters is identical to the ensemble size.
  # TODO: detect if they are identical. If not, we will need to resample the 
  # parameters with replacement.
  design_list[["param"]] <- seq_len(ensemble_size)
  design_matrix <- data.frame(design_list)

  if (sobol) {
    half <- floor(ensemble_size / 2)
    X1 <- design_matrix[1:half, ]
    X2 <- design_matrix[(half + 1):ensemble_size, ]
    sobol_obj <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2)
    return(sobol_obj)
  }
  # This ensures that regardless of whether the sobol or non-sobol version is called 
  # that the output is a list that includes the design as X. In the sobol version the 
  # list includes additional info beyond just X that's required by the function that 
  # does the sobol index calculations, but not required to do the runs themselves.
  return(list(X = design_matrix))
}