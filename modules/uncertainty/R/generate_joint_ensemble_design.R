#' Generate joint ensemble design for parameter sampling
#' Creates a joint ensemble design that maintains parameter correlations across
#' all sites in a multi-site run. This function generates sample indices that
#' are shared across sites to ensure consistent parameter sampling.
#'
#' @details
#' Parameter samples are drawn in memory via \code{\link{load_pft_posteriors}}
#' and \code{\link{get_parameter_samples}}, or reused when a \code{samples}
#' bundle is supplied. Nothing is read from or written to \code{samples.Rdata}.
#'
#' \code{load_pft_posteriors} resolves each PFT's posterior: it uses
#' \code{pft$posterior.files} when that is set, otherwise works out a directory
#' from \code{pft$outdir} or \code{pft$posteriorid}, then reads
#' \code{post.distns.Rdata} there, falling back to \code{prior.distns.Rdata},
#' and picks up MCMC chains from \code{trait.mcmc*.Rdata} where they exist.
#'
#' Difference from generate_OAT_SA_design: This function samples inputs
#' randomly or quasi-randomly, while generate_OAT_SA_design holds all
#' non-parameter inputs constant to isolate parameter effects.
#'
#' @param settings PEcAn settings object. This function directly uses:
#'   \itemize{
#'     \item \code{settings$pfts} - List of PFTs (extracts \code{posterior.files})
#'     \item \code{settings$ensemble$samplingspace} - Input sampling configuration
#'     \item \code{settings$run$inputs} - Input paths for each input type
#'   }
#'   When \code{samples} is not supplied, \code{load_pft_posteriors} additionally
#'   uses \code{settings$database$bety} and \code{settings$host$name} for the
#'   optional posterior lookup.
#' @param ensemble_size Integer specifying the number of ensemble members.
#'   The input_design is generated once for the entire model run. You might
#'   want to recycle existing ensemble_samples when splitting larger runs
#'   into smaller jobs while keeping the same parameters.
#' @param samples Optional pre-computed parameter samples. When supplied, these
#'   are used directly instead of loading posteriors and sampling. When
#'   \code{NULL} (default), samples are generated in memory via
#'   \code{load_pft_posteriors} and \code{get_parameter_samples}.
#' @param sobol Logical. If TRUE, returns a \code{sensitivity::soboljansen}
#'   object for Sobol sensitivity analysis.
#'
#' @return If \code{sobol = FALSE}, a list with \code{design_matrix} (the design,
#'   one row per run, whose \code{param} column indexes the samples), \code{X}
#'   (the same matrix under its older name, kept so existing callers keep
#'   working), and \code{samples} (the parameter bundle those indices point
#'   into).
#'   If \code{sobol = TRUE}, a \code{sensitivity::soboljansen()} result object
#'   carrying the same \code{design_matrix} and \code{samples} entries, with
#'   \code{$X} set by \pkg{sensitivity} itself, plus the extra components its
#'   index calculations need.
#'
#' @export

generate_joint_ensemble_design <- function(settings,
                                           ensemble_size,
                                           samples = NULL,
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
  # Generate parameter samples in memory (or use the ones passed in).
  if (is.null(samples)) {
    loaded <- load_pft_posteriors(settings, posterior.files)
    samples <- get_parameter_samples(
      pft_names         = loaded$pft_names,
      prior_distns_list = loaded$prior_distns_list,
      trait_mcmc_list   = loaded$trait_mcmc_list,
      ensemble.size     = ensemble_size,
      ens.sample.method = ens.sample.method,
      sa_quantiles      = NULL,
      do_ensemble       = TRUE,
      independent       = loaded$independent
    )
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
    # Carry the design and samples on the object so a sobol design travels like
    # any other. sensitivity sets $X itself; design_matrix is the same matrix
    # under the name the non-sobol return uses. Neither addition touches the
    # components sensitivity::tell() needs.
    sobol_obj$design_matrix <- sobol_obj$X
    sobol_obj$samples <- samples
    return(sobol_obj)
  }
  # Both returns expose the design as design_matrix, with X kept as its older
  # name. The sobol return carries additional components that its index
  # calculations need but the runs themselves do not.
  return(list(design_matrix = design_matrix, X = design_matrix, samples = samples))
}
