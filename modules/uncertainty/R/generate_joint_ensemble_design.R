#' Generate joint ensemble design for parameter sampling
#' Creates a joint ensemble design that maintains parameter correlations across
#' all sites in a multi-site run. This function generates sample indices that
#' are shared across sites to ensure consistent parameter sampling.
#'
#' @details
#' Note on internal dependencies
#'
#' If samples.Rdata doesn't exist we call get.parameter.samples(), which loads
#' parameter distributions.
#'
#' In practice it:
#' - uses pft$posterior.files directly when it is defined (an Rdata file with
#'   post.distns or prior.distns),
#' - otherwise figures out an output directory from pft$outdir or, if needed,
#'   via pft$posteriorid in the database,
#' - then looks in that directory for post.distns.Rdata, falling back to
#'   prior.distns.Rdata,
#' - and, for MCMC posteriors, looks up trait.mcmc*.Rdata linked to the same
#'   posteriorid or a trait.mcmc.Rdata file in that directory.
#'
#' Difference from generate_OAT_SA_design: This function samples inputs
#' randomly or quasi-randomly, while generate_OAT_SA_design holds all
#' non-parameter inputs constant to isolate parameter effects.
#'
#' @param settings PEcAn settings object. This function directly uses:
#'   \itemize{
#'     \item \code{settings$outdir} - Output directory path for samples.Rdata
#'     \item \code{settings$pfts} - List of PFTs (extracts \code{posterior.files})
#'     \item \code{settings$ensemble$samplingspace} - Input sampling configuration
#'     \item \code{settings$run$inputs} - Input paths for each input type
#'   }
#'   When samples.Rdata doesn't exist, settings is passed to
#'   \code{\link{get.parameter.samples}} which additionally requires:
#'   \itemize{
#'     \item \code{settings$ensemble} - Ensemble configuration
#'     \item \code{settings$database$bety} - Database connection (optional)
#'     \item \code{settings$host$name} - Host name for dbfile.check (optional)
#'   }
#' @param ensemble_size Integer specifying the number of ensemble members.
#'   The input_design is generated once for the entire model run. You might
#'   want to recycle existing ensemble_samples when splitting larger runs
#'   into smaller jobs while keeping the same parameters.
#' @param sobol Logical. If TRUE, returns a \code{sensitivity::soboljansen}
#'   object for Sobol sensitivity analysis.
#'
#' @return A list containing ensemble samples and indices.
#'   If \code{sobol = FALSE}, returns \code{list(X = design_matrix)}.
#'   If \code{sobol = TRUE}, returns a \code{sensitivity::soboljansen()}
#'   result object with the design matrix in \code{$X} plus additional
#'   components for Sobol index calculations.
#'
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