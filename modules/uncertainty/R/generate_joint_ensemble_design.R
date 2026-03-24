#' Minimum trait sample bank size across PFTs
#'
#' Returns the smallest number of trait samples available across all PFTs
#' and traits. Used to verify that the parameter bank is large enough for
#' a Sobol or ensemble design.
#'
#' @param trait.samples named list of PFT trait sample lists, as stored
#'   in \code{samples.Rdata}.
#' @return integer, minimum bank size (0 if empty)
#' @keywords internal
#' @export
trait_sample_bank_size <- function(trait.samples) {
  if (is.null(trait.samples) || length(trait.samples) == 0) {
    return(0L)
  }

  bank_sizes <- unlist(
    purrr::map(trait.samples, function(pft_traits) {
      if (is.null(pft_traits) || length(pft_traits) == 0) {
        return(integer(0))
      }
      purrr::map_int(pft_traits, function(trait_values) {
        if (is.null(trait_values) || length(trait_values) == 0) {
          return(NA_integer_)
        }
        as.integer(length(trait_values))
      })
    }),
    use.names = FALSE
  )

  bank_sizes <- bank_sizes[!is.na(bank_sizes) & bank_sizes > 0L]
  if (length(bank_sizes) == 0) {
    return(0L)
  }

  as.integer(min(bank_sizes))
}

.sobol_parameter_bank_size <- function(samples_file) {
  if (!file.exists(samples_file)) {
    return(0L)
  }

  samples <- new.env(parent = emptyenv())
  load(samples_file, envir = samples)

  if (is.null(samples$trait.samples) || length(samples$trait.samples) == 0) {
    return(0L)
  }

  trait_sample_bank_size(samples$trait.samples)
}

.map_sobol_to_indices <- function(x, size) {
  indices <- floor(stats::qunif(x, min = 1, max = size + 1))
  as.integer(pmin(indices, size))
}

#' Generate joint ensemble design for parameter sampling
#'
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
#' When \code{sobol = TRUE}, every input listed in
#' \code{settings$ensemble$samplingspace} (other than \code{parameters})
#' becomes an independent Sobol factor. This allows variance-based
#' sensitivity analysis to attribute output variance to each source
#' (parameters, met, initial conditions, events, etc.) independently.
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
#'   When \code{sobol = TRUE}, this is the Sobol base sample size \code{N}, not
#'   the expanded number of model runs (which will be \code{N * (k + 2)} for
#'   \code{k} independent factors).
#'   The input_design is generated once for the entire model run. You might
#'   want to recycle existing ensemble_samples when splitting larger runs
#'   into smaller jobs while keeping the same parameters.
#' @param sobol Logical, generate a variance-based Sobol design using
#'   \code{sensobol}.
#'
#' @return A list with component \code{X}, a data frame design matrix
#'   describing PEcAn parameter and sampled input indices. If \code{sobol = TRUE},
#'   the list also includes the metadata needed by
#'   \code{\link{compute_sobol_indices}}: \code{N}, \code{params},
#'   \code{backend}, \code{matrices}, \code{first}, \code{total}, and
#'   \code{factor_metadata}.
#'
#' @export

generate_joint_ensemble_design <- function(settings,
                                           ensemble_size,
                                           sobol = FALSE) {
  ens.sample.method <- settings$ensemble$samplingspace$parameters$method
  design_list <- list()
  sampled_inputs <- list()
  posterior.files <- settings$pfts |>
    purrr::map_chr("posterior.files", .default = NA_character_)
  samp <- settings$ensemble$samplingspace

  if (sobol) {
    # every input in samplingspace (except parameters) is an independent factor
    input_names <- setdiff(names(samp), "parameters")
    sobol_factors <- c("param", input_names)

    total_runs <- as.integer(ensemble_size) * (length(sobol_factors) + 2L)
    samples_file <- file.path(settings$outdir, "samples.Rdata")
    if (.sobol_parameter_bank_size(samples_file) < total_runs) {
      PEcAn.uncertainty::get.parameter.samples(
        settings = settings,
        ensemble.size = total_runs,
        posterior.files = posterior.files,
        ens.sample.method = ens.sample.method
      )
    }

    sobol_design <- sensobol::sobol_matrices(
      matrices = c("A", "B", "AB"),
      N = as.integer(ensemble_size),
      params = sobol_factors,
      order = "first",
      type = "QRN"
    )
    sobol_design <- as.data.frame(sobol_design)

    # map param column to trait bank indices
    sobol_indices <- list()
    sobol_indices[["param"]] <- .map_sobol_to_indices(
      sobol_design[["param"]],
      total_runs
    )
    sampled_inputs[["parameters"]] <- list(ids = sobol_indices[["param"]])

    # map each input to its available paths
    for (input_tag in input_names) {
      input_paths <- settings$run$inputs[[tolower(input_tag)]]$path
      if (is.null(input_paths) || length(input_paths) == 0) {
        PEcAn.logger::logger.error(
          "Input ", sQuote(input_tag), " has no paths specified"
        )
      }
      sobol_indices[[input_tag]] <- .map_sobol_to_indices(
        sobol_design[[input_tag]],
        length(input_paths)
      )
      sampled_inputs[[input_tag]] <- list(ids = sobol_indices[[input_tag]])
      design_list[[input_tag]] <- sobol_indices[[input_tag]]
    }

    design_list[["param"]] <- sobol_indices[["param"]]
    design_matrix <- tibble::as_tibble(design_list)

    factor_metadata <- tibble::tibble(
      factor = sobol_factors,
      source_type = sobol_factors,
      source_tag = ifelse(
        sobol_factors == "param", NA_character_, sobol_factors
      )
    )

    return(list(
      X = design_matrix,
      N = as.integer(ensemble_size),
      params = sobol_factors,
      backend = "sensobol",
      matrices = c("A", "B", "AB"),
      first = "saltelli",
      total = "jansen",
      factor_metadata = factor_metadata
    ))
  }

  # for non-Sobol path, simple sequential or sampled design
  sampled_inputs[["parameters"]] <- list(ids = seq_len(ensemble_size))
  for (input_tag in setdiff(names(samp), "parameters")) {
    input_result <- PEcAn.uncertainty::input.ens.gen(
      settings = settings,
      ensemble_size = ensemble_size,
      input = input_tag,
      method = samp[[input_tag]]$method
    )
    sampled_inputs[[input_tag]] <- input_result
    design_list[[input_tag]] <- input_result$ids
  }

  if (!file.exists(file.path(settings$outdir, "samples.Rdata"))) {
    PEcAn.uncertainty::get.parameter.samples(
      settings,
      ensemble.size = ensemble_size,
      posterior.files,
      ens.sample.method
    )
  }

  design_list[["param"]] <- sampled_inputs[["parameters"]]$ids
  design_matrix <- tibble::as_tibble(design_list)
  return(list(X = design_matrix))
}
