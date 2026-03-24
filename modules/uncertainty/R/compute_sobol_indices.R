#' Compute Sobol indices from a finished PEcAn run
#'
#' Loads standardized ensemble output for a Sobol run, computes first-order and
#' total-order Sobol indices with \code{sensobol}, and saves both the Sobol design
#' metadata and the computed indices using PEcAn-style filenames.
#'
#' First-order indices quantify the share of output variance attributable to a
#' factor alone, while total-order indices summarize the full contribution of
#' that factor including its interactions with other factors. PEcAn uses
#' \code{sensobol} for these variance-based estimators; see Saltelli et al. (2008)
#' for methodological background and Puy et al. (2022) for package details.
#'
#' This function handles one output variable at a time. To compute indices for
#' multiple variables, call it in a loop (see examples).
#'
#' @param outdir PEcAn run output directory containing \code{ensemble.output.*.Rdata}
#'   files.
#' @param sobol_obj object produced by
#'   \code{PEcAn.uncertainty::generate_joint_ensemble_design(..., sobol = TRUE)}.
#' @param var Variable name to summarize (default \code{"GPP"}).
#'
#' @examples
#' \dontrun{
#'   # single variable
#'   result <- compute_sobol_indices(outdir, sobol_obj, var = "GPP")
#'
#'   # multiple variables
#'   vars <- c("GPP", "NPP", "TotSoilCarb")
#'   all_results <- purrr::map_dfr(vars, function(v) {
#'     compute_sobol_indices(outdir, sobol_obj, var = v) |>
#'       dplyr::mutate(variable = v)
#'   })
#' }
#'
#' @return A tibble of Sobol first-order and total-order indices with attached
#'   factor metadata.
#' @references Saltelli, A., Ratto, M., Andres, T., Campolongo, F., Cariboni,
#'   J., Gatelli, D., et al. (2008). Global Sensitivity Analysis: The Primer.
#'   John Wiley & Sons.
#'
#'   Puy, A., Lo Piano, S., Saltelli, A., and Levin, S. A. (2022).
#'   sensobol: An R Package to Compute Variance-Based Sensitivity Indices.
#'   Journal of Statistical Software, 102(5), 1-37.
#'   \doi{10.18637/jss.v102.i05}
#' @export
compute_sobol_indices <- function(outdir,
                                  sobol_obj,
                                  var = "GPP") {
  if (is.null(sobol_obj$backend) || sobol_obj$backend != "sensobol") {
    PEcAn.logger::logger.error(
      "compute_sobol_indices expects a sensobol design object returned by ",
      "generate_joint_ensemble_design(..., sobol = TRUE)"
    )
  }

  output_files <- list.files(
    outdir,
    pattern = "^ensemble\\.output\\..*\\.Rdata$",
    full.names = TRUE
  )
  if (length(output_files) == 0) {
    PEcAn.logger::logger.error("No ensemble.output.*.Rdata files found in ", outdir)
  }

  output_var <- vapply(
    strsplit(basename(output_files), "\\."),
    function(x) if (length(x) >= 4) x[[4]] else NA_character_,
    character(1)
  )
  matched_files <- output_files[output_var == var]

  if (length(matched_files) == 0) {
    PEcAn.logger::logger.error(
      "No standardized ensemble output found for variable '", var,
      "' in ", outdir
    )
  }
  if (length(matched_files) > 1) {
    PEcAn.logger::logger.error(
      "Multiple standardized ensemble outputs found for variable '", var,
      "' in ", outdir, ". Please keep only one matching file."
    )
  }

  output_file <- matched_files[[1]]
  output_env <- new.env(parent = emptyenv())
  load(output_file, envir = output_env)
  if (is.null(output_env$ensemble.output)) {
    PEcAn.logger::logger.error(
      "Object `ensemble.output` missing from standardized output file ",
      output_file
    )
  }

  y <- as.numeric(unlist(output_env$ensemble.output, use.names = FALSE))
  expected_length <- sobol_obj$N * (length(sobol_obj$params) + 2L)
  if (length(y) != expected_length) {
    PEcAn.logger::logger.error(
      "Standardized ensemble output has ", length(y),
      " values but expected ", expected_length,
      " for Sobol design size N = ", sobol_obj$N
    )
  }

  sobol_indices_result <- sensobol::sobol_indices(
    matrices = sobol_obj$matrices,
    Y = y,
    N = sobol_obj$N,
    params = sobol_obj$params,
    first = sobol_obj$first,
    total = sobol_obj$total,
    order = "first",
    boot = FALSE
  )

  sobol_results <- tibble::as_tibble(sobol_indices_result$results)
  if (!is.null(sobol_obj$factor_metadata)) {
    factor_metadata <- tibble::as_tibble(sobol_obj$factor_metadata) |>
      dplyr::rename(parameters = "factor")
    sobol_results <- dplyr::left_join(
      sobol_results,
      factor_metadata,
      by = "parameters"
    )
  }

  sobol_design <- sobol_obj
  save(
    sobol_design,
    file = file.path(
      outdir,
      sub("^ensemble\\.output\\.", "sobol.design.", basename(output_file))
    )
  )
  save(
    sobol_indices_result,
    sobol_results,
    file = file.path(
      outdir,
      sub("^ensemble\\.output\\.", "sobol.indices.", basename(output_file))
    )
  )

  return(sobol_results)
}
