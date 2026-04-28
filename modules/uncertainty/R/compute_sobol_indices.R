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
#' One variable per call. For a multisite outdir (one ensemble.output file
#' per site per variable) pass `ensemble_id` to pick the right file; loop
#' sites and variables in the caller.
#'
#' @param outdir PEcAn run output directory containing \code{ensemble.output.*.Rdata}
#'   files.
#' @param sobol_obj object produced by
#'   \code{PEcAn.uncertainty::generate_joint_ensemble_design(..., sobol = TRUE)}.
#' @param var Variable name to summarize (default \code{"GPP"}).
#' @param ensemble_id optional ensemble id; filters
#'   \code{ensemble.output.<ensemble_id>.<var>.<startyr>.<endyr>.Rdata} when
#'   the outdir holds output from more than one site. NULL means expect one
#'   matching file per var.
#' @param boot pass through to \code{sensobol::sobol_indices}; TRUE returns
#'   bootstrap CIs.
#' @param R bootstrap replicates when \code{boot = TRUE}. NULL keeps the
#'   sensobol default.
#'
#' @examples
#' \dontrun{
#'   # single site
#'   compute_sobol_indices(outdir, sobol_obj, var = "GPP")
#'
#'   # multisite - loop sites and vars, disambiguate via ensemble_id
#'   for (i in seq_along(settings)) {
#'     eid <- settings[[i]]$ensemble$ensemble.id
#'     for (v in variables) {
#'       compute_sobol_indices(outdir, sobol_obj, var = v,
#'                             ensemble_id = eid, boot = TRUE, R = 500)
#'     }
#'   }
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
                                  var = "GPP",
                                  ensemble_id = NULL,
                                  boot = FALSE,
                                  R = NULL) {
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

  # filename layout is ensemble.output.<ensemble_id>.<var>.<startyr>.<endyr>.Rdata
  toks       <- strsplit(basename(output_files), "\\.")
  output_eid <- vapply(toks,
    function(x) if (length(x) >= 3) x[[3]] else NA_character_,
    character(1)
  )
  output_var <- vapply(toks,
    function(x) if (length(x) >= 4) x[[4]] else NA_character_,
    character(1)
  )

  keep <- output_var == var
  if (!is.null(ensemble_id)) {
    keep <- keep & output_eid == ensemble_id
  }
  matched_files <- output_files[keep]

  if (length(matched_files) == 0) {
    PEcAn.logger::logger.severe(
      "No standardized ensemble output found for variable '", var,
      "'", if (!is.null(ensemble_id)) paste0(" with ensemble_id '", ensemble_id, "'"),
      " in ", outdir
    )
  }
  if (length(matched_files) > 1) {
    PEcAn.logger::logger.severe(
      "Multiple ensemble outputs match variable '", var,
      "' in ", outdir,
      ". Pass ensemble_id to disambiguate or keep one matching file."
    )
  }

  output_file <- matched_files[[1]]
  output_env <- new.env(parent = emptyenv())
  load(output_file, envir = output_env)
  if (is.null(output_env$ensemble.output)) {
    PEcAn.logger::logger.error(
      "object `ensemble.output` missing from output file ",
      output_file
    )
  }

  y <- as.numeric(unlist(output_env$ensemble.output, use.names = FALSE))
  expected_length <- sobol_obj$N * (length(sobol_obj$params) + 2L)
  if (length(y) != expected_length) {
    PEcAn.logger::logger.error(
      "ensemble output has ", length(y),
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
    boot = boot,
    R = R
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
