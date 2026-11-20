#' Compute Sobol indices from a finished PEcAn run
#'
#' @param ensemble_output_file the \code{ensemble.output} Rdata file that
#'   \code{\link{get.results}} saves for one site and variable, named by
#'   \code{\link{ensemble.filename}}. It holds one value per run, in run
#'   order, which is the order of the design rows.
#' @param sobol_design design returned by
#'   \code{generate_joint_ensemble_design(..., sobol = TRUE)} and used to
#'   write the configs for this run.
#' @param nboot number of bootstrap replicates for confidence intervals on the
#'   indices. Defaults to what \code{sobol_design} was built with, which is 0
#'   (no intervals) for a design from \code{generate_joint_ensemble_design()}.
#'
#' @return \code{sobol_design} with first and total order indices filled in
#'   by \code{sensitivity::tell()}, with their confidence intervals when
#'   \code{nboot} is above 0.
#' @export
compute_sobol_indices <- function(ensemble_output_file, sobol_design,
                                  nboot = sobol_design$nboot) {
  y <- unlist(PEcAn.utils::load_local(ensemble_output_file)$ensemble.output,
              use.names = FALSE)
  # file and design from different runs: tell() only warns, or says nothing
  stopifnot(length(y) == nrow(sobol_design$X))
  sobol_design$nboot <- nboot
  sensitivity::tell(sobol_design, y)
}
