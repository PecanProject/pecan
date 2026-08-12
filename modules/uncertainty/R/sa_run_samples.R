#' Expand SA quantile samples into one parameter set per run
#'
#' \code{\link{get_parameter_samples}} returns sensitivity analysis samples as
#' one table per PFT, quantiles down the rows and traits across the columns.
#' The config writers want the opposite shape: one row per model run, holding
#' the parameter values that run uses. This converts between the two, using the
#' labels the design carries.
#'
#' Every run starts from each PFT's median values. A run that the design labels
#' as moving a trait then has that one value replaced with the trait's value at
#' the labelled quantile, which is what makes a one-at-a-time design separable.
#' The first row, the median run, keeps every value at its median.
#'
#' Entries that are not PFTs, such as \code{env}, are carried at their medians
#' for every run, matching what \code{write.sa.configs} passes today.
#'
#' @param sa_samples Sensitivity analysis samples, a named list with one
#'   data.frame per PFT, quantiles as rownames and traits as columns, as
#'   returned by \code{\link{get_parameter_samples}}.
#' @param design_matrix The design from \code{\link{generate_OAT_SA_design}},
#'   carrying \code{sa_pft}, \code{sa_trait} and \code{sa_quantile}.
#'
#' @return A named list with one data.frame per entry of \code{sa_samples}, each
#'   with one row per design row and one column per trait. This is the shape
#'   \code{\link{write.ensemble.configs}} takes as \code{ensemble.samples}.
#'
#' @seealso \code{\link{generate_OAT_SA_design}} for the labels this reads.
#' @export
sa_run_samples <- function(sa_samples, design_matrix) {
  MEDIAN <- "50"

  required <- c("sa_pft", "sa_trait", "sa_quantile")
  if (!all(required %in% names(design_matrix))) {
    PEcAn.logger::logger.severe(
      "design_matrix is missing the sensitivity analysis labels",
      paste0(paste(setdiff(required, names(design_matrix)), collapse = ", "), "."),
      "Generate the design with generate_OAT_SA_design()."
    )
  }

  n_runs <- nrow(design_matrix)
  run_samples <- list()

  for (pft_name in names(sa_samples)) {
    pft_samples <- sa_samples[[pft_name]]

    # every run starts at this PFT's medians
    medians <- pft_samples[MEDIAN, , drop = FALSE]
    per_run <- as.data.frame(medians[rep(1, n_runs), , drop = FALSE])
    rownames(per_run) <- NULL

    # the runs that move a trait of this PFT get that one value replaced
    moved <- which(!is.na(design_matrix$sa_pft) & design_matrix$sa_pft == pft_name)
    for (row in moved) {
      trait <- design_matrix$sa_trait[row]
      per_run[row, trait] <- pft_samples[design_matrix$sa_quantile[row], trait]
    }

    run_samples[[pft_name]] <- per_run
  }

  run_samples
}