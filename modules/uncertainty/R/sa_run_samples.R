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

#' Build run ids and descriptions for a sensitivity analysis design
#'
#' Sensitivity analysis runs are named after what they are, so a run directory
#' says which trait sits at which quantile. The design carries that, so the
#' names can be built before anything is written.
#'
#' @param design_matrix The design from \code{\link{generate_OAT_SA_design}}.
#' @param site_id Site id, used as the last part of each run id.
#' @param pft_names Names of the PFTs in the run, used to describe the median
#'   run, which moves nothing.
#'
#' @return A list with \code{ids}, one run id per design row, and
#'   \code{paramlists}, one description per design row for the runs table.
#'
#' @export
sa_run_ids <- function(design_matrix, site_id, pft_names) {
  n_runs <- nrow(design_matrix)
  ids <- character(n_runs)
  paramlists <- character(n_runs)

  for (row in seq_len(n_runs)) {
    if (is.na(design_matrix$sa_pft[row])) {
      ids[row] <- PEcAn.utils::get.run.id("SA", "median", site.id = site_id)
      paramlists[row] <- paste0(
        "quantile=MEDIAN,trait=all,pft=",
        paste(pft_names, sep = ",")
      )[1]
    } else {
      quantile <- as.numeric(design_matrix$sa_quantile[row]) / 100
      ids[row] <- PEcAn.utils::get.run.id(
        run.type = "SA",
        index    = round(quantile, 3),
        trait    = design_matrix$sa_trait[row],
        pft.name = design_matrix$sa_pft[row],
        site.id  = site_id
      )
      paramlists[row] <- paste0(
        "quantile=", design_matrix$sa_quantile[row],
        ",trait=", design_matrix$sa_trait[row],
        ",pft=", design_matrix$sa_pft[row]
      )
    }
  }

  list(ids = ids, paramlists = paramlists)
}


#' Index a sensitivity analysis run's ids by PFT, quantile and trait
#'
#' The sensitivity analysis post-processing looks runs up by which trait sits at
#' which quantile, rather than by run order. This builds that lookup from the
#' design and the ids the writer returned, so it no longer has to be produced as
#' a side effect of writing the configs.
#'
#' @param design_matrix The design from \code{\link{generate_OAT_SA_design}}.
#' @param run_ids Run ids, one per design row, in design order.
#'
#' @return A named list with one data.frame per PFT, quantiles as rownames and
#'   traits as columns, holding the run id for each combination. The median run
#'   fills the median row of every trait, since one run covers them all. This is
#'   the \code{sa.run.ids} object saved in \code{sensitivity.samples.<id>.Rdata}.
#'
#' @export
sa_run_id_table <- function(design_matrix, run_ids) {
  MEDIAN <- "50"

  median_id <- run_ids[[1]]
  moved <- !is.na(design_matrix$sa_pft)
  runs <- list()

  for (pft_name in unique(design_matrix$sa_pft[moved])) {
    rows <- which(moved & design_matrix$sa_pft == pft_name)
    run_table <- data.frame()

    # one median run stands in for every trait's median
    for (trait in unique(design_matrix$sa_trait[rows])) {
      run_table[MEDIAN, trait] <- median_id
    }

    for (row in rows) {
      run_table[design_matrix$sa_quantile[row], design_matrix$sa_trait[row]] <- run_ids[[row]]
    }

    runs[[pft_name]] <- run_table
  }

  runs
}