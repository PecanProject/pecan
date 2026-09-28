#' Compute Sobol indices from a finished PEcAn run
#'
#' Reads the ensemble output that \code{\link{get.results}} saved for one site
#' and variable, and feeds it to \code{sensitivity::tell()}. Each value is the
#' run's mean from \code{start.year} to \code{end.year}, in run order, which is
#' the order of the design rows.
#'
#' @param settings single-site PEcAn settings, as returned by
#'   \code{runModule.run.write.configs()} so that
#'   \code{settings$ensemble$ensemble.id} is set. For a multi-site run call
#'   this once per site.
#' @param sobol_obj design returned by
#'   \code{generate_joint_ensemble_design(..., sobol = TRUE)} and used to
#'   write the configs for this run.
#' @param variable output variable to compute indices for, as given in a
#'   \code{<variable>} entry of \code{settings$ensemble}.
#' @param start.year,end.year years the output was averaged over. Default to
#'   the ensemble years in \code{settings}, or NA as in \code{get.results()}.
#' @param nboot number of bootstrap replicates for confidence intervals on the
#'   indices. Defaults to what \code{sobol_obj} was built with, which is 0 (no
#'   intervals) for a design from \code{generate_joint_ensemble_design()}.
#'
#' @return \code{sobol_obj} with first and total order indices filled in by
#'   \code{sensitivity::tell()}, with their confidence intervals when
#'   \code{nboot} is above 0.
#' @export
compute_sobol_indices <- function(settings,
                                  sobol_obj,
                                  variable,
                                  start.year = settings$ensemble$start.year %||% NA,
                                  end.year = settings$ensemble$end.year %||% NA,
                                  nboot = sobol_obj$nboot) {
  if (!PEcAn.settings::is.Settings(settings)) {
    PEcAn.logger::logger.severe(
      "compute_sobol_indices takes a single site's settings;",
      "for a multi-site run call it once per site"
    )
  }

  # get.results() names the file for a derived variable by its left-hand side
  variable <- PEcAn.utils::convert.expr(variable)$variable.drv
  fname <- ensemble.filename(settings, "ensemble.output", "Rdata",
                             all.var.yr = FALSE,
                             variable = variable,
                             start.year = start.year,
                             end.year = end.year)
  if (!file.exists(fname)) {
    PEcAn.logger::logger.severe(
      "no ensemble output at", fname, "- run get.results() first"
    )
  }

  y <- unlist(PEcAn.utils::load_local(fname)$ensemble.output, use.names = FALSE)
  if (length(y) != nrow(sobol_obj$X)) {
    PEcAn.logger::logger.severe(
      "ensemble output has", length(y), "values but the sobol design has",
      nrow(sobol_obj$X), "rows"
    )
  }
  if (anyNA(y)) {
    PEcAn.logger::logger.severe(
      "runs with no", variable, "output:", sum(is.na(y)),
      "- sobol indices need every run"
    )
  }

  sobol_obj$nboot <- nboot
  sensitivity::tell(sobol_obj, y)
}
