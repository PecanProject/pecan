#' Compute Sobol indices from a finished PEcAn run
#'
#' Loads model outputs from a Sobol ensemble, calculates summary
#' statistics for a chosen variable, feeds them to \code{sensitivity::tell()},
#' and returns the updated Sobol object.
#'
#' @param outdir     PEcAn run output directory that contains runs.txt
#' @param sobol_obj  object produced by PEcAn.uncertainty::generate_joint_ensemble_design()
#' @param var        Variable name to summarise (default "GPP").
#' @param stat_fun   Summary statistic applied to var default mean .
#'
#' @return           sobol_obj 
#' .
#' @export
compute_sobol_indices <- function(outdir,
                                  sobol_obj,
                                  var = "GPP",
                                  stat_fun = mean) {
 
 
 
  runs_file <- file.path(outdir, "runs.txt")
  if (!file.exists(runs_file)) {
    PEcAn.logger::logger.error("runs.txt not found in ", outdir)
   }
  run_ids <- readLines(runs_file) 
  

  
  # Load outputs and compute response vector y
  y <- vapply(run_ids, function(rid) {
    fpath <- file.path(outdir, rid)
    out   <- PEcAn.utils::read.output(runid = rid, outdir = fpath)
    if (!is.list(out) || !var %in% names(out)) {
      PEcAn.logger::logger.error("Variable '", var, "' missing in output for run ", rid)
    }
    stat_fun(out[[var]], na.rm = TRUE)
  }, numeric(1))
  
  # Compute Sobol indices
  sobol_obj <-sensitivity::tell(sobol_obj, y)
  
  # Return the updated object
  return(invisible(sobol_obj))
}
