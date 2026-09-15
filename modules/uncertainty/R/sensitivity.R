#' Reads output of sensitivity analysis runs
#'
#'
#' @return dataframe with one col per quantile analysed and one row per trait,
#'  each cell is a list of AGB over time
#' @param traits model parameters included in the sensitivity analysis
#' @param quantiles quantiles selected for sensitivity analysis
#' @param pecandir specifies where pecan writes its configuration files
#' @param outdir directory with model output to use in sensitivity analysis
#' @param pft.name name of PFT used in sensitivity analysis (Optional)
#' @param start.year first year to include in sensitivity analysis
#' @param end.year last year to include in sensitivity analysis
#' @param variable variables to be read from model output
#' @param per.pft flag to determine whether we want SA on pft-specific variables
#' @param sa.run.ids list of run ids to read.
#'   If NULL, will look in `pecandir` for a file named `samples.Rdata`
#'   and read from that
#' @export
#' @author Ryan Kelly, David LeBauer, Rob Kooper, Mike Dietze, Istem Fer, Akash B V
read.sa.output <- function(traits, quantiles, pecandir, outdir, pft.name = "",
                           start.year, end.year, variable, sa.run.ids = NULL,
                           per.pft = FALSE) {

  # Load Manifest
  manifest_file <- file.path(pecandir, "runs_manifest.csv")
  if (!file.exists(manifest_file)) {
    PEcAn.logger::logger.severe("runs_manifest.csv not found in ", pecandir)
  }
  manifest <- utils::read.csv(manifest_file, stringsAsFactors = FALSE)

  sa.output <- matrix(nrow = length(quantiles),
                      ncol = length(traits),
                      dimnames = list(quantiles, traits))

  expr <- variable$expression
  variables <- variable$variables

  for (trait in traits) {
    for (quantile in quantiles) {
      # We look for the row that matches the current pft, trait, and quantile.
      subset_df <- manifest[
        manifest$type == "Sensitivity" & 
        manifest$pft_name == pft.name &
        manifest$trait == trait &
        as.character(manifest$quantile) == as.character(quantile), 
      ]

      if (nrow(subset_df) == 1) {
         run.id <- subset_df$run_id
      } else if (nrow(subset_df) > 1) {
         PEcAn.logger::logger.warn("Multiple runs found for", trait, quantile, "- using the last one.")
         run.id <- utils::tail(subset_df$run_id, 1)
      } else {
         PEcAn.logger::logger.warn("No run found in manifest for", trait, quantile)
         next # Skip this quantile
      }

      if (is.null(run.id) || is.na(run.id)) {
         PEcAn.logger::logger.warn("Run ID invalid or missing for", trait, quantile)
         next
      }

      pass_pft <- if (isTRUE(per.pft)) pft.name else NULL

      # TODO: If adding time-based filtering, consider dataframe = TRUE
      # See benchmark module for usage example
      
      # Pass ALL variables at once to avoid repeated file opening. And call read.output once
      out.tmp <- PEcAn.utils::read.output(
        runid = run.id,
        outdir = file.path(outdir, run.id),
        start.year = start.year, end.year = end.year,
        variables = variables,
        pft.name = pass_pft
      )
      
      # Assign loaded variables to local environment for expression evaluation
      for (var in names(out.tmp)) {
        assign(var, out.tmp[[var]])
      }

      # derivation & aggregation
      out <- eval(parse(text = expr))

      sa.output[quantile, trait] <- mean(out, na.rm = TRUE)

    } ## end loop over quantiles
    PEcAn.logger::logger.info("reading sensitivity analysis output for model run at ", quantiles, "quantiles of trait", trait)
  } ## end loop over traits
  return(as.data.frame(sa.output))
} # read.sa.output

