#' Prepare Yang ensemble training targets at stations
#'
#' Generate three-hour station covariates, route 25 NEE and LE members, and
#' aggregate their finite values over the supplied UTC calendar. See the module
#' README for the station inputs and units.
#' @param config Path to the station input YAML. Relative paths are resolved
#'   beside the YAML. See the installed flux-targets/prepare.yaml example.
#' @param outdir New product directory. Its parent may already exist.
#' @param models Optional directory of fitted Yang RDS bundles. If omitted,
#'   fit and save all 25 residual bundles for each flux in outdir/models.
#' @param python Python executable with the station processing dependencies.
#' @return Named paths to targets/nee.h5 and targets/le.h5. The output also
#'   contains three-hour station features and member Parquet partitions.
#' @examples
#' \dontrun{
#' prepare_flux_targets("station-inputs.yaml", "station-targets", models = "yang-models")
#' }
#' @export
prepare_flux_targets <- function(config, outdir, models = NULL,
                                 python = Sys.getenv("RETICULATE_PYTHON", "python3")) {
  if (length(config) != 1L || length(outdir) != 1L || is.na(outdir) || !nzchar(outdir)) {
    stop("Provide one YAML file and one output directory")
  }
  config <- normalizePath(config, mustWork = TRUE)
  if (file.exists(outdir)) stop("Target output already exists: ", outdir)
  if (!is.null(models)) {
    if (length(models) != 1L || !dir.exists(models)) stop("Provide one residual model directory")
    models <- normalizePath(models, mustWork = TRUE)
  }
  if (length(python) != 1L || !nzchar(Sys.which(python))) stop("Python executable unavailable")
  launcher <- system.file("python", "prepare_flux_targets.py", package = "PEcAnAssimSequential", mustWork = TRUE)
  args <- c("-B", shQuote(launcher), shQuote(config), shQuote(path.expand(outdir)), "--rscript",
            shQuote(file.path(R.home("bin"), "Rscript")), if (!is.null(models)) c("--models", shQuote(models)))
  status <- system2(python, args)
  if (status != 0L) stop("Station target preparation failed (exit ", status, ")")
  file.path(normalizePath(outdir, mustWork = TRUE), "targets", c("nee.h5", "le.h5")) |>
    stats::setNames(c("NEE", "LE"))
}
