#' Train and export site-held-out NEE and LE map models
#'
#' Build training arrays from station products, fit the proxy and period
#' residual models and export the three outer folds for map prediction.
#' See the module README for environmental inputs and the Python environment.
#' @param config Path to the training YAML. Relative source paths resolve
#'   beside the YAML. See the installed flux-targets/train.yaml example.
#' @param outdir Output directory for prepared arrays, training stages,
#'   held-out scores and the model bundle.
#' @param python Python executable with the training dependencies.
#' @param resume Continue a matching run, verifying completed stages.
#' @return Absolute path to the exported model_bundle directory, suitable
#'   for the model_bundle field in a predict_flux_maps configuration.
#' @examples
#' \dontrun{
#' model_bundle <- train_flux_models("train.yaml", "training-run")
#' }
#' @export
train_flux_models <- function(config, outdir, python = Sys.getenv("RETICULATE_PYTHON", "python3"),
                              resume = FALSE) {
  path_arg <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!path_arg(config) || !path_arg(outdir)) stop("Provide one training YAML and one output directory")
  if (!is.logical(resume) || length(resume) != 1L || is.na(resume)) stop("resume must be TRUE or FALSE")
  if (!path_arg(python) || !nzchar(Sys.which(python))) stop("Python executable unavailable")
  config <- normalizePath(config, mustWork = TRUE)
  launcher <- system.file("python", "train_flux_models.py", package = "PEcAnAssimSequential", mustWork = TRUE)
  args <- c("-B", shQuote(launcher), shQuote(config), shQuote(path.expand(outdir)), if (resume) "--resume")
  status <- system2(python, args)
  if (status != 0L) stop("Flux model training failed (exit ", status, ")")
  normalizePath(file.path(outdir, "model_bundle"), mustWork = TRUE)
}
