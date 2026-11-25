#' Predict period maps of net ecosystem exchange and latent heat flux
#'
#' Run the period flux models on a grid window using environmental covariates.
#' The prediction backend runs in a separate Linux Python process. The package
#' README describes the CPU and CUDA environments and the required map inputs.
#'
#' @param config Path to a prediction YAML file. Paths in the YAML are relative
#'   to its directory. Choose a fresh output directory for a new request.
#' @param python Python executable in the prediction environment.
#' @param resume Logical. Continue a matching run using its verified blocks.
#' @return A data frame with integer `period_id`, character `variable` (NEE or
#'   LE), POSIXct `start_utc` and `stop_utc_exclusive`, and absolute GeoTIFF
#'   `path`. The seven bands contain the fold mean, population standard
#'   deviation across folds, three individual fold predictions, eligibility,
#'   and a missing-reason bit mask.
#' @examples
#' \dontrun{
#' maps <- predict_flux_maps("map-request.yaml")
#' nee <- terra::rast(maps$path[maps$variable == "NEE"][1])
#' }
#' @export
predict_flux_maps <- function(config, python = Sys.getenv("RETICULATE_PYTHON", "python3"),
                              resume = FALSE) {
  if (!is.character(config) || length(config) != 1L || is.na(config) || !nzchar(config)) {
    stop("Provide the path to one prediction YAML file.")
  }
  config <- normalizePath(config, mustWork = TRUE)
  if (!is.logical(resume) || length(resume) != 1L || is.na(resume)) {
    stop("resume must be TRUE or FALSE.")
  }
  if (!is.character(python) || length(python) != 1L || is.na(python) ||
      !nzchar(python) || !nzchar(Sys.which(python))) {
    stop("Python executable unavailable: ", paste(python, collapse = ", "))
  }
  launcher <- system.file("python", "predict_flux_maps.py",
                          package = "PEcAnAssimSequential", mustWork = TRUE)
  inventory <- tempfile(fileext = ".csv")
  on.exit(unlink(inventory))
  args <- c("-B", shQuote(launcher), "run", "--config", shQuote(config),
            "--inventory", shQuote(inventory), if (resume) "--resume")
  log <- suppressWarnings(system2(python, args, stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(log, "status"))) {
    stop("Prediction failed (exit ", attr(log, "status"), "):\n", paste(log, collapse = "\n"),
         call. = FALSE)
  }
  maps <- utils::read.csv(inventory, colClasses = c("integer", rep("character", 4)),
                          fileEncoding = "UTF-8")
  maps$start_utc <- as.POSIXct(maps$start_utc, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  maps$stop_utc_exclusive <- as.POSIXct(maps$stop_utc_exclusive,
                                        format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  maps
}
