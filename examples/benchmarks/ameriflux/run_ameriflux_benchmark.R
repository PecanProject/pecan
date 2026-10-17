# Benchmarking pipeline for AmeriFlux observations using PEcAn.benchmark toolkit
# Evaluates model observations retrieved from ccmmf/cal-val-data

logger.info <- function(...) message("[INFO] ", paste(...))
logger.warn <- function(...) message("[WARN] ", paste(...))
logger.severe <- function(...) stop("[SEVERE] ", paste(...))
if (requireNamespace("PEcAn.logger", quietly = TRUE)) {
  logger.info <- PEcAn.logger::logger.info
  logger.warn <- PEcAn.logger::logger.warn
  logger.severe <- PEcAn.logger::logger.severe
}

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Determine base directory portably
base_dir <- file.path(getwd(), "examples/benchmarks/ameriflux")
if (!dir.exists(base_dir)) {
  base_dir <- getwd()
}

if (!requireNamespace("PEcAn.benchmark", quietly = TRUE)) {
  bench_r_dir <- file.path(base_dir, "../../../modules/benchmark/R")
  if (dir.exists(bench_r_dir)) {
    r_files <- list.files(bench_r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
    invisible(lapply(r_files, source))
  } else if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(file.path(base_dir, "../../../modules/benchmark"))
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(file.path(base_dir, "../../../modules/benchmark"))
  }
} else {
  library(PEcAn.benchmark)
}

# URL for observations from cal-val-data repo main branch
cal_val_obs_url <- "https://raw.githubusercontent.com/ccmmf/cal-val-data/v0.1.0/data/observations.csv"

logger.info("Attempting to fetch AmeriFlux observations from:", cal_val_obs_url)

obs_data <- tryCatch({
  read.csv(cal_val_obs_url, stringsAsFactors = FALSE)
}, error = function(e) {
  logger.warn("Could not fetch observations from GitHub URL (repository may be private or requires authentication):", conditionMessage(e))
  logger.warn("Checking for local fallback observations file in inst/extdata...")
  local_fallback <- file.path(base_dir, "../../../modules/benchmark/inst/extdata/simulated_observations.csv")
  if (file.exists(local_fallback)) {
    read.csv(local_fallback, stringsAsFactors = FALSE)
  } else {
    NULL
  }
})

if (is.null(obs_data) || nrow(obs_data) == 0) {
  logger.warn("AmeriFlux observations unavailable (URL requires authentication / release v0.1.0 pending). Skipping AmeriFlux benchmark execution.")
} else {
  logger.info(sprintf("Successfully loaded %d observation records for AmeriFlux benchmarking.", nrow(obs_data)))
  print(head(obs_data))
}
