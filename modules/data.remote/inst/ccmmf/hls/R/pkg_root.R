# Resolve the hls component root for CLI entry points.
# Set HLS_ROOT, or run scripts via Rscript from this component (auto-detect).

hls_pkg_root <- function() {
  env <- trimws(Sys.getenv("HLS_ROOT", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop(
      "Set HLS_ROOT to the hls component directory ",
      "(the folder that contains R/bootstrap.R)."
    )
  }

  dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  for (k in seq_len(6L)) {
    if (file.exists(file.path(dir, "R", "bootstrap.R"))) {
      return(normalizePath(dir, mustWork = FALSE))
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      break
    }
    dir <- parent
  }

  stop("Could not locate the scripts/hls root from the calling script. Set HLS_ROOT.")
}

# Load the shared tilewise framework (core + weighted-stats helpers). Product
# implementations (MSLSP / NDTI) are sourced per-driver so a run only loads the
# libraries and side effects of the product it is running.
load_hls <- function() {
  if (isTRUE(getOption("hls.loaded"))) {
    return(invisible(TRUE))
  }
  root <- hls_pkg_root()
  source(file.path(root, "R", "bootstrap.R"))
  options(hls.loaded = TRUE)
  invisible(TRUE)
}
