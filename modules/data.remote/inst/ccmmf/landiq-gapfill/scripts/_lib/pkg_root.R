# Resolve the landiq-gapfill package root for CLI entry points.
# Set LANDIQ_GAPFILL_ROOT, or run scripts via Rscript from this package (auto-detect).

landiq_gapfill_pkg_root <- function() {
  env <- trimws(Sys.getenv("LANDIQ_GAPFILL_ROOT", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop(
      "Set LANDIQ_GAPFILL_ROOT to the landiq-gapfill package directory ",
      "(the folder that contains scripts/_lib/bootstrap.R)."
    )
  }

  dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  for (k in seq_len(6L)) {
    if (file.exists(file.path(dir, "scripts", "_lib", "bootstrap.R"))) {
      return(normalizePath(dir, mustWork = FALSE))
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      break
    }
    dir <- parent
  }

  stop(
    "Could not locate landiq-gapfill root from the calling script. ",
    "Set LANDIQ_GAPFILL_ROOT."
  )
}

load_landiq_gapfill <- function() {
  if (isTRUE(getOption("landiq_gapfill.loaded"))) {
    return(invisible(TRUE))
  }
  root <- landiq_gapfill_pkg_root()
  source(file.path(root, "scripts", "_lib", "bootstrap.R"))
  options(landiq_gapfill.loaded = TRUE)
  invisible(TRUE)
}
