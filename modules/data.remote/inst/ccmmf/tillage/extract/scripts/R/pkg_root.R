# Resolve the tillage/extract package root for CLI entry points.
# Set TILLAGE_ROOT (parent of extract/), or run scripts via Rscript (auto-detect).

ndti_extract_pkg_root <- function() {
  tillage <- trimws(Sys.getenv("TILLAGE_ROOT", ""))
  if (nzchar(tillage)) {
    return(normalizePath(file.path(tillage, "extract"), mustWork = FALSE))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop(
      "Set TILLAGE_ROOT to the tillage package directory ",
      "(the folder that contains extract/scripts/R/bootstrap.R)."
    )
  }

  dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  for (k in seq_len(6L)) {
    if (file.exists(file.path(dir, "scripts", "R", "bootstrap.R"))) {
      return(normalizePath(dir, mustWork = FALSE))
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      break
    }
    dir <- parent
  }

  stop(
    "Could not locate tillage/extract root from the calling script. ",
    "Set TILLAGE_ROOT."
  )
}

load_ndti_extract <- function() {
  if (isTRUE(getOption("ndti_extract.loaded"))) {
    return(invisible(TRUE))
  }
  root <- ndti_extract_pkg_root()
  source(file.path(root, "scripts", "R", "bootstrap.R"))
  options(ndti_extract.loaded = TRUE)
  invisible(TRUE)
}
