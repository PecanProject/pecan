# Resolve the mslsp-extract package root for CLI entry points.
# Set MSLSP_EXTRACT_ROOT, or run scripts via Rscript from this package (auto-detect).

mslsp_extract_pkg_root <- function() {
  env <- trimws(Sys.getenv("MSLSP_EXTRACT_ROOT", ""))
  if (nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop(
      "Set MSLSP_EXTRACT_ROOT to the mslsp-extract package directory ",
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
    "Could not locate mslsp-extract root from the calling script. ",
    "Set MSLSP_EXTRACT_ROOT."
  )
}

load_mslsp_extract <- function() {
  if (isTRUE(getOption("mslsp_extract.loaded"))) {
    return(invisible(TRUE))
  }
  root <- mslsp_extract_pkg_root()
  source(file.path(root, "scripts", "_lib", "bootstrap.R"))
  options(mslsp_extract.loaded = TRUE)
  invisible(TRUE)
}
