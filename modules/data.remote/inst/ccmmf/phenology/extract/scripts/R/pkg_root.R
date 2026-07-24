# Resolve the phenology/extract package root for CLI entry points.
# Set PHENOLOGY_ROOT (parent of extract/), or run scripts via Rscript (auto-detect).

mslsp_extract_pkg_root <- function() {
  pheno <- trimws(Sys.getenv("PHENOLOGY_ROOT", ""))
  if (nzchar(pheno)) {
    return(normalizePath(file.path(pheno, "extract"), mustWork = FALSE))
  }

  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop(
      "Set PHENOLOGY_ROOT to the phenology package directory ",
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
    "Could not locate phenology/extract root from the calling script. ",
    "Set PHENOLOGY_ROOT."
  )
}

load_mslsp_extract <- function() {
  if (isTRUE(getOption("mslsp_extract.loaded"))) {
    return(invisible(TRUE))
  }
  root <- mslsp_extract_pkg_root()
  source(file.path(root, "scripts", "R", "bootstrap.R"))
  options(mslsp_extract.loaded = TRUE)
  invisible(TRUE)
}
