# Load shared helpers for statewide event generation.

load_events_lib <- function() {
  if (isTRUE(getOption("events.lib.loaded"))) {
    return(invisible(TRUE))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0L) {
    stop("load_events_lib() must be called from make_events_statewide.R (Rscript --file=...).")
  }
  root <- dirname(normalizePath(sub("^--file=", "", file_arg[1L]), mustWork = FALSE))
  lib <- file.path(root, "_lib")
  source(file.path(lib, "paths.R"))
  source(file.path(lib, "io.R"))
  source(file.path(lib, "matched_input.R"))
  source(file.path(lib, "trait_pool.R"))
  source(file.path(lib, "phenology_events.R"))
  source(file.path(lib, "planting_events.R"))
  source(file.path(lib, "harvest_events.R"))
  source(file.path(lib, "tillage_events.R"))
  options(events.lib.loaded = TRUE)
  invisible(root)
}
