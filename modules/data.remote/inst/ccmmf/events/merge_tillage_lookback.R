#!/usr/bin/env Rscript
# Fold tillage lookback amend parquets into canonical yearly products.
# Usage: Rscript merge_tillage_lookback.R [year ...]
# Default years: 2016-2022 (amends come from job year+1).

args <- commandArgs(trailingOnly = TRUE)
years <- if (length(args) > 0L) {
  as.integer(args)
} else {
  2016:2022
}

root <- Sys.getenv("EVENTS_ROOT", "")
if (!nzchar(root)) {
  root <- tryCatch(
    dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]))),
    error = function(e) getwd()
  )
}
source(file.path(root, "R", "bootstrap.R"))
source(file.path(root, "R", "paths.R"))
source(file.path(root, "R", "io.R"))
source(file.path(root, "R", "tillage_events.R"))

out_dir <- events_paths()$out_dir
message("[tillage-merge] out_dir=", out_dir, " years=", paste(years, collapse = ","))
merge_tillage_lookback(out_dir, years)
message("[tillage-merge] done")
