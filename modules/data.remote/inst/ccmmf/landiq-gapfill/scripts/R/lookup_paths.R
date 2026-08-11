# Resolve CDL x LandIQ probability tables under outputs/.
#
# Read: find whatever cdl_prob_by_class_*.parquet is present (newest if several).
# Write (cdl-landiq-probs): name files from the training-year window.

landiq_lookup_years <- function() {
  train_years <- landiq_emission_training_years()
  yr_min <- min(train_years)
  yr_max <- max(train_years)
  excluded <- sort(setdiff(seq.int(yr_min, yr_max), train_years))
  list(
    yr_min = yr_min,
    yr_max = yr_max,
    excluded = excluded,
    train_years = train_years
  )
}

#' Pick a filename suffix from outputs/ for stem_*.parquet (newest if several).
.gapfill_discover_output_suffix <- function(stem) {
  out <- path_outputs()
  files <- list.files(
    out,
    pattern = paste0("^", stem, "_.+\\.parquet$"),
    full.names = TRUE
  )
  if (!length(files)) {
    return(NA_character_)
  }
  if (length(files) > 1L) {
    files <- files[which.max(file.info(files)$mtime)]
    message(
      "Multiple ", stem, "_*.parquet under ", out,
      "; using newest: ", basename(files)
    )
  }
  sub(paste0("^", stem, "_(.+)\\.parquet$"), "\\1", basename(files))
}

#' Suffix for reading CDL x LandIQ probability tables (discover under outputs/).
landiq_lookup_suffix <- function() {
  suf <- .gapfill_discover_output_suffix("cdl_prob_by_class")
  if (is.na(suf)) {
    stop(
      "No cdl_prob_by_class_*.parquet under ", path_outputs(), ".\n",
      "  Rebuild with: Rscript scripts/gapfill.R cdl-landiq-probs"
    )
  }
  need <- c(
    sprintf("cdl_prob_by_class_%s.parquet", suf),
    sprintf("cdl_prob_by_subclass_%s.parquet", suf),
    sprintf("landiq_subclass_frequency_%s.parquet", suf)
  )
  missing <- need[!file.exists(file.path(path_outputs(), need))]
  if (length(missing) > 0L) {
    stop(
      "Incomplete CDL x LandIQ probability tables (suffix=", suf, ") under ",
      path_outputs(), ".\n",
      "  Missing: ", paste(missing, collapse = ", "), "\n",
      "  Rebuild with: Rscript scripts/gapfill.R cdl-landiq-probs"
    )
  }
  suf
}

#' Suffix for writing probability tables from the current training-year window.
landiq_lookup_build_suffix <- function() {
  y <- landiq_lookup_years()
  suf <- sprintf("%d-%d", y$yr_min, y$yr_max)
  if (length(y$excluded) > 0L) {
    suf <- paste0(suf, "_excl", paste(y$excluded, collapse = "-"))
  }
  suf
}
