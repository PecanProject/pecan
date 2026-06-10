#!/usr/bin/env Rscript

#' Combine a bunch of `sipnet.out` files into a single file
#'
#' @param directory Parent directory to search for `sipnet.out` files. You must
#' provide either this or an explicit vector of `files`.
#' @param outfile File to which combined `sipnet.out` will be written
#' @param files Optional vector of paths to files to combine. All must be
#' readable with [read_sipnet_out()]. If `NULL` (default), looks for files
#' named `sipnet.out` in `directory` and its subdirectories, recursively.
#'
#' @return `outfile` (path to output file), invisibly
#' @export
combine_sipnet_out <- function(directory, outfile, files = NULL) {
  if (missing(directory) && is.null(files)) {
    PEcAn.logger::logger.severe("Must provide either `directory` or `files`")
  }
  if (is.null(files)) {
    # NOTE that this expects file paths (including parent directories) to be
    # lexicographically sorted. For the common case of segmented SIPNET runs,
    # the parent directories are named `segment_001`, `segment_002`, etc., so
    # this will work automatically.
    # If you don't want to make this assumption, or have a custom sort order,
    # pass `files` directly to this function.
    files <- sort(list.files(directory, "sipnet\\.out", full.names = TRUE, recursive = TRUE))
  }
  if (length(files) == 0) {
    PEcAn.logger::logger.severe("No files provided; nothing to combine.")
  }
  flist <- lapply(files, read_sipnet_out)
  combined <- do.call(rbind.data.frame, flist)
  # Mimic the SIPNET fixed-width right-aligned format
  combined_fwf <- format(combined, justify = "right")
  utils::write.table(combined_fwf, outfile, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "  ")
  invisible(outfile)
}
