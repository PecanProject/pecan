#' Write ED meteorlogy header
#'
#' Write ED met driver header from R met driver list object
#'
#' @param ed_metheader ED meteorlogy header object (see [read_ed_metheader])
#' @param filename Full file name (including path) of ED met header
#' @param header_line Character string for top line of output file. Default is
#' `'header'`.
#' @export
write_ed_metheader <- function(ed_metheader, filename,
                               header_line = shQuote("header")) {
  nformats <- length(ed_metheader)
  blocks <- vector("list", nformats)
  for (i in seq_len(nformats)) {
    metformat <- ed_metheader[[i]]
    block_lines <- character(6)
    prefix <- normalizePath(metformat$path_prefix, mustWork = FALSE)
    if (file.exists(prefix) && file.info(prefix)$isdir) {
      # ED doesn't treat directories specially.
      # Need to add trailing slash.
      prefix <- paste0(prefix, "/")
    }
    block_lines[1] <- prefix
    block_lines[2] <- paste(
      metformat$nlon,
      metformat$nlat,
      metformat$dx,
      metformat$dy,
      metformat$xmin,
      metformat$ymin
    )
    block_lines[3] <- nrow(metformat$variables)
    block_lines[4] <- paste(metformat$variables$variable, collapse = " ")
    block_lines[5] <- paste(metformat$variables$update_frequency, collapse = " ")
    block_lines[6] <- paste(metformat$variables$flag, collapse = " ")
    blocks[[i]] <- block_lines
  }
  file_lines <- c(header_line, as.character(nformats), Reduce(c, blocks))
  writeLines(file_lines, filename)
}

# TODO: First line does actually matter -- maybe a list of format names?
# Regardless, need to set it to something
