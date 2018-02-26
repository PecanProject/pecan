#' Match a file
#'
#' Return a list of files given a full prefix and optional suffix. Optionally, 
#' confirm that the right number of files are returned. If the wrong number of 
#' files is returned, throw an error.
#' @param path_prefix Full path and file prefix
#' @param suffix File suffix, as character (default = `NULL`)
#' @param expect Number of files expected to be returned (default = `NULL`)
#' @return Character vector of matched file names, as full paths.
#' @export
match_file <- function(path_prefix, suffix = NULL, expect = NULL) {
  path <- dirname(path_prefix)
  prefix <- basename(path_prefix)
  file_matches <- list.files(path, prefix, full.names = TRUE)
  if (!is.null(suffix)) {
    file_matches <- grep(paste0(suffix, "$"), file_matches, value = TRUE)
  }
  if (!is.null(expect) && length(file_matches) != expect) {
    PEcAn.logger::logger.severe(
      "Expected ", expect, " files but found ", length(file_matches), ". ",
      "The following prefix was used: ", path_prefix
    )
  }
  if (!length(file_matches) > 0 && (is.null(expect) || expect != 0)) {
    PEcAn.logger::logger.warn(
      "No files found. The following prefix was used: ", path_prefix
    )
  }
  file_matches
}
