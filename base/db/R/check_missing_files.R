#' Check for Missing or Empty Files in Conversion Results
#'
#' This function inspects the file paths in a list of data frames (typically produced by a download or conversion routine) to ensure that each file is present and non-empty. Specifically, it checks whether any file path is missing or has a file size of zero, and logs an error if such files are detected. It also normalizes `existing.input` and `existing.dbfile` so that each is returned as a list of data frames.
#'
#' @param result A list of data frames containing file information. Each data frame is expected to have a column named `file` with absolute file paths created by a data-conversion or download function. For example, this might be the structure returned by a "download_X" or "met2model_X" function when invoked via [convert_input()].
#' @param existing.input A data frame or list of data frames (possibly zero rows) representing input records in the BETY `inputs` table that match (or partially match) the data being added. This is converted to a list of data frames if it is not already.
#' @param existing.dbfile A data frame or list of data frames (possibly zero rows) representing dbfile records in the BETY `dbfiles` table that match (or partially match) the data being added. This is also converted to a list of data frames if it is not already.
#'
#' @return A list containing:
#' \itemize{
#'   \item A list of data frames for `existing.input`
#'   \item A list of data frames for `existing.dbfile`
#' }
#'
#' @details
#' The function calculates the file size for each file specified in the `result` data frames. If any file path is missing (`NA`) or any file size is zero, the function raises a fatal error (via [PEcAn.logger::logger.severe]) indicating that an expected file is either nonexistent or empty. If no such issues are found, it merely ensures that `existing.input` and `existing.dbfile` are each wrapped in a list for consistent downstream usage.
#'
#' @author Betsy Cowdery, Michael Dietze, Ankur Desai, Tony Gardella, Luke Dramko

check_missing_files <- function(result, existing.input = NULL, existing.dbfile = NULL) {
  result_sizes <- purrr::map_dfr(
    result,
    ~ dplyr::mutate(
      .,
      file_size = purrr::map_dbl(file, file.size),
      missing = is.na(file_size),
      empty = file_size == 0
    )
  )

  if (any(result_sizes$missing) || any(result_sizes$empty)) {
    PEcAn.logger::logger.severe(
      "Requested Processing produced empty files or Nonexistent files:\n",
      log_format_df(result_sizes[, c(1, 8, 9, 10)]),
      "\n Table of results printed above.",
      wrap = FALSE
    )
  }


  # Wrap in a list for consistent processing later
  if (is.data.frame(existing.input)) {
    existing.input <- list(existing.input)
  }

  if (is.data.frame(existing.dbfile)) {
    existing.dbfile <- list(existing.dbfile)
  }
  return(list(existing.input, existing.dbfile))
}

log_format_df <- function(df) {
  formatted_df <- rbind(colnames(df), format(df))
  formatted_text <- purrr::reduce(formatted_df, paste, sep = " ")
  paste(formatted_text, collapse = "\n")
}