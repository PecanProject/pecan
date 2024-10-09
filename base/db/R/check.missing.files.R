#' Function to check if result has empty or missing files
#'
#' @param result A list of dataframes with file paths
#' @param outname Name of the output file
#' @param existing.input Existing input records
#' @param existing.dbfile Existing dbfile records
#' @return A list of dataframes with file paths, a list of strings with the output file name, a list of existing input records, and a list of existing dbfile records
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
    log_format_df <- function(df) {
      formatted_df <- rbind(colnames(df), format(df))
      formatted_text <- purrr::reduce(formatted_df, paste, sep = " ")
      paste(formatted_text, collapse = "\n")
    }

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
