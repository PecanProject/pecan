#' Standard Preprocessing Result Object
#'
#' @description
#' Construct a minimal, uniform result object for preprocessing functions.
#' This object is intended to standardize the current mix of return values such
#' as path strings, nested lists, and BETY identifier lists.
#'
#' @param tag Character scalar identifying the input type. One of
#'   `"met"`, `"soil"`, `"ic"`, `"phenology"`, or `"fia"`.
#' @param paths Character vector of output file paths.
#' @param input_id Integer scalar BETY input ID, or `NA_integer_` when unused.
#' @param dbfile_id Integer scalar BETY dbfile ID, or `NA_integer_` when unused.
#' @param format Character scalar naming the output format.
#' @param source Character scalar naming the source dataset or provider.
#' @param status Character scalar. One of `"success"`, `"error"`, or
#'   `"skipped"`.
#' @param error_message Character scalar or `NULL`. Error detail when
#'   `status == "error"`.
#'
#' @return An object of class `pecan_preprocess_result`.
#' @export
#'
#' @examples
#' standard_result(
#'   tag = "met",
#'   paths = "/data/dbfiles/ERA5_772_2005.nc",
#'   input_id = 42001L,
#'   dbfile_id = 55003L,
#'   format = "CF Meteorology",
#'   source = "ERA5"
#' )
standard_result <- function(tag,
                            paths = character(),
                            input_id = NA_integer_,
                            dbfile_id = NA_integer_,
                            format = "",
                            source = "",
                            status = "success",
                            error_message = NULL) {
  result <- list(
    tag = tag,
    paths = as.character(paths),
    input_id = as.integer(input_id),
    dbfile_id = as.integer(dbfile_id),
    format = format,
    source = source,
    status = status,
    error_message = error_message
  )

  class(result) <- c("pecan_preprocess_result", "list")
  validate_standard_result(result)
}

#' Validate a Standard Preprocessing Result
#'
#' @description
#' Validate the structure and field types of a standard preprocessing result.
#' Returns the input object invisibly when valid and throws an error otherwise.
#'
#' @param x Object to validate.
#'
#' @return `x`, invisibly, when validation succeeds.
#' @export
validate_standard_result <- function(x) {
  allowed_tags <- c("met", "soil", "ic", "phenology", "fia")
  allowed_status <- c("success", "error", "skipped")

  if (!inherits(x, "pecan_preprocess_result")) {
    stop("`x` must inherit from 'pecan_preprocess_result'.", call. = FALSE)
  }
  if (!is.list(x)) {
    stop("`x` must be a list.", call. = FALSE)
  }
  if (!is.character(x$tag) || length(x$tag) != 1L || is.na(x$tag) || !(x$tag %in% allowed_tags)) {
    stop("`tag` must be one of: met, soil, ic, phenology, fia.", call. = FALSE)
  }
  if (!is.character(x$paths)) {
    stop("`paths` must be a character vector.", call. = FALSE)
  }
  if (!is.integer(x$input_id) || length(x$input_id) != 1L) {
    stop("`input_id` must be an integer scalar.", call. = FALSE)
  }
  if (!is.integer(x$dbfile_id) || length(x$dbfile_id) != 1L) {
    stop("`dbfile_id` must be an integer scalar.", call. = FALSE)
  }
  if (!is.character(x$format) || length(x$format) != 1L) {
    stop("`format` must be a character scalar.", call. = FALSE)
  }
  if (!is.character(x$source) || length(x$source) != 1L) {
    stop("`source` must be a character scalar.", call. = FALSE)
  }
  if (!is.character(x$status) || length(x$status) != 1L || !(x$status %in% allowed_status)) {
    stop("`status` must be one of: success, error, skipped.", call. = FALSE)
  }
  if (!is.null(x$error_message) && (!is.character(x$error_message) || length(x$error_message) != 1L)) {
    stop("`error_message` must be NULL or a character scalar.", call. = FALSE)
  }
  if (identical(x$status, "error") && is.null(x$error_message)) {
    stop("`error_message` must be provided when `status` is 'error'.", call. = FALSE)
  }
  if (!identical(x$status, "error") && !is.null(x$error_message)) {
    stop("`error_message` must be NULL unless `status` is 'error'.", call. = FALSE)
  }

  # TODO: When callers migrate, validate caller-specific invariants here.
  invisible(x)
}

#' Print a Standard Preprocessing Result
#'
#' @param x A `pecan_preprocess_result` object.
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @export
print.pecan_preprocess_result <- function(x, ...) {
  validate_standard_result(x)

  cat(sprintf("PEcAn preprocessing result [%s]\n", x$tag))
  cat(sprintf("  status: %s\n", x$status))
  cat(sprintf("  source: %s\n", x$source))
  cat(sprintf("  format: %s\n", x$format))

  if (length(x$paths) == 0L) {
    cat("  paths: <none>\n")
  } else {
    cat(sprintf("  paths: %s\n", paste(x$paths, collapse = ", ")))
  }

  cat(sprintf(
    "  db_ids: input=%s, dbfile=%s\n",
    ifelse(is.na(x$input_id), "NA", as.character(x$input_id)),
    ifelse(is.na(x$dbfile_id), "NA", as.character(x$dbfile_id))
  ))

  if (identical(x$status, "error")) {
    cat(sprintf("  error: %s\n", x$error_message))
  }

  # TODO: Expand printed details only after all preprocessors return this type.
  invisible(x)
}

#' Coerce a Standard Preprocessing Result to a Data Frame
#'
#' @param x A `pecan_preprocess_result` object.
#' @param ... Unused.
#'
#' @return A data frame with one row per path.
#' @export
as.data.frame.pecan_preprocess_result <- function(x, ...) {
  validate_standard_result(x)

  n_paths <- length(x$paths)
  if (n_paths == 0L) {
    path_values <- NA_character_
    n_rows <- 1L
  } else {
    path_values <- x$paths
    n_rows <- n_paths
  }

  # TODO: Revisit row shape if callers need one row per result instead of per path.
  data.frame(
    tag = rep.int(x$tag, n_rows),
    path = path_values,
    input_id = rep.int(x$input_id, n_rows),
    dbfile_id = rep.int(x$dbfile_id, n_rows),
    format = rep.int(x$format, n_rows),
    source = rep.int(x$source, n_rows),
    status = rep.int(x$status, n_rows),
    error_message = rep.int(
      if (is.null(x$error_message)) NA_character_ else x$error_message,
      n_rows
    ),
    stringsAsFactors = FALSE
  )
}