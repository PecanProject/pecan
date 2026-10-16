#' Preprocessing Result Constructor
#'
#' Uniform return type for all preprocessing pipeline functions.
#' Returns a named list that can be inserted directly into a PEcAn settings object.
#' Disk-only for now; memory-primary path can be added later without breaking
#' the interface.
#'
#' @param file Character. Path to the generated file.
#' @param format Character. Format identifier (e.g., "CFmet", "ED2met").
#' @param mimetype Character. MIME type of the file.
#' @param dbfile.id Integer. BETY dbfile ID, if available. NULL if dbparms = NULL.
#' @param start_date POSIXct. Start date of the data period.
#' @param end_date POSIXct. End date of the data period.
#' @param source Character. Data source identifier (e.g., "Ameriflux", "CRUNCEP").
#'
#' @return A named list with class attribute "preprocess_result" for potential
#'   future subclassing, but behaves as a plain list for now.
#'
#' @examples
#' result <- preprocess_result(
#'   file = "/data/flux/US-NR1.2004.nc",
#'   format = "CFmet",
#'   mimetype = "application/x-netcdf",
#'   dbfile.id = 12345,
#'   start_date = as.POSIXct("2004-01-01"),
#'   end_date = as.POSIXct("2004-12-31"),
#'   source = "Ameriflux"
#' )
#'
#' @md
#' @export
preprocess_result <- function(file, format, mimetype, dbfile.id = NULL,
                              start_date = NULL, end_date = NULL, source = NULL) {
  # Validation - using PEcAn.logger::logger.severe() for consistency with codebase
  if (!is.character(file) || length(file) != 1) {
    PEcAn.logger::logger.severe("file must be a single character path")
  }
  if (!file.exists(file)) {
    PEcAn.logger::logger.warn("file does not exist:", file)
  }
  if (!is.character(format) || length(format) != 1) {
    PEcAn.logger::logger.severe("format must be a single character string")
  }
  if (!is.character(mimetype) || length(mimetype) != 1) {
    PEcAn.logger::logger.severe("mimetype must be a single character string")
  }
  if (!is.null(dbfile.id) && (!is.numeric(dbfile.id) || length(dbfile.id) != 1)) {
    PEcAn.logger::logger.severe("dbfile.id must be a single integer or NULL")
  }

  result <- list(
    file = file,
    format = format,
    mimetype = mimetype,
    dbfile.id = dbfile.id,
    start_date = start_date,
    end_date = end_date,
    source = source
  )

  # Class attribute for future subclassing, but no S3 methods yet
  class(result) <- c("preprocess_result", "list")

  result
}

#' Insert Preprocess Result into Settings
#'
#' Takes a preprocess_result (or list of them) and patches it into the
#' appropriate location in a PEcAn settings object.
#'
#' @param settings List. PEcAn settings object.
#' @param result preprocess_result or list of preprocess_result.
#' @param input.type Character. Input type identifier, e.g., "met", "soil".
#'
#' @return Updated settings object.
#'
#' @md
#' @export
insert_preprocess_result <- function(settings, result, input.type) {
  if (!is.list(result)) {
    PEcAn.logger::logger.severe("result must be a list or preprocess_result")
  }

  # Handle single result or list of results
  if (inherits(result, "preprocess_result")) {
    result <- list(result)
  }

  # Ensure path exists in settings
  if (is.null(settings$run$inputs)) {
    settings$run$inputs <- list()
  }
  if (is.null(settings$run$inputs[[input.type]])) {
    settings$run$inputs[[input.type]] <- list()
  }

  # Insert results
  settings$run$inputs[[input.type]]$path <- sapply(result, `[[`, "file")
  settings$run$inputs[[input.type]]$format <- result[[1]]$format
  settings$run$inputs[[input.type]]$mimetype <- result[[1]]$mimetype

  if (!is.null(result[[1]]$dbfile.id)) {
    settings$run$inputs[[input.type]]$dbfile.id <- sapply(result, `[[`, "dbfile.id")
  }

  settings
}

#' Validate Preprocess Result
#'
#' Check that a preprocess_result has all required fields and valid values.
#' Useful for parity tests and pipeline validation.
#'
#' @param result List. Object to validate.
#' @param require.file.exists Logical. Whether to check file existence.
#'
#' @return Logical. TRUE if valid, throws error otherwise.
#'
#' @md
#' @export
validate_preprocess_result <- function(result, require.file.exists = TRUE) {
  required_fields <- c("file", "format", "mimetype")

  for (field in required_fields) {
    if (is.null(result[[field]])) {
      PEcAn.logger::logger.severe("Missing required field:", field)
    }
  }

  if (!is.character(result$file) || length(result$file) != 1) {
    PEcAn.logger::logger.severe("file must be a single character path")
  }

  if (require.file.exists && !file.exists(result$file)) {
    PEcAn.logger::logger.severe("file does not exist:", result$file)
  }

  TRUE
}
