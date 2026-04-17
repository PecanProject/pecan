#' Validate PEcAn events JSON against schema v0.1.0
#'
#' Validates a PEcAn events JSON file (single-site object or an array of site
#' objects) against the bundled JSON Schema (draft 2020-12) using the AJV
#' engine.
#'
#' - Logs an error and returns FALSE if the JSON file does not exist or does
#'   not conform to the schema.
#' - Logs a warning and returns TRUE if the optional package `jsonvalidate` is
#'   not installed, so calling code can proceed without a hard dependency.
#'
#' @param events_json character. Path to the JSON file to validate.
#' @param verbose logical. When `TRUE`, include detailed AJV messages on error.
#' @param max_errs integer. Print only this many validation errors.
#'  To see the rest, use the `errors` attribute of the return value.
#'
#' @return Logical TRUE if valid. If invalid, FALSE with an attribute "errors"
#'  containing a dataframe of reported problems.
#' NA if validator unavailable.
#'
#' @author David LeBauer
#'
#' @examples
#' # validate_events_json(system.file("events_fixtures/events_site1.json",
#' #                                package = "PEcAn.data.land"))
#'
#' @export
validate_events_json <- function(events_json, verbose = TRUE, max_errs = 50) {
  if (!file.exists(events_json)) {
    PEcAn.logger::logger.error(glue::glue("events_json file does not exist: {events_json}"))
    return(FALSE)
  }

  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    PEcAn.logger::logger.warn("Skipping events schema validation: package 'jsonvalidate' not installed.")
    return(NA)
  }

  schema <- system.file("events_schema_v0.1.0.json", package = "PEcAn.data.land", mustWork = TRUE)
  ok <- jsonvalidate::json_validate(events_json, schema = schema, engine = "ajv", verbose = verbose, error = FALSE)
  if (isTRUE(ok)) {
    PEcAn.logger::logger.info(glue::glue("events_json file is valid: {events_json}"))
    return(TRUE)
  }

  errs <- attr(ok, "errors")
  detail <- if (is.null(errs)) {
    "<no details>"
  } else {
    errs <- utils::head(errs, max_errs)
    paste(sprintf(
      "%s: %s",
      ifelse(nzchar(errs$instancePath), errs$instancePath, "<root>"), errs$message
    ), collapse = "; ")
  }
  PEcAn.logger::logger.error(glue::glue("events_json does not conform to schema: {events_json}; {detail}"))

  ok
}
