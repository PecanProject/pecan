#' Build URL for output file hosted on THREDDS fileServer 
#'
#' @param workflow_id ID of target workflow (numeric or character)
#' @param target Target path, relative to workflow directory (character)
#' @param hostname THREDDS server hostname (default = "localhost")
#' @param port THREDDS server port (default = 8000)
#' @param https Logical. If `TRUE`, use https, otherwise use http
#'   (default = `FALSE`).
#' @return THREDDS `http` `fileServer` URL (character)
#' @author Alexey Shiklomanov
#' @export
output_url <- function(workflow_id, target,
                       hostname = "localhost",
                       port = 8000,
                       https = FALSE) {
  httpstring <- if (https) "https" else "http"
  port <- as.character(port)
  workflow_id <- as.character(workflow_id)
  prefix_url <- sprintf(
    "%s://%s:%s/thredds/fileServer/outputs/PEcAn_%s",
    httpstring, hostname, port, workflow_id
  )
  file.path(prefix_url, target)
}

#' Download a file from a specific
#'
#' @inheritParams output_url
#' @param target Target file path, relative to output directory of
#'   specific run (as specified by `run_id`)
#' @param run_id Run ID (numeric or character). If `NULL`, try to use
#'   the run listed in the `runs.txt` file. If multiple runs are
#'   available, throw a warning and use the first one.
#' @param ... Additional arguments to [output_url]
#' @return HTTP fileServer URL of a particular run output file (character)
#' @author Alexey Shiklomanov
#' @export
run_url <- function(workflow_id, target, run_id = NULL, ...) {
  if (is.null(run_id)) {
    run_id <- readLines(output_url(workflow_id, "run/runs.txt", ...))
    if (length(run_id) > 1) {
      warning("Multiple runs found. Selecting first run.")
      run_id <- head(run_id, 1)
    }
  }
  new_target <- file.path("out", as.character(run_id), target)
  output_url(workflow_id, new_target, ...)
}
