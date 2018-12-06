#' Build URL for output file hosted on THREDDS fileServer 
#'
#' @param workflow_id ID of target workflow (numeric or character)
#' @param target Target path, relative to workflow directory (character)
#' @param ... Additional arguments to [thredds_fs_url]
#' @return THREDDS http fileServer URL (character)
#' @author Alexey Shiklomanov
#' @export
output_url <- function(workflow_id, target, ...) {
  workflow_id <- as.character(workflow_id)
  prefix_url <- sprintf("%s/outputs/PEcAn_%s", thredds_fs_url(...), workflow_id)
  file.path(prefix_url, target)
}

#' Build a THREDDS fileServer URL for a specific output file from a
#' specific run
#'
#' @inheritParams output_url
#' @param target Target file path, relative to output directory of
#'   specific run (as specified by `run_id`)
#' @param run_id Run ID (numeric or character). If `NULL`, try to use
#'   the run listed in the `runs.txt` file. If multiple runs are
#'   available, throw a warning and use the first one.
#' @param ... Additional arguments to [thredds_fs_url]
#' @return HTTP fileServer URL of a particular run output file (character)
#' @author Alexey Shiklomanov
#' @export
run_url <- function(workflow_id, target, run_id = NULL, ...) {
  if (is.null(run_id)) run_id <- get_run_id(workflow_id, ...)
  new_target <- file.path("out", as.character(run_id), target)
  output_url(workflow_id, new_target, ...)
}

#' Get a run ID from the `runs.txt` file
#'
#' @inheritParams output_url
#' @return A single output ID (integer64)
#' @author Alexey Shiklomanov
get_run_id <- function(workflow_id, ...) {
  run_id <- bit64::as.integer64(readLines(output_url(workflow_id, "run/runs.txt", ...)))
  if (length(run_id) > 1) {
    warning("Multiple runs found. Selecting first run.")
    run_id <- head(run_id, 1)
  }
  run_id
}

#' Build a THREDDS fileServer URL for a specific dbfile
#'
#' @param target Target file path, relative to `dbfiles` root folder (character)
#' @param ... Additional arguments to [thredds_fs_url]
#' @return THREDDS HTTP fileServer URL to dbfile (character)
#' @author Alexey Shiklomanov
#' @export
dbfile_url <- function(target, ...) {
  file.path(thredds_fs_url(...), "dbfiles", target)
}

#' Create a THREDDS fileServer URL prefix
#'
#' @param hostname THREDDS server hostname (default = "localhost")
#' @param port THREDDS server port (default = 8000)
#' @param https Logical. If `TRUE`, use https, otherwise use http
#'   (default = `FALSE`).
#' @return THREDDS fileServer URL prefix (character)
#' @author Alexey Shiklomanov
thredds_fs_url <- function(hostname = getOption("pecanapi.docker_hostname"),
                           port = getOption("pecanapi.docker_port"),
                           https = getOption("pecanapi.docker_https")) {
  httpstring <- if (https) "https" else "http"
  port <- as.character(port)
  sprintf(
    "%s://%s:%s/thredds/fileServer",
    httpstring, hostname, port
  )
}
