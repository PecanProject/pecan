#' Create a THREDDS OpenDAP access URL to a PEcAn file
#'
#' @param target Full path to target (character) relative to THREDDS
#'   root. For outputs, this should start with "outputs/", and for
#'   dbfiles, "dbfiles/".
#' @inheritParams thredds_fs_url
#' @return OpenDAP URL to target file (character)
#' @author Alexey Shiklomanov
#' @export
thredds_dap_url <- function(target,
                            hostname = getOption("pecanapi.docker_hostname"),
                            port = getOption("pecanapi.docker_port"),
                            https = getOption("pecanapi.docker_https")) {
  httpstring <- if (https) "https" else "http"
  port <- as.character(port)
  prefix_url <- sprintf("%s://%s:%s/thredds/dodsC", httpstring, hostname, port)
  file.path(prefix_url, target)
}

#' Create a THREDDS OpenDAP access URL to a specific model output file
#'
#' @inheritParams run_url
#' @return OpenDAP URL to target file (character)
#' @author Alexey Shiklomanov
#' @export
run_dap <- function(workflow_id, target, run_id = NULL, ...) {
  if (is.null(run_id)) run_id <- get_run_id(workflow_id, ...)
  new_target <- file.path(
    "outputs",
    paste0("PEcAn_", workflow_id),
    "out",
    run_id,
    target
)
  thredds_dap_url(new_target, ...)
}
