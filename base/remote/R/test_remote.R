#' Test remote execution
#'
#' @inheritParams remote.execute.cmd
#' 
#' @param ... additional arguments.
#'
#' @return `TRUE` is remote execution is successful.
#' If unsuccessful, depends on the value of `stderr`.
#' If `stderr` = TRUE (default), this function will throw an error.
#' If `stderr` = FALSE, this function will print a logger error and return FALSE.
#' @export
#'
#' @examples
#' # Localhost execution should always work
#' good_host <- list(name = "localhost")
#' test_remote(good_host)
#'
#' bad_host <- list(name = "bigbadwolf")
#' if (!test_remote(bad_host, stderr = FALSE)) {
#'   print("Big Bad Wolf is a bad host.")
#' }
test_remote <- function(host, stderr = TRUE, ...) {
  dots <- list(...)
  cmd <- "echo"
  test_string <- paste("Testing remote", host$name)
  if (!is.null(dots$args)) {
    args <- c(test_string, dots$args)
  } else {
    args <- test_string
  }
  out <- remote.execute.cmd(host, cmd, args = args, stderr = stderr)

  if (length(out) > 0 && out == test_string) {
    return(TRUE)
  } else {
    msg <- paste("Error in remote execution. Here is the remote output:\n", paste(out, collapse = '\n'))
    if (stderr) {
      PEcAn.logger::logger.severe(msg)
    } else {
      PEcAn.logger::logger.error(msg)
      return(FALSE)
    }
  }
}
