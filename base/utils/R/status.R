#------------------------------------------------------------------------------
# Functions used to write STATUS used by history
#------------------------------------------------------------------------------

#' PEcAn workflow status tracking
#'
#' Records the progress of a PEcAn workflow by writing statuses and timestamps
#'   to a STATUS file. Use these each time a module starts, finishes,
#'   or is skipped.

#' @details
#' All of these functions write to or read from a STATUS file in your run's
#'   output directory. If the file is not specified in the call, they will look
#'   for a `settings` object in the global environment and use
#'   `<settings$outdir>/STATUS` if possible.
#'
#' Since the status functions may be called inside error-handling routines,
#'  it's important that they not produce new errors of their own. Therefore
#'  if the output file doesn't exist or is not writable, rather than complain
#'  the writer functions (`status.start`, `status.end`, `status.skip`) will
#'  print to the console and `status.check` will simply return 0.
#'
#' @name status
#' @author Rob Kooper
#' @param name one-word description of the module being checked or recorded,
#'  e.g. "TRAIT", "MODEL", "ENSEMBLE"
#' @param status one-word summary of the module result, e.g. "DONE", "ERROR"
#' @param file path to status file.
#'   If NULL, taken from `settings` (see details)
#' @return For `status.start`, `status.end`, and `status.skip`: NULL, invisibly
#'
NULL

#' @describeIn status Record module start time
#' @export
status.start <- function(name, file = NULL) {
  file <- get_status_path(file)
  cat(
    paste(name, format(Sys.time(), "%F %T"), sep = "\t"),
    file = file,
    append = TRUE)
}

#' @describeIn status Record module completion time and status
#' @export
status.end <- function(status = "DONE", file = NULL) {
  file <- get_status_path(file)
  cat(
    paste("", format(Sys.time(), "%F %T"), status, "\n", sep = "\t"),
    file = file,
    append = TRUE)
}

#' @describeIn status Record that module was skipped
#' @export
status.skip <- function(name, file = NULL) {
  file <- get_status_path(file)
  cat(
    paste(
      name,
      format(Sys.time(), "%F %T"), "",
      format(Sys.time(), "%F %T"),
      "SKIPPED", "\n", sep = "\t"),
    file = file,
    append = TRUE)
}

#' @describeIn status Look up module status from file
#' @return For `status.check`, an integer:
#'   0 if module not run, 1 if done, -1 if error
#' @export
status.check <- function(name, file = NULL) {
  file <- get_status_path(file)
  if (!file.exists(file)) {
    return(0L)
  }
  status_data <- utils::read.table(
    file, row.names = 1, header = FALSE,
    sep = "\t", quote = "", fill = TRUE)
  if (!name %in% row.names(status_data)) {
    return(0L)
  }
  status_data[name, ]
  if (is.na(status_data[name, 3])) {
    PEcAn.logger::logger.warn("UNKNOWN STATUS FOR", name)
    return(0L)
  }
  if (status_data[name, 3] == "DONE") {
    return(1L)
  }
  if (status_data[name, 3] == "ERROR") {
    return(-1L)
  }
  return(0L)
}

# Verify user-provided output path, or if null try to read it from a
#   `settings` object visible in the scope where status.* was called
# Example:
# ```
#   settings <- list(outdir = "foo")
#   status.start("outer")
#   f <- function() { settings$outdir <- "bar"; status.start("inner") }
#   f()
# ```
# writes "outer" to a file named `foo/STATUS` and "inner" to `bar/STATUS`.
get_status_path <- function(file) {
  if (!is.null(file)) {
    if (dir.exists(file)) {
      dir <- file
      base <- "STATUS"
    } else {
      dir <- dirname(file)
      base <- basename(file)
    }
  } else {
    dir <- get0(
      x = "settings",
      envir = parent.frame(2),
      inherits = TRUE,
      ifnotfound = list())$outdir
    base <- "STATUS"
  }

  if (!is.null(dir) && dir.exists(dir)) {
    return(file.path(dir, base))
  } else {
    # cat treats empty path as "write to stdout"
    return("")
  }
}