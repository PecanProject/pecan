.utils.logger <- new.env()
.utils.logger$filename <- NA
.utils.logger$console <- TRUE
.utils.logger$stderr <- TRUE
.utils.logger$quit <- FALSE
.utils.logger$level <- 0
.utils.logger$width <- ifelse(getOption("width") < 10,
                              getOption("width"),
                              getOption("width") - 5)

##' Prints a debug message.
##'
##' This function will print a debug message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.debug('variable', 5)
##' }
logger.debug <- function(msg, ...) {
  logger.message("DEBUG", msg, ...)
} # logger.debug


##' Prints an informational message.
##'
##' This function will print an informational message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.info('PEcAn version 1.2')
##' }
logger.info <- function(msg, ...) {
  logger.message("INFO", msg, ...)
} # logger.info


##' Prints a warning message.
##'
##' This function will print a warning message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.warn('detected NA values')
##' }
logger.warn <- function(msg, ...) {
  logger.message("WARN", msg, ...)
} # logger.warn


##' Prints an error message.
##'
##' This function will print an error message.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.error('system did not converge')
##' }
logger.error <- function(msg, ...) {
  logger.message("ERROR", msg, ...)
} # logger.error


##' Prints a severe message and stops execution.
##'
##' This function will print a message and stop execution of the code. This
##' should only be used if the application should terminate.
##'
##' Set \code{\link{logger.setQuitOnSevere}(FALSE)} to avoid terminating
##' the session. The default is to not quit if running interactively.
##'
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @inheritParams logger.message
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.severe('missing parameters')
##' }
logger.severe <- function(msg, ..., wrap = TRUE) {
  logger.message("SEVERE", msg, ...)

  # run option
  error <- getOption("error")
  if (!is.null(error)) {
    eval(error)
  }

  # quit if not interactive, otherwise use stop
  if (.utils.logger$quit) {
    quit(save = "no", status = 1)
  } else {
    stop(paste(msg, ...))
  }
} # logger.severe


##' Prints a message at a certain log level.
##'
##' This function will print a message. This is the function that is responsible
##' for the actual printing of the message.
##'
##' This is a place holder and will be later filled in with a more complex
##' logging set
##'
##' @param level the level of the message (DEBUG, INFO, WARN, ERROR)
##' @param msg the message that should be printed.
##' @param ... any additional text that should be printed.
##' @param wrap Whether or not to wrap long messages (default =
##'   `TRUE`). If `FALSE`, preserve format of original string. Useful
##'   for specifically formatted error messages.
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.message('DEBUG', 'variable', 5)
##' }
logger.message <- function(level, msg, ..., wrap = TRUE) {
  if (logger.getLevelNumber(level) >= .utils.logger$level) {
    calls <- utils::limitedLabels(sys.calls())
    calls <- calls[!grepl("^(#[0-9]+: )?(PEcAn\\.logger::)?logger", calls)]
    calls <- calls[!grepl("(severe|error|warn|info|debug)ifnot", calls)]
    func <- sub("\\(.*", "", utils::tail(calls, 1))
    if (length(func) == 0) {
      func <- "console"
    }

    stamp.text <- sprintf("%s %-6s [%s] :", Sys.time(), level, func)
    args <- sapply(list(...), FUN = toString)
    long.msg <- stringi::stri_trans_general(
      paste(c(msg, args), collapse = " "),
      "latin-ascii"
    )
    if (nchar(long.msg) > 20 && wrap) {
      new.msg <- paste("\n", strwrap(long.msg, width = .utils.logger$width,
                                     indent = 2, exdent = 2), collapse = " ")
    } else {
      new.msg <- long.msg
    }
    text <- paste(stamp.text, new.msg, "\n")

    if (.utils.logger$console) {
      if (.utils.logger$stderr) {
        cat(text, file = stderr())
      } else {
        cat(text, file = stdout())
      }

    }
    if (!is.na(.utils.logger$filename)) {
      cat(text, file = .utils.logger$filename, append = TRUE)
    }
  }
} # logger.message


##' Configure logging level.
##'
##' This will configure the logger level. This allows to turn DEBUG, INFO,
##' WARN, ERROR, and SEVERE messages on and off.
##'
##' Note that this controls _printing_ of messages and does not change other behavior.
##' In particular, suppressing SEVERE by setting the level to "OFF" does not prevent
##' logger.severe() from signaling an error (and terminating the program if 
##' `logger.setQuitOnSevere(TRUE)`).
##'
##' @param level the level of the message. One of "ALL", "DEBUG", "INFO", "WARN",
##' "ERROR", "SEVERE", or "OFF".
##'
##' @export
##' @return When logger level is set, the previous level is returned invisibly.
##'   This can be passed to `logger.setLevel()` to restore the previous level.
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setLevel('DEBUG')
##' 
##' # Temporarily turn logger off
##' old_logger_level <- logger.setLevel("OFF")
##'   # code here
##' logger.setLevel(old_logger_level)
##' }
logger.setLevel <- function(level) {
  original_level <- logger.getLevel()
  .utils.logger$level <- logger.getLevelNumber(level)

  invisible(original_level)
} # logger.setLevel


## Given the string representation this will return the numeric value
##
## Supported levels
##   ALL    = 0
##   DEBUG  = 10
##   INFO   = 20
##   WARN   = 30
##   ERROR  = 40
##   SEVERE = 50
##   OFF    = 60
##
## SEVERE is treated as more serious than ERROR,
## and will terminate the session if `logger.setQuitOnSevere(TRUE)`
## or call stop() otherwise
##
## @return level the level of the message
## @author Rob Kooper
logger.getLevelNumber <- function(level) {
  if (toupper(level) == "ALL") {
    return(0)
  } else if (toupper(level) == "DEBUG") {
    return(10)
  } else if (toupper(level) == "INFO") {
    return(20)
  } else if (toupper(level) == "WARN") {
    return(30)
  } else if (toupper(level) == "ERROR") {
    return(40)
  } else if (toupper(level) == "SEVERE") {
    return(50)
  } else if (toupper(level) == "OFF") {
    return(60)
  } else {
    logger.warn(level, " is not a valid value, setting level to INFO")
    return(logger.getLevelNumber("INFO"))
  }
} # logger.getLevelNumber


##' Get configured logging level.
##'
##' This will return the current level configured of the logging messages.
##' 
##' Note that `logger.setLevel()` invisibly returns current level, so 
##' `logger.getLevel()` is not required to restore the level after a 
##' temporary change.
##'
##' @return A string giving the lowest message level that will be reported, one of 
##' "ALL", "DEBUG", "INFO", "WARN", "ERROR", "SEVERE", or "OFF".
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.getLevel()
##' }
logger.getLevel <- function() {
  if (.utils.logger$level < 10) {
    return("ALL")
  } else if (.utils.logger$level < 20) {
    return("DEBUG")
  } else if (.utils.logger$level < 30) {
    return("INFO")
  } else if (.utils.logger$level < 40) {
    return("WARN")
  } else if (.utils.logger$level < 50) {
    return("ERROR")
  } else if (.utils.logger$level < 60) {
    return("SEVERE")
  } else {
    return("OFF")
  }
} # logger.getLevel


##' Configure logging to console.
##'
##' Should the logging to be printed to the console or not.
##'
##' @param console set to true to print logging to console.
##' @param stderr set to true (default) to log to stderr instead of stdout
##' @return Invisibly, a list of the previously set values of `console`
##'   and `stderr`. This can be used to restore the previous settings after a
##'   temporary change.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setUseConsole(TRUE)
##' }
logger.setUseConsole <- function(console, stderr = TRUE) {
  old <- list(console = .utils.logger$console, stderr = .utils.logger$stderr)
  .utils.logger$console <- console
  .utils.logger$stderr <- stderr

  invisible(old)
} # logger.setUseConsole


##' Configure logging output filename.
##'
##' The name of the file where the logging information should be written to.
##'
##' @param filename the file to send the log messages to
##'  (or NA to not write to file)
##' @return Invisibly, the previously set filename.
##'   This can be used to restore settings after a temporary change.
##' @export
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setOutputFile('pecan.log')
##' }
logger.setOutputFile <- function(filename) {
  old <- .utils.logger$filename
  .utils.logger$filename <- filename

  invisible(old)
} # logger.setOutputFile


##' Configure whether severe should quit.
##'
##' The default is for a non-interactive session to quit.
##' Setting this to false is especially useful for running tests.
##'
##' @param severeQuits should R quit on a severe error.
##' @export
##' @return invisibly, the previous value of `severeQuits`.
##'   This can be used to restore settings after a temporary change.
##' @author Rob Kooper
##' @examples
##' \dontrun{
##' logger.setQuitOnSevere(FALSE)
##' }
logger.setQuitOnSevere <- function(severeQuits) {
  old <- .utils.logger$quit
  .utils.logger$quit <- severeQuits

  invisible(old)
} # logger.setQuitOnSevere


##' Configure the number of chars per line
##' 
##' The default is for 60 chars per line. Setting this to any value will
##' wrap the line when printing a message at that many chars.
##'
##' @param width number of chars to print before wrapping to next line.
##' @return Invisibly, the previously set width.
##'   This can be used to restore settings after a temporary change.
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{
##' logger.setWidth(70)
##' }
logger.setWidth <- function(width) {
  old <- .utils.logger$width
  .utils.logger$width <- width

  invisible(old)
} # logger.setWidth
