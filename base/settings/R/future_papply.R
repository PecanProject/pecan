#' Apply functions to PEcAn MultiSettings (EXPERIMENTAL parallel version)
#'
#' Works like lapply(), but for PEcAn Settings and MultiSettings objects.
#'  The "p" is for "PEcAn".
#'
#' This is an experimental parallel-enabled version of `papply`, released under a
#' new name for testing. If it performs well in testing, the functionality in
#' future_papply() may take over the existing papply() name in a future release.
#'
#' \code{papply} calls a function on each \code{\link{Settings}} object in a
#' \code{\link{MultiSettings}} object, and returns the results in a list.
#' It has some additional features, however:
#'
#' \itemize{
#'   \item If the result of \code{fn} is a \code{Settings} object,
#'         then \code{papply} will coerce the returned list into a new
#'         \code{MultiSettings}.
#'   \item If \code{settings} is a \code{Settings} object,
#'         then \code{papply} knows to call \code{fn} on it directly.
#'   \item By default, \code{papply} will proceed even if \code{fn} throws an
#'         error for one or more of the elements in \code{settings}.
#'         Note that if this option is used, the returned results list will
#'         have entries for \emph{only} those elements that did not
#'         result in an error.
#' }
#'
#' MultiSettings objects are processed using `furrr::future_map`.
#' Components will be run in parallel if your session configures a parallel
#' backend via `future::plan` and in series if it does not.
#'
#' @section Warning & feedback request:
#' This implementation is experimental! If it performs well in testing it may
#' eventually replace the existing `papply` function, but we want to be sure it
#' is a truly drop-in replacement before doing that. Please report your
#' experience with it, good or bad.
#'
#' We are especially interested in reports/patches/suggestions/test cases around
#'
#' * Identifying applications of papply that require serial execution or where
#'   parallel execution may be unwanted
#' * Testing for concurrency issues (esp. in calls that write a lot of files)
#' * Ensuring that console output (print(), logger.info(), etc) is handled
#'   correctly and not lost
#' * Testing against unusually-formatted MultiSettings objects (e.g. anything
#'   that varies a dimension other than site)
#'
#' @param settings A \code{\link{MultiSettings}} or \code{\link{Settings}}
#'   object to operate on
#' @param fn The function to apply to \code{settings}
#' @param stop.on.error Whether to halt execution if a single element in
#'   \code{settings} results in error. See Details.
#' @param ... additional arguments to \code{fn}
#'
#' @return A single \code{fn} return value, or a list of such values
#'   (coerced to \code{MultiSettings} if appropriate; \emph{see Details})
#'
#' @author Chris Black
#' @export
#'
#' @example examples/examples.papply.R
future_papply <- function(settings, fn, ..., stop.on.error = FALSE) {
  if (is.MultiSettings(settings)) {
    catcher <- function(settings_i,
                        idx,
                        fun = fn,
                        len = length(settings),
                        stop_err = stop.on.error) {
      # PEcAn.logger writes to stderr() by default, which is hard to redirect
      # and gets lost easily in parallel contexts. Instead switch to stdout()
      # for duration of this call.
      oc <- PEcAn.logger::logger.setUseConsole(TRUE, FALSE)
      on.exit(
        PEcAn.logger::logger.setUseConsole(oc$console, oc$stderr),
        add = TRUE
      )
      err <- NULL
      wrn <- NULL
      msg <- NULL

      # Call the function with condition handlers. Yes, we need both layers:
      # * tryCatch is for _exiting_ handlers that stop execution of fun() and
      # set `res` to the handler's return value.
      # * withCallingHandlers is for _calling_ handlers that process conditions
      # and then hand control back to fun() so it can finish and set `res` to
      # its own return value.
      res <- tryCatch(
        withCallingHandlers(
          fun(settings_i, ...),
          message = function(m) {
            msg <<- append(msg, m)
            tryInvokeRestart("muffleMessage")
          },
          warning = function(w) {
            wrn <<- append(wrn, w)
            tryInvokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          err <<- append(err, e)
          if (stop_err) {
            PEcAn.logger::logger.error(
              "papply threw an error for element", idx, "of", len,
              ", and is aborting since stop.on.error=TRUE. Message was:",
              sQuote(as.character(err))
            )
            stop()
          }
          NULL
        }
      )

      list(result = res, error = err, warning = wrn, message = msg)
    }

    # Q: Why unclass()? A: Because MultiSettings is a list under the hood but
    # overrides `[[` and `names()` in ways that confuse purrr and friends.
    res_list <- furrr::future_imap(unclass(settings), \(s, i) catcher(s, i))

    result <- purrr::map(res_list, "result")
    errs <- purrr::map(res_list, "error")
    wrns <- purrr::map(res_list, "warning")
    msgs <- purrr::map(res_list, "message")

    has_err <- !purrr::map_lgl(errs, is.null)
    if (any(has_err)) {
      err_i <- which(has_err)
      err_strs <- purrr::map(errs[has_err], \(x)x$message)
      err_strs <- paste0(names(err_strs), ": ", sQuote(err_strs))
      # Q: Shouldn't we check stop.on.error here?
      # A: No, assume it's FALSE bc TRUE should have stopped inside catcher(),
      # and however we got here we want to print the errors anyway.
      PEcAn.logger::logger.warn(
        "papply encountered errors for element(s)", toString(err_i),
        "of", length(settings), ", but continued since stop.on.error=FALSE",
        "(there are no results for these elements, however).",
        "Error(s):", toString(err_strs)
      )
      result <- result[!has_err]
    }

    has_wrn <- !purrr::map_lgl(wrns, is.null)
    if (any(has_wrn)) {
      wrn_i <- which(has_wrn)
      wrn_strs <- purrr::map(wrns[has_wrn], \(x)x$message)
      wrn_strs <- paste0(names(wrn_strs), ": ", sQuote(wrn_strs))
      PEcAn.logger::logger.warn(
        "papply threw warnings for element(s)", toString(wrn_i),
        "of", length(settings), ". Warnings(s):", toString(wrn_strs)
      )
    }

    has_msg <- !purrr::map_lgl(msgs, is.null)
    if (any(has_msg)) {
      msg_i <- which(has_msg)
      msg_strs <- purrr::map(msgs[has_msg], \(x)x$message)
      msg_strs <- paste0(names(msg_strs), ": ", sQuote(msg_strs))
      PEcAn.logger::logger.info(
        "papply got messages from element(s)", toString(msg_i),
        "of", length(settings), ". Messages(s):", toString(msg_strs)
      )
    }

    if (all(sapply(result, is.Settings))) {
      result <- MultiSettings(result)
    }

    return(result)
  } else if (is.Settings(settings)) {
    return(fn(settings, ...))
  } else {
    PEcAn.logger::logger.severe(
      "The function", deparse(substitute(fn)),
      "requires input of type MultiSettings or Settings"
    )
  }
}
