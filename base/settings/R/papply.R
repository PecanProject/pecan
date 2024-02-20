#' @title Apply functions to PEcAn MultiSettings
#'
#' @description Works like lapply(),
#'   but for PEcAn Settings and MultiSettings objects
#'
#' @param settings A \code{\link{MultiSettings}}, \code{\link{Settings}},
#'   or \code{\link[base]{list}} to operate on
#' @param fn The function to apply to \code{settings}
#' @param stop.on.error Whether to halt execution if a single element in
#'   \code{settings} results in error. See Details.
#' @param ... additional arguments to \code{fn}
#'
#' @details
#' \code{papply} is mainly used to call a function on each
#' \code{\link{Settings}} object in a \code{\link{MultiSettings}} object,
#' and returning the results in a list.
#' It has some additional features, however:
#'
#' \itemize{
#'   \item If the result of \code{fn} is a \code{Settings} object,
#'         then \code{papply} will coerce the returned list into a new
#'         \code{MultiSettings}.
#'   \item If \code{settings} is a \code{Settings} object,
#'         then \code{papply} knows to call \code{fn} on it directly.
#'   \item If \code{settings} is a generic \code{list},
#'         then \code{papply} coerces it to a \code{Settings} object
#'         and then calls \code{fn} on it directly.
#'         This is meant for backwards compatibility with old-fashioned PEcAn
#'         settings lists, but could have unintended consequences
#'   \item By default, \code{papply} will proceed even if \code{fn} throws an
#'         error for one or more of the elements in \code{settings}.
#'         Note that if this option is used, the returned results list will
#'         have entries for \emph{only} those elements that did not
#'         result in an error.
#' }
#'
#' @return A single \code{fn} return value, or a list of such values
#'   (coerced to \code{MultiSettings} if appropriate; \emph{see Details})
#'
#' @author Ryan Kelly
#' @export
#'
#' @example examples/examples.papply.R
papply <- function(settings, fn, ..., stop.on.error = FALSE) {
  if (is.MultiSettings(settings)) {
    result <- list()
    errors <- character(0)
    for (i in seq_along(settings)) {
      PEcAn.logger::logger.debug(
        "papply executing ", deparse(substitute(fn)),
        "on element ", i, " of ", length(settings), ".")

      tmp = settings[[i]]
      if(all(grepl("settings",names(tmp$run)))) tmp$run = tmp$run[[i]]
      
      result.i <- try(fn(tmp, ...), silent = TRUE)

      if (!inherits(result.i, "try-error")) {
        ind <- length(result) + 1
        if (!is.null(result.i)) {
          result[[ind]] <- result.i
        } else {
          # Have to use special syntax to actually get a null value in
          result[ind] <- list(NULL)
        }
        if (!is.null(settingNames(settings))) {
          names(result)[ind] <- settingNames(settings)[i]
        }
      } else {
        if (stop.on.error) {
          PEcAn.logger::logger.error(
            "papply threw an error for element ", i, " of ", length(settings),
            ", and is aborting since stop.on.error=TRUE. Message was: '",
            as.character(result.i), "'")
          stop()
        } else {
          warning.message.i <- paste0(
            "papply threw an error for element ", i, " of ", length(settings),
            ", but is continuing since stop.on.error=FALSE",
            " (there will be no results for this element, however).",
            " Message was: '", as.character(result.i), "'")
          PEcAn.logger::logger.warn(warning.message.i)
          errors <- c(
            errors,
            paste0("Element ", i, ": '", as.character(result.i), "'"))
        }
      }
    }

    if (all(sapply(result, is.Settings))) {
      result <- MultiSettings(result)
    }

    if (length(errors) > 0) {
      PEcAn.logger::logger.warn(
        "papply encountered errors for ", length(errors), " elements, ",
        "but continued since stop.on.error=FALSE.",
        paste(errors, collapse = "; "))
    }

    return(result)
  } else if (is.Settings(settings)) {
    return(fn(settings, ...))
  } else if (is.list((settings))) {
    # Assume it's settings list that hasn't been coerced to Settings class...
    return(fn(as.Settings(settings), ...))
  } else {
    PEcAn.logger::logger.severe(
      "The function", fn, "requires input of type MultiSettings or Settings")
  }
} # papply
