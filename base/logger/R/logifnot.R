#' Logger message if conditions are not met
#'
#' Similar to [base::stopifnot], but allows you to use a custom message and 
#' logger level. If all conditions are `TRUE`, silently exit.
#'
#' Conditions can be vectorized, or can return non-logical values.The 
#' underlying function automatically applies `isTRUE(all(.))` to the 
#' conditions.
#'
#' @param msg Logger message to write, as a single character string.
#' @param ... Conditions to evaluate 
#' @return Invisibly, `TRUE` if conditions are met, `FALSE` otherwise
#' @examples
#' a <- 1:5
#' b <- list(6, 7, 8)
#' debugifnot("By the way, something is not a list.", is.list(a), is.list(b))
#' infoifnot("Something is not a list.", is.list(a), is.list(b))
#' warnifnot("I would prefer it if you used lists.", is.list(a), is.list(b))
#' errorifnot("You should definitely use lists.", is.list(a), is.list(b))
#' try({
#'   severeifnot("I absolutely cannot deal with the fact that something is not a list.", is.list(a), is.list(b))
#' })
#' @export
severeifnot <- function(msg, ...) {
  if (!check_conditions(...)) {
    PEcAn.logger::logger.severe(msg)
  } else {
    invisible(TRUE)
  }
}

#' @rdname severeifnot
#' @export
errorifnot <- function(msg, ...) {
  if (!check_conditions(...)) {
    PEcAn.logger::logger.error(msg)
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' @rdname severeifnot
#' @export
warnifnot <- function(msg, ...) {
  if (!check_conditions(...)) {
    PEcAn.logger::logger.warn(msg)
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' @rdname severeifnot
#' @export
infoifnot <- function(msg, ...) {
  if (!check_conditions(...)) {
    PEcAn.logger::logger.info(msg)
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' @rdname severeifnot
#' @export
debugifnot <- function(msg, ...) {
  if (!check_conditions(...)) {
    PEcAn.logger::logger.debug(msg)
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' Check a list of conditions
check_conditions <- function(...) {
  dots <- list(...)
  conditions <- vapply(dots, is_definitely_true, logical(1))
  all(conditions)
}

#' Robust logical check
is_definitely_true <- function(x) {
  if (is.null(x) || length(x) == 0 || !is.logical(x)) {
    return(FALSE)
  }
  isTRUE(all(x))
}
