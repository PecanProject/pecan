#' Create a SafeList object
#'
#' `SafeList` is a wrapper class for the normal R list. It should behave
#' identically, except for the `$` operator being overridden to require exact
#' matches.
#'
#' The constructor works identical to `list()` unless:
#'
#' 1) The only argument is a list, in which case the result is the same list,
#'    with its class attribute updated to include 'SafeList', or
#' 2) The only argument is a SafeList, in which case that argument is returned
#'    unchanged
#'
#' @param ... A list to upgrade to SafeList, or elements to be added to a new
#'   SafeList
#' @return The resulting SafeList
#' @export
#' @author Ryan Kelly
SafeList <- function(...) {
  result <- list(...)
  if (length(result) == 1) {
    if (inherits(result[[1]], "SafeList")) {
      return(result[[1]])
    } else if (is.list(result[[1]])) {
      result <- result[[1]]
    }
  }
  class(result) <- c("SafeList", class(result))
  return(result)
} # SafeList


#' @export
#' @describeIn SafeList Coerce an object to SafeList.
#' @param x list object to be tested or coerced
#' @return a SafeList version of x
as.SafeList <- function(x) {
  return(SafeList(x))
} # as.SafeList


#' @export
#' @describeIn SafeList Test if object is already a SafeList.
#' @return logical
is.SafeList <- function(x) {
  inherits(x, "SafeList")
} # is.SafeList


#' Extract SafeList component by name
#'
#' Overrides `$.list`, and works just like it except forces exact match
#' (i.e., makes `x$name` behave exactly like `x[[name, exact=T]]`)
#'
#' @title Extract SafeList component by name
#' @param x the SafeList object
#' @param name the name of the component
#' @return The specified component
#' @export
#' @author Ryan Kelly
"$.SafeList" <- function(x, name) {
  return(x[[name, exact = TRUE]])
} # "$.SafeList"
