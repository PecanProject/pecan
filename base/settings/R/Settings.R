#' Create a PEcAn Settings object
#'
#' @title Create a PEcAn Settings object
#' @param ... objects to concatenate
#' @return a list containing all objects in `...`,
#'   with class c("Settings", "SafeList", "list").
#' @export
#' @author Ryan Kelly
Settings <- function(...) {
  args <- list(...)
  if (length(args) == 1 && inherits(args[[1]], "Settings")) {
    return(args[[1]])
  }

  result <- SafeList(...)
  class(result) <- c("Settings", class(result))
  return(result)
}

#' @export
#' @describeIn Settings coerce an object to Settings
#' @param x object to test or coerce
as.Settings <- function(x) {
  return(Settings(x))
}

#' @export
#' @describeIn Settings test if object is already a Settings
is.Settings <- function(x) {
  return(inherits(x, "Settings"))
}
