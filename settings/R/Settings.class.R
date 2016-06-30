##' Extract Settings component by name
##'
##' Overrides `$`.list, and works just like it except forces exact match 
##' (i.e., makes x$name behave exactly like x[[name, exact=T]])
##' 
##' @title Extract Settings component by name
##' @return The specified component
##' @param x the Settings object
##' @param name the name of the component
##' @export
##' @author Ryan Kelly
"$.Settings" <- function(x, name) {
  return(x[[name, exact=TRUE]])
}
