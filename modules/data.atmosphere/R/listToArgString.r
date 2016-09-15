##' @name listToArgString
##' @title listToArgString
##' @export
##'
##' @param l a named list of function arguments
##' @return A string containing named argument/value pairs separated by commas
##'
##' @author Ryan Kelly
##
## This little utility is used in a few places in data.atmosphere. 
listToArgString <- function(l) {
  arg.string <- ''
  arg.names <- names(l)
  for(i in seq_along(l)) {
    # Quote value if character
    val <- ifelse(.shouldBeQuoted(l[[i]]), paste0("'", l[[i]], "'"), l[[i]])
    name <- ifelse(is.null(arg.names), "", arg.names[i])
    
    if(i>1) {
      arg.string <- paste0(arg.string, ", ")
    }
    if(name == "") {
      arg.string <- paste0(arg.string,  val)
    } else {
      arg.string <- paste0(arg.string, name, "=", val)
    }
  }
  return(arg.string)
}

.shouldBeQuoted <- function(x) {
  return(
    is.character(x) ||
    is.POSIXt(x) || 
    is.Date(x)
  )
}