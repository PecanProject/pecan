##' @name listToArgString
##' @title listToArgString
##' @export
##'
##' @param l a named list of function arguments
##' @return A string containing named argument/value pairs separated by commas
##'
##' @author Ryan Kelly
## This little utility is used in a few places in data.atmosphere.
listToArgString <- function(l) {
  arg.string <- ""
  arg.names  <- names(l)
  
  for (i in seq_along(l)) {
    # Quote value if character
    val  <- .parseArg(l[[i]])
    name <- ifelse(is.null(arg.names), "", arg.names[i])
    
    if (i > 1) {
      arg.string <- paste0(arg.string, ", ")
    }
    
    if (name == "") {
      arg.string <- paste0(arg.string, val)
    } else {
      arg.string <- paste0(arg.string, name, "=", val)
    }
  }
  return(arg.string)
} # listToArgString

.parseArg <- function(x) {
  if (is.character(x) || lubridate::is.POSIXt(x) || lubridate::is.Date(x)) {
    return(paste0("'", x, "'"))
  } else if (is.null(x)) {
    return("NULL")
  } else if(is.data.frame(x)){
    # note that this will treat everything as characters
    foo <- sapply(1:ncol(x), function(v) paste(colnames(x)[v],
                                               "=c('" ,
                                               paste(x[,v], collapse = "','"),
                                               "')"))
    foobar <- paste0("data.frame(", paste(foo, collapse = ","), ")")
    return(foobar)
  }else if(is.list(x)){ 
    # note that this will not handle sublist names
    foo <- toString(x)
    foobar <- paste0("list(", foo, ")")
    return(foobar)
  }else {
    return(x)
  }
} # .parseArg
