#' format a list of arguments as one comma-separated string
#'
#' @export
#'
#' @param l a named list of function arguments
#' @return A string containing named argument/value pairs separated by commas
#'
#' @author Ryan Kelly
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
    # does your list have sublist
    has_sub_list <- sapply(x,is.list)
    if(any(has_sub_list)){
      foo.string <- list()
      for(r in seq_along(has_sub_list)){
        if(has_sub_list[r]){
          foonames <- names(x[[r]])
          foobar <- unlist(x[[r]])
          tmp <-sapply(seq_along(x[[r]]), function(pas) paste0(foonames[pas], "='", foobar[pas], "'"))
          foo <- paste0(names(x)[r],"=list(",toString(tmp),")")
          foo.string[[r]] <- foo
        }else{ # this doesn't take care of everything
          foo.string[[r]] <- paste0(names(x)[r], "='", x[[r]],"'")
        }
      }
      val <- paste0("list(",toString(foo.string),")")
      return(val)
     
    }else{
      foonames <- names(x)
      foobar <- unlist(x)
      tmp <-sapply(seq_along(x), function(pas) paste0(foonames[pas], "='", foobar[pas], "'"))
      foo <- paste0("list(",toString(tmp),")")
      return(foo)
    }

  }else {
    return(x)
  }
} # .parseArg
