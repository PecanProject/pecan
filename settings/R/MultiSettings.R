##' Create a PEcAn MultiSettings object
##'
##' 
##' @title Create a PEcAn MultiSettings object
##' @param 
##' @return
##' @export
##' @author Ryan Kelly
MultiSettings <- function(...) {
  args <- list(...)
  if(length(args)==1) {
    if(is(args[[1]], "MultiSettings")) {
      return(args[[1]])
    }
    if(is.list(args[[1]]) && all(sapply(args[[1]], is.Settings))) {
      args <- args[[1]]
    }
  }
  
  if(!all(sapply(args, is.Settings))) {
    stop("Can't create MultiSettings from anything other than Settings")
    # Could allow the constructor to take an arbitrary list of lists and convert each to Settings.
    # But probably no one will want to do that any time soon, and this sanity-check is safer. 
  }
  
  result <- args
  names(result) <- paste("settings", seq_along(result), sep=".")
  class(result) <- c("MultiSettings", class(result))
  return(result)
}


##' @export
##' @describeIn 
as.MultiSettings <- function(x) {
  return(MultiSettings(x))
}

##' @export
is.MultiSettings <- function(x) {
  return(is(x, "MultiSettings"))
}

##' @export
"[[<-.MultiSettings" <- function(x, value, i) {
  if(is.character(i) || is.null(i)) {
    x.list <- x
    class(x.list) <- "list"
    for(j in seq_along(x.list)) {
      x.list[[j]][[i]] <- value
    }
    return(MultiSettings(x.list))
  } else {
    stop("Can't add elements to a MultiSettings using numeric indexing")
  }
}

##' @export
"$<-.MultiSettings" <- function(x, value, i) {
  x[[i]] <- value
  return(x)
}


##' @export
"[<-.MultiSettings" <- function(x, value, i) {
  stop("Can't add elements to a MultiSettings. Use MultiSettings() to create a new one.")
}


##' @export
"[[.MultiSettings" <- function(x, i) {
  if(is.character(i)) {
    vals <- lapply(x, function(y) y[[i]])
    if(!all(sapply(vals, function(y, y1) identical(y, y1), vals[[1]]))) {
      stop(paste("Tried to get", i, "by name from a MultiSettings, but the value varies across settings"))
    }
    return(vals[[1]])
  } else {
    NextMethod()
  }
}

##' @export
"$.MultiSettings" <- function(x, i) {
  return(x[[i]])
}


##' @export
"[.MultiSettings" <- function(x, i) {
  if(is.character(i))
    stop("Selecting from MultiSettings using single brackets and character indices is not allowed")
  MultiSettings(NextMethod())
}

##' @export
names.MultiSettings <- function(x) {
  for(i in seq_along(x)) {
    if(i == 1) {
      result <- names(x[[i]])
    } else {
      result <- intersect(result, names(x[[i]])) 
    }
  }
  return(result)
}

##' @export
"names<-.MultiSettings" <- function(x, value) {
  stop("Can't name elements of MultiSettings.")
}

# 
# ##' @export
# listToXml.MultiSettings <- function(item, tag, collapse=TRUE) {
#   if(collapse) {
#     collapsedItem <- list()
#     for(i in seq_along(item)) {
#       collapsedItem <- 
#     }
#   } else {
#     NextMethod()
#   }
# }