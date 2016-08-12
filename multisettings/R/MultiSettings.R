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
  result <- list()
  
  for(i in seq_along(args)) {
    arg.i = args[[i]]
    if(is.Settings(arg.i) || is.MultiSettings(arg.i) || 
       (is.list(arg.i) && all(sapply(arg.i, is.Settings)))) {
      browser()
      result <- c(result, arg.i)
    } else {
      stop("MultiSettings can only be made from Setting, MultiSettings, or a list of Settings")
    }
  }

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
"[[<-.MultiSettings" <- function(x, value, i, global=TRUE) {
  if(is.character(i)) {
    for(j in seq_along(x)) {
      x[[j]][[i]] <- value
    }
    return(x)
  } else if(is.numeric(i)) {
    if(is.Settings(value)) {
      NextMethod()
    } else {
      stop("Can only add a Settings object to MultiSettings.")
    }
  } else {
    stop("Invalid indexing to MultiSettings object")
  }
}

##' @export
"$<-.MultiSettings" <- function(x, value, i) {
  if(is.character(i)) {
    x[[i]] <- value
    return(x)
  } else {
    stop("Invalid indexing to MultiSettings object")
  }
}


##' @export
"[<-.MultiSettings" <- function(x, value, i) {
  if(is.numeric(i)) {
    if(is.Settings(value)) {
      for(j in seq_along(i)) {
        x[[i[j]]] <- value
      }
    } else if(is.MultiSettings(value)) {
      if(length(value) == length(i)) {
        for(j in seq_along(i)) {
          x[[i[j]]] <- value[[j]]
        }
      } else {
        stop("Length mismatch when assigning to MultiSettings")
      }
    } else {
      stop("Can only add Settings objets to MultiSettings")
    }
    return(x)
  } else {
    stop("Invalid indexing to MultiSettings object")
  }
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

# print.MultiSettings <- function(x, printAll=FALSE, ...) {
#   if(printAll) {
#     NextMethod()
#   } else {
#     print(paste0("A MultiSettings object containing ", length(x), " Settings."), ...)
#   }
# }

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