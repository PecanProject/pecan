##' Create a PEcAn MultiSettings object
##'
##' 
##' @title Create a PEcAn MultiSettings object
##' @param 
##' @return
##' @export
##' @author Ryan Kelly
MultiSettings <- function(...) {
  result <- list(...)

  if(length(result) == 1) {
    if(is.MultiSettings(result[[1]])) {
      return(result[[1]])
    } else if(is.list(result[[1]]) && all(sapply(result[[1]], is.Settings))) {
      result <- result[[1]]
    }
  }
  
  if(!all(sapply(result, is.Settings))) {
    stop("MultiSettings can only be made from Setting, MultiSettings, or a list of Settings")
  }

  if(length(result) > 0)
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
    if(global) {
      value <- replicate(length(x), value, simplify=FALSE)
      x[[i, global=F]] <- value
    } else {
      if(length(x) == length(value)) {
        value <- as.list(value)
        for(j in seq_along(x)) {
          x[[j]][[i]] <- value[[j]]
        }
      } else if(length(x) == 1 && length(value) > 1) {
        x <- MultiSettings(replicate(length(value), x[[1]], simplify=FALSE))
        x[[i, global=FALSE]] <- value
      } else {
        stop("Length mismatch in assigning to MultiSettings")
      }
    }
    return(x)
  } else {
    if(is.Settings(value) || is.null(value)) {
      NextMethod()
    } else {
      stop("Can only add a Settings object to MultiSettings.")
    }
  }
}

##' @export
"$<-.MultiSettings" <- function(x, value, i, global=TRUE) {
  return("[[<-.MultiSettings"(x, value, i, global))
}


##' @export
"[<-.MultiSettings" <- function(x, value, i) {
  stop("MultiSettings don't support assignments using '['")
}


##' @export
"[[.MultiSettings" <- function(x, i, collapse=TRUE) {
  if(is.character(i)) {
    result <- lapply(x, function(y) y[[i]])
    if(collapse && .allListElementsEqual(result)) {
      return(result[[1]])
    } else {
      return(result)
    }
  } else {
    NextMethod()
  }
}

.allListElementsEqual <- function(x) {
  firstElement <- x[[1]]
  replicatedFirstElement <- replicate(length(x), firstElement, simplify=FALSE)
  return(isTRUE(all.equal(replicatedFirstElement, x, check.attributes=FALSE)))
}

##' @export
"$.MultiSettings" <- function(x, i) {
  return(x[[i]])
}


##' @export
"[.MultiSettings" <- function(x, i) {
  if(is.character(i)) {
    stop("MultiSettings don't support selecting by '[' with character indices")
  } else {
    return(MultiSettings(NextMethod()))
  }
}

##' @export
names.MultiSettings <- function(x) {
  return(unique(unlist(lapply(x, names))))
}

##' @export
"names<-.MultiSettings" <- function(x, value) {
  stop("Can't name elements of MultiSettings.")
}

print.MultiSettings <- function(x, printAll=FALSE, ...) {
  if(printAll) {
    NextMethod()
  } else {
    print(paste0("A MultiSettings object containing ", length(x), " Settings."), ...)
  }
}




##' @export
listToXml.MultiSettings <- function(item, tag="pecan.multi", collapse=TRUE) {
  if(collapse && length(item) > 1) {
    expandableItemsTag <- "multisettings"
    if(expandableItemsTag %in% names(item)) {
      stop("Settings can't contain reserved tag 'multisettings'.")
    }
    
    tmp <- list()
    expandableItems <- character(0)
    for(setting in names(item)) {
      if(.allListElementsEqual(item[[setting]])) {
        tmp[[setting]] <- item[[setting]][[1]]
        expandableItems <- c(expandableItems, setting)
      } else {
        tmp[[setting]] <- item[[setting]]
      }
    }
    item <- tmp

    item[[expandableItemsTag]] <- expandableItems
  }
  
  NextMethod(tag=tag)
}


