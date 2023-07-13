#' Create a PEcAn MultiSettings object
#'
#' @param ... Settings objects to concatenate
#' @return list with class "Multisettings"
#' @export
#' @author Ryan Kelly
MultiSettings <- function(...) {
  result <- list(...)

  if (length(result) == 1) {
    if (is.MultiSettings(result[[1]])) {
      return(result[[1]])
    } else if (is.list(result[[1]]) && all(sapply(result[[1]], is.Settings))) {
      result <- result[[1]]
    }
  }

  if (!all(sapply(result, is.Settings))) {
    stop(
      "MultiSettings can only be made from Setting,",
      " MultiSettings, or a list of Settings")
  }

  if (length(result) > 0 && is.null(names(result))) {
    names(result) <- paste("settings", seq_along(result), sep = ".")
  }
  class(result) <- c("MultiSettings", class(result))
  return(result)
} # MultiSettings


#' @export
#' @describeIn MultiSettings coerce an existing object to MultiSettings
#' @param x object to test or coerce
as.MultiSettings <- function(x) {
  return(MultiSettings(x))
}

#' @export
#' @describeIn MultiSettings test if an object is a MultiSettings
is.MultiSettings <- function(x) {
  return(inherits(x, "MultiSettings"))
}

#' @export
"[[<-.MultiSettings" <- function(x, value, i, global = TRUE) {
  if (is.character(i)) {
    if (global) {
      value <- replicate(length(x), value, simplify = FALSE)
      x[[i, global = FALSE]] <- value
    } else {
      if (length(x) == length(value)) {
        value <- as.list(value)
        if (is.null(names(value))) {
          names(value) <- rep(i, length(x))
        }
        for (j in seq_along(x)) {
          x[[j]][[i]] <- value[[j]]
        }
      } else if (length(x) == 1 && length(value) > 1) {
        x <- MultiSettings(replicate(length(value), x[[1]], simplify = FALSE))
        x[[i, global = FALSE]] <- value
      } else {
        stop("Length mismatch in assigning to MultiSettings")
      }
    }
    return(x)
  } else {
    if (is.Settings(value) || is.null(value)) {
      NextMethod()
    } else {
      stop("Can only add a Settings object to MultiSettings.")
    }
  }
} # "[[<-.MultiSettings"

#' @export
"$<-.MultiSettings" <- function(x, value, i, global = TRUE) {
  return(`[[<-.MultiSettings`(x, value, i, global))
}

#' @export
"[<-.MultiSettings" <- function(x, value, i) {
  stop("MultiSettings don't support assignments using '['")
}

#' @export
"[[.MultiSettings" <- function(x, i, collapse = TRUE, setAttributes = FALSE) {
  if (is.character(i)) {
    result <- lapply(x, function(y) y[[i]])
    if (collapse && .allListElementsEqual(result)) {
      result <- result[[1]]
      if (setAttributes) {
        attr(result, "settingType") <- "global"
      }
    } else {
      if (setAttributes) {
        attr(result, "settingType") <- "multi"
      }
    }
    return(result)
  } else {
    NextMethod()
  }
} # "[[.MultiSettings"

.allListElementsEqual <- function(x) {
  firstElement <- x[[1]]
  replicatedFirstElement <- replicate(
    length(x),
    firstElement,
    simplify = FALSE)
  return(isTRUE(
    all.equal(replicatedFirstElement, x, check.attributes = FALSE)))
} # .allListElementsEqual

#' @export
"$.MultiSettings" <- function(x, i) {
  return(x[[i]])
}

#' @export
"[.MultiSettings" <- function(x, i) {
  if (is.character(i)) {
    stop("MultiSettings don't support selecting by '[' with character indices")
  } else {
    return(MultiSettings(NextMethod()))
  }
} # "[.MultiSettings"

#' @export
names.MultiSettings <- function(x) {
  return(unique(unlist(lapply(x, names))))
}

#' @export
"names<-.MultiSettings" <- function(x, value) {
  stop("Can't name MultiSettings this way. Use settingNames() instead.")
}

#' function that can retrieve or update the names of multi-settings.
#' 
#' @param multiSettings object for which to retrieve or set the names.
#' @param settingNames  names to be set for the multi-settings object.
#' 
#' @export
settingNames <- function(multiSettings, settingNames) {
  if (missing(settingNames)) {
    return(attr(multiSettings, "names"))
  } else {
    attr(multiSettings, "names") <- settingNames
    return(multiSettings)
  }
} # settingNames

#' @export
print.MultiSettings <- function(x, printAll = FALSE, ...) {
  if (printAll) {
    NextMethod()
  } else {
    print(
      paste0("A MultiSettings object containing ", length(x), " Settings."),
      ...)
  }
}

#' generic function for printing contents of objects.
#'
#' @param x object to be printed.
#'
#' @export
printAll <- function(x) {
  UseMethod("printAll", x)
}

#' @export
printAll.MultiSettings <- function(x) {
  return(print(x, TRUE))
}

.expandableItemsTag <- "multisettings"

#' @export
listToXml.MultiSettings <- function(item, tag, collapse = TRUE) {
  if (collapse && length(item) > 1) {
    if (.expandableItemsTag %in% names(item)) {
      stop("Settings can't contain reserved tag 'multisettings'.")
    }

    tmp <- list()
    expandableItems <- list()
    for (setting in names(item)) {
      value <- item[[setting, setAttributes = TRUE]]
      tmp[[setting]] <- value
      if (attr(value, "settingType") == "multi") {
        expandableItems <- c(expandableItems, setting)
      }
    }
    item <- tmp

    names(expandableItems) <- rep(.expandableItemsTag, length(expandableItems))
    item[[.expandableItemsTag]] <- expandableItems
  }

  NextMethod()
} # listToXml.MultiSettings

#' generic function for expanding multi-settings.
#'
#' @param x object to be expanded.
#'
#' @export
expandMultiSettings <- function(x) {
  UseMethod("expandMultiSettings")
}

#' @export
expandMultiSettings.list <- function(x) {
  if (!(.expandableItemsTag %in% names(x))) {
    return(x)
  } else {
    result <- MultiSettings(Settings(x))
    for (setting in x[[.expandableItemsTag]]) {
      result[[setting, global = FALSE]] <- x[[setting]]
    }

    result[[.expandableItemsTag]] <- NULL

    return(result)
  }
} # expandMultiSettings.list
