#----------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## #-------------------------------------------------------------------------------
##' Create a PEcAn Settings object
##'
##' 
##' @title Create a PEcAn Settings object
##' @param 
##' @return
##' @export
##' @author Ryan Kelly
Settings <- function(...) {
  args <- list(...)
  if(length(args)==1 && is(args[[1]], "Settings")) {
    return(args[[1]])
  }
  
  result <- SafeList(...)
  class(result) <- c("Settings", class(result))
  return(result)
}

##' @describeIn 
as.Settings <- function(x) {
  return(Settings(x))
}

is.Settings <- function(x) {
  return(is(x, "Settings"))
}



##' Create a PEcAn SettingsList object
##'
##' 
##' @title Create a PEcAn SettingsList object
##' @param 
##' @return
##' @export
##' @author Ryan Kelly
SettingsList <- function(...) {
  args <- list(...)
  if(length(args)==1) {
    if(is(args[[1]], "SettingsList")) {
      return(args[[1]])
    }
    if(is.list(args[[1]]) && all(sapply(args[[1]], is.Settings))) {
      args <- args[[1]]
    }
  }
  
  if(!all(sapply(args, is.Settings))) {
    stop("Can't create SettingsList from anything other than Settings")
    # Could allow the constructor to take an arbitrary list of lists and convert each to Settings.
    # But probably no one will want to do that any time soon, and this sanity-check is safer. 
  }
  
  result <- args
  names(result) <- paste("settings", seq_along(result), sep=".")
  # result <- fix.SettingsList.names(result)
  class(result) <- c("SettingsList", class(result))
  return(result)
}

# Settings all need names so they can be read/written to xml. This gives dummy ones.
fix.SettingsList.names <- function(settingsList) {
  if(length(settingsList) == 0) {
    return(settingsList)
  }
  # browser()
  sl.names <- names(settingsList)
  if(is.null(sl.names)) {
    names(settingsList) <- paste("settings", seq_along(settingsList), sep=".")
  } else {
    ind <- which(is.na(sl.names) | sl.names=="")
    names(settingsList)[ind] <- paste("settings", ind, sep=".")
  }
  return(settingsList)
}

##' @export
##' @describeIn 
as.SettingsList <- function(x) {
  return(SettingsList(x))
}

##' @export
is.SettingsList <- function(x) {
  return(is(x, "SettingsList"))
}

##' @export
"[[<-.SettingsList" <- function(x, value, i) {
  if(!is.Settings(value) && !is.null(value)) {
    stop("Can only add Settings to SettingsList")
  }
  result <- NextMethod()
  result <- fix.SettingsList.names(result)
}

##' @export
"$<-.SettingsList" <- function(x, value, i) {
  # Don't know why this didn't work with just NextMethod...
  x[[i]] <- value
  x <- fix.SettingsList.names(x)
  return(x)
}


##' @export
"[<-.SettingsList" <- function(x, value, i) {
  if(!is.null(value))
    value <- SettingsList(value) # Throws errorr if value can't be coerced to SettingsList
  result <- NextMethod()
  result <- fix.SettingsList.names(result)
  return(result)
}


##' @export
"[[.SettingsList" <- function(x, i) {
  if(is.character(i)) {
    vals <- lapply(x, function(y) y[[i]])
    if(all(sapply(vals, function(y, y1) identical(y, y1), vals[[1]]))) {
      return(vals[[1]])
    } else {
      return(vals)
    }
  } else {
    NextMethod()
  }
}

##' @export
"$.SettingsList" <- function(x, i) {
  return(x[[i]])
}


##' @export
"[.SettingsList" <- function(x, i) {
  if(is.character(i))
    stop("Selecting from SettingsList using single brackets and character indices is not allowed")
  SettingsList(NextMethod())
}

##' @export
names.SettingsList <- function(x) {
  for(i in seq_along(x)) {
    if(i == 1) {
      result <- names(x[[i]])
    } else {
      result <- intersect(result, names(x[[i]])) 
    }
  }
  return(result)
}
