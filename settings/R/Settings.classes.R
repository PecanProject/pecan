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
  class(result) <- c("SettingsList", class(result))
  return(result)
}

##' @describeIn 
as.SettingsList <- function(x) {
  return(SettingsList(x))
}

is.SettingsList <- function(x) {
  return(is(x, "SettingsList"))
}


"[[<-.SettingsList" <- function(x, value, i) {
  if(!is.Settings(value) && !is.null(value)) {
    stop("Can only add Settings to SettingsList")
  }
  NextMethod()
}

"$<-.SettingsList" <- function(x, value, i) {
  # Don't know why this didn't work with just NextMethod...
  x[[i]] <- value
  return(x)
}


# This one's confusing so I didn't override it. No need to use single brackets for assigning to list, anyway.
"[<-.SettingsList" <- function(x, value, i) {
  stop("Single bracket assignments to SettingsList not allowed. Use double brackets.")
}

