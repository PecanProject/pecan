##-----------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-----------------------------------------------------------------------------
#' Create a PEcAn Settings object
#'
#' @title Create a PEcAn Settings object
#' @param ... objects to concatenate
#' @return a list containing all objects in `...`,
#'   with class c("Settings", "SafeList", "list").
#' @export
#' @author Ryan Kelly
Settings <- function(...) {
  args <- list(...)
  if (length(args) == 1 && inherits(args[[1]], "Settings")) {
    return(args[[1]])
  }

  result <- SafeList(...)
  class(result) <- c("Settings", class(result))
  return(result)
}

#' @export
#' @describeIn Settings coerce an object to Settings
#' @param x object to test or coerce
as.Settings <- function(x) {
  return(Settings(x))
}

#' @export
#' @describeIn Settings test if object is already a Settings
is.Settings <- function(x) {
  return(inherits(x, "Settings"))
}
