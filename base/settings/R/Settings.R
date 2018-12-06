##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
##' Create a PEcAn Settings object
##' 
##' @title Create a PEcAn Settings object
##' @param ... TODO
##' @return
##' @export
##' @author Ryan Kelly
Settings <- function(...) {
  args <- list(...)
  if (length(args) == 1 && inherits(args[[1]], "Settings")) {
    return(args[[1]])
  }
  
  result <- SafeList(...)
  class(result) <- c("Settings", class(result))
  return(result)
}

##' @export
##' @describeIn 
as.Settings <- function(x) {
  return(Settings(x))
}

##' @export
is.Settings <- function(x) {
  return(inherits(x, "Settings"))
}
