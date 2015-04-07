#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Creates an absolute path to a folder.
##'
##' This will take a folder and make it into an absolute folder name. It
##' will normalize the path and prepend it with the current working folder
##' if needed to get an absolute path name.
##'
##' @title Creates an absolute path to a folder
##' @name full.path
##' @author Rob Kooper
##' @return absolute path
##' @export
##' @examples
##' full.path("pecan")
full.path <- function(folder){
  # normalize pathname
  folder <- normalizePath(folder, mustWork=FALSE)

  # add cwd if needed
  if (substr(folder, 1, 1) != '/') {
  	folder <- file.path(getwd(), folder)
  	folder <- normalizePath(folder, mustWork=FALSE)
  }

  invisible(folder)
} 


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
