#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' @name recurse.create
##' @title recurse.create
##' @description recursively follow the file structure in 'ins' and create all the same folders in 'path' as well as symbolic links to all the file. This is done, rather than creating a symbolic link to the whole structure, so individual files can later be unlinked and replaced with different files/links.
##' @author Mike Dietze
##' @param path new location to create folders and links
##' @param ins  reference location of inputs
##' @export
recurse.create <- function(path, ins) {
  files <- list.files(ins, full.names = TRUE)
  if (length(files) == 0) {
    return()
  }
  is.dir <- dir.exists(files) ## determine which files are actually folders
  ## create links to true files
  curr <- files[!is.dir]
  for (i in seq_along(curr)) {
    file.symlink(curr[i], file.path(path, basename(curr[i])))
  }

  down <- files[is.dir]
  for (i in seq_along(down)) {
    ndir <- file.path(path, basename(down[i]))
    ## create folders
    dir.create(ndir)

    ## recurse to fill folders
    recurse.create(ndir, down[i])
  }
} # recurse.create
