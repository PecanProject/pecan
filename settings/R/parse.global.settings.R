##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------


##' Parse global settings into a list of pecan Settings objects
##' 
##' Currently, used to convert a multi-site settings object into a list of settings objects, one
##' per site. Could be extended to handle multi-model comparisons or other applications
##' 
##' @param global.settings The global settings object, likely returned by read.settings()
##' @return A list of settings objects
##' @export
##' @author Ryan Kelly
parse.global.settings <- function(global.settings){
  global.settings <- SafeList(global.settings) # allow $ indexing with impunity
  
  # If the list contains <runs>, assume this is a multi-site run
  if(!is.null(global.settings$runs)) {
    runs <- global.settings$runs
    shared.settings <- Settings(global.settings)
      shared.settings$runs <- NULL
      
    settings.list = SettingsList()
    for(i in seq_along(runs)) {
      settings.i <- shared.settings
      settings.i$run <- runs[[i]]
      settings.list[[i]] <- c(settings.list, settings.i)
    }
    invisible(settings.list)
  }
  
  # Otherwise, just convert to a settings object and return
  invisible(Settings(global.settings))
}
