#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##'
##' This function retrieves model output for further analyses
##' @name get.model.output
##' @title Retrieve model output
##'
##' @param model the ecosystem model run
##'
##' @export
##' 
##' @examples
##' \dontrun{
##' get.model.output(model)
##' get.model.output("ED2")
##' }
##'
##' @author Michael Dietze, Shawn Serbin, David LeBauer
get.model.output <- function(model, settings){
  print(paste("---- Getting output for ",model," model",sep=""))
  my.fcn = paste("get.model.output",model,sep=".")

  if(exists(my.fcn)){
    do.call(my.fcn, args=list(settings))    
  } else {
    print("-------------------------------------------------------------------")
    print(c(my.fcn,"could not be found"))
    print(c("please make sure module for:",model,"is implemented and loaded"))
    print("-------------------------------------------------------------------")
    print(" ")

  }

  return()
}
####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
