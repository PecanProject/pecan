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
##' Start selected ecosystem model runs within PEcAn workflow
##' 
##' @name start.model.runs
##' @title Start ecosystem model runs
##' @export 
##' @examples
##' \dontrun {
##' start.model.runs("ED2")
##' start.model.runs("SIPNET")
##' }
##' @author Shawn Serbin
##'
start.model.runs <- function(model){

  fcn.name <- paste("start.runs.",model,sep="")
  if(exists(fcn.name)){
    print(" ")
    print("-------------------------------------------------------------------")
    print(paste(" Starting model runs",model))
    print("-------------------------------------------------------------------")
    print(" ")
    
    do.call(fcn.name,args=list())
    
  } else {
    warning(paste(fcn.name,"does not exist"))
    warning(paste("This function is required, please make sure the model module is loaded for",model))
    stop()
  }

  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
