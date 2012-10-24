#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Removes previous model run output from worker node local scratch directories on EBI-CLUSTER 
##'
##' @title Clear EBI-CLUSTER worker node local scratch directories of old PEcAn output
##' @name clear.scratch
##' @author Shawn Serbin
##' @return nothing
##' @export
##' @examples
##' \dontrun{
##' clear.scratch(settings)
##' }
clear.scratch <- function(settings){
  
  ### Setup script
  clear.scratch <- system.file("clear.scratch.sh", package="PEcAn.utils")
  host <-  settings$run$host
  nodes <- paste("all.q@compute-0-",seq(0,24,1),".local",sep="")
  
  if(any(grep('cluster',host$name))) {
    for (i in nodes){
      print(" ")
      print(paste("----- Removing output on node: ",i,sep=""))
      system(paste("ssh -T ", settings$run$host$name," qlogin -q ",
                   i," < ",clear.scratch,sep=""))
      print(" ")
    } ### End of for loop
    
  } else {
    print("---- No output to delete.  Output host is not EBI-CLUSTER ----")
    
  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
