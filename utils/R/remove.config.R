#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
remove.config <- function(dir,settings,model){

  fcn.name <- paste("remove.config.",model,sep="")
  if(exists(fcn.name)){
    do.call(fcn.name,args=list(dir,settings))
  } else {
    warning(paste(fcn.name,"does not exist"))
    warning("This function is not required, but it's implementation is recommended")
  }

  
}
