#--------------------------------------------------------------------------------------------------#
##' Removes previous model run output from worker node local scratch directories on EBI-CLUSTER 
##'
##' @title Clear EBI-CLUSTER worker node local scratch directories of old PEcAn output
##' @name clear.scratch
##' @author Shawn Serbin
##' @return nothing
##' @examples
##' \dontrun{
##' clear.scratch(settings)
##' }
clear.scratch <- function(settings){
  
  ### Setup script
  clear.scratch <- system.file("data", "clear.scratch.sh", package="PEcAn.utils")
  host <-  settings$run$host
  nodes <- paste("all.q@compute-0-",seq(0,24,1),".local",sep="")
  
  if(any(grep('cluster',host$name))) {
    for (i in nodes){
      print(paste("----- Removing output on node: ",i,sep=""))
      system(paste("ssh -T ", settings$run$host$name," qlogin -q ",
                   i," < ",clear.scratch,sep=""))
    } ### End of for loop
    
  } else {
    print("---- No output to delete.  Output host is not EBI-CLUSTER ----")
    
  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################