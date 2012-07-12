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
get.model.output <- function(model){

  my.fcn = paste("get.model.output",model,sep=".")

  if(exists(my.fcn)){
    do.call(my.fcn,args=list())
  } else {
    print("-------------------------------------------------------------------")
    print(c(my.fcn,"could not be found"))
    print(c("please make sure module for:",model,"is implemented and loaded"))
    print("-------------------------------------------------------------------")
    print(" ")

  }

  return()
  
  if (model=="ED2"){
    print(" ")
    print("-------------------------------------------------------------------")
    print(" Retrieving ED2 model output")
    print("-------------------------------------------------------------------")
    print(" ")
    Sys.sleep(1)
    
    ### Retrieve model output from local or remote server
    get.model.output.ed()
  
    ### Parse model output
    
  }else if (model=="SIPNET") {
    #write.configs.sipnet()
    print(" ")
    print("-------------------------------------------------------------------")
    print(" Generating SIPNET model run and configuration files")
    print("-------------------------------------------------------------------")
    print(" ")
    
    print("!!!! Not yet implemented !!!!")
    
  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
