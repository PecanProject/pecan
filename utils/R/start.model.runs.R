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
  
  if (model=="ED2"){
    print(" ")
    print("-------------------------------------------------------------------")
    print(" Starting ED2 model runs")
    print("-------------------------------------------------------------------")
    print(" ")
    Sys.sleep(2)
    
    ### Retrieve model output from local or remote server
    start.runs.ed()
    
  }else if (model=="SIPNET") {
    #write.configs.sipnet()
    print(" ")
    print("-------------------------------------------------------------------")
    print(" Starting SIPNET model runs")
    print("-------------------------------------------------------------------")
    print(" ")
    
    print("!!!! Not yet implemented !!!!")
    
  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################