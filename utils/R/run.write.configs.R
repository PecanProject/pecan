#--------------------------------------------------------------------------------------------------#
##' Main driver function to call the ecosystem model specific (e.g. ED, SiPNET) 
##' run and configuration file scripts 
##' 
##' @name run.write.configs
##' @title Run model specific write configuration functions
##' @param model the ecosystem model to generate the configuration files for
##' @export
##'
##' @author Shawn Serbin
run.write.configs <- function(model){
  
  if (model=="ED2"){
    print(" ")
    print("-------------------------------------------------------------------")
    print(" Generating ED2 model run and configuration files")
    print("-------------------------------------------------------------------")
    print(" ")
    Sys.sleep(1)
    run.write.configs.ed()
    
  }else if (model=="SIPNET") {
    #write.configs.sipnet()
    print(" ")
    print("-------------------------------------------------------------------")
    print(" Generating SIPNET model run and configuration files")
    print("-------------------------------------------------------------------")
    print(" ")
  }
  
  
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
