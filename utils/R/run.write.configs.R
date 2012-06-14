#--------------------------------------------------------------------------------------------------#
##' Main driver function to call the ecosystem model specific (e.g. ED, SiPNET) 
##' run and configuration file scripts 
##'
##' @name run.write.configs
##' @param model the ecosystem model to generate the configuration files for
##' @author
#--------------------------------------------------------------------------------------------------#
run.write.configs <- function(model){
  
  if (model=="ED2"){
    print(" ")
    print("-------------------------------------------------------------------")
    print(" Generating ED2 model run and configuration files")
    print("-------------------------------------------------------------------")
    print(" ")
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