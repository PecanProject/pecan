#--------------------------------------------------------------------------------------------------#
##' 
##' @name run.write.configs.ed.R
##'
##' @title Generate input ED2.2 model ED2IN and settings.xml files for PEcAn workflow
##'
##'
##' @author Shawn Serbin
#--------------------------------------------------------------------------------------------------#
run.write.configs.ed <- function() {
  
  ### Identify PFTs in the input settings.xml file
  num.pfts <- length(settings$pfts)
  pft.names=as.list(rep(NA,num.pfts))
  
  for (i in 1:num.pfts){
    pft.names[i] <- settings$pfts[i]$pft$name
    
    ### If no PFT(s) are specified insert NULL to warn user 
    if(length(pft.names)==0) pft.names[1]="NULL" 
    ###
    
    ### Get output directory info
    outdirs[i] <- settings$pfts[i]$pft$outdir
    
  } ### End of for loop to extract pft names
  
  print(" ")
  print("-------------------------------------------------------------------")
  print("Selected PFT(s): ")
  print(pft.names)
  print("-------------------------------------------------------------------")
  print(" ")
  
  ### Generate empty list arrays for output.
  trait.samples <- list()
  sa.samples <- list()
  ensemble.samples <- list()
  env.samples <- list()
  ###
  
  ### Prepare for model output
  remove.config()
  
  ### Sensitivity Analysis
  if('sensitivity.analysis' %in% names(settings)) {
    
    ### Get info on the quantiles to be run in the sensitivity analysis (if requested)
    quant <- get.quantiles(settings$sensitivity.analysis$quantiles)
    ### Get info on the years to run the sensitivity analysis (if requested)
    sa.years = data.frame(sa.start = settings$sensitivity.analysis$start.year, 
                          sa.end = settings$sensitivity.analysis$end.year)
    
    print(" ")
    print(" ")
    print("-------------------------------------------------------------------")
    print("Selected Quantiles: ")
    #print("Lower             Mid             Upper")
    print(round(quant,3))
    print("-------------------------------------------------------------------")
    print(" ")
    print(" ")
    
    
  } ### End of SA
  
} ### End of function: run.write.configs.ed.R
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################