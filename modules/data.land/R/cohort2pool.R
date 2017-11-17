##' @name cohort2pool
##' @title cohort2pool
##' @export
##' @param veg_file standard cohort veg_file
##' @param allom_param parameters for allometric equation, a and b
##' @author Saloni Shah

cohort2pool <- function(veg_file,allom_param =NULL) {
  
  dat <- readRDS(veg_file)
  
  #Building Site ID from past directories
  
  path <- dirname(veg_file)
  last_dir <- basename(path)
  nums_id <- strsplit(last_dir,"[^[:digit:]]")
  base_id <- nums_id[[1]][length(nums_id[[1]])]
  suffix <- nums_id[[1]][(length(nums_id[[1]])-1)]
  if (suffix == 1) {
    siteid <- as.numeric(paste0(suffix, "0000", base_id))
  } else if (siteid ==0) {
    siteid <- as.numeric(base_id)
  }
  
  #Grab DBH
  
  dbh <- dat[[2]]$DBH
  if(is.null(allom_param)){
    a = 2                        
    b= 0.3
  }
  
  #Calculate AGB
  
  for (i in 1: length(dbh)) {
    biomass = a + b*log(dbh)  
  }
  
  tot_biomass <- sum(biomass)
  
  #Prep Arguments for pool_ic function
  
  input <- list()
  dims <- list(time =1)
  variables <-list(AGB=tot_biomass)
  input$dims <-dims
  input$vals <-variables
  
  # Execute pool_ic function
  
  result <- PEcAn.data.land::pool_ic_list2netcdf(input = input, outdir = path, siteid = siteid)
  
  
  return(result)
}

