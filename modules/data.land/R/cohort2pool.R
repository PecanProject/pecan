##' cohort2pool function
##'Calculates total biomass using veg cohort file. 
##'
##' @export
##' @param veg_file standard cohort veg_file
##' @param allom_param parameters for allometric equation, a and b. Based on base-10 log-log linear model (power law)
##' @author Saloni Shah
##' @example 
##' \dontrun{
##' veg_file <- "~/downloads/FFT_site_1-25665/FFT.2008.veg.rds"
##' cohort2pool(veg_File = veg_file, allom_param = NULL)
##' }

veg_file <- "/fs/data1/pecan.data/dbfiles/Forest_Geo_site_1-5005/Forest_Geo.1981.veg.rds" #file path for RDS file
cohort2pool <- function(veg_file,allom_param =NULL) {
  
  ## Building Site ID from past directories
  path <- dirname(veg_file)
  last_dir <- basename(path)
  nums_id <- strsplit(last_dir,"[^[:digit:]]")
  base_id <- nums_id[[1]][length(nums_id[[1]])]
  suffix <- nums_id[[1]][(length(nums_id[[1]])-1)]
  siteid = as.numeric(suffix)*1e9 + as.numeric(base_id)
  
  ## load data
  
  dat <- readRDS(veg_file)
  
  ## Grab DBH
  dbh <- dat[[2]]$DBH
  
  ## Grab allometry
  if(is.null(allom_param)){
    a <- 2                        
    b <- 0.3
  } else {
    print("user provided allometry parameters not yet supported")
    return(NULL)
  }
  
  #Calculate AGB
  biomass = 10^(a + b*log10(dbh))
  biomass[is.na(biomass)] <- 0
  tot_biomass <- sum(biomass)
  AGB <- tot_biomass
  
  #Prep Arguments for pool_ic function
  dims <- list(time =1) #Time dimension may be irrelevant
  variables <-list(AGB = tot_biomass)
  input <- list(dims = dims,
                vals = variables)
  
  # Execute pool_ic function
  result <- PEcAn.data.land::pool_ic_list2netcdf(input = input, outdir = path, siteid = siteid)
  
  return(result)
}