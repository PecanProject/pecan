##' cohort2pool function
##'Calculates total biomass using veg cohort file. 
##'
##' @export
##' @param veg_file path to standard cohort veg_file
##' @param allom_param parameters for allometric equation, a and b. Based on base-10 log-log linear model (power law)
##' @author Saloni Shah
##' @examples
##' \dontrun{
##' veg_file <- "/fs/data1/pecan.data/dbfiles/Forest_Geo_site_1-5005/Forest_Geo.1981.veg.rds"
##' cohort2pool(veg_file = veg_file, allom_param = NULL)
##' }

cohort2pool <- function(veg_file, allom_param = NULL) {
  
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
    a <- -2                        
    b <- 2.5
  } else {
    print("user provided allometry parameters not yet supported")
    return(NULL)
  }
  
  #Calculate AGB
  biomass = 10^(a + b*log10(dbh))
  biomass[is.na(biomass)] <- 0
  tot_biomass <- sum(biomass)
  ##### No P
  #Need to Convert the total kg of biomass to (kg/m^2) but data on area of land does not exist
  # Setting AGB to average biomass of the the stand
  AGB <- tot_biomass/length(biomass) # units (KgC/m^2)
  
  #Calculate Component Biomass
  #Jenkins, Jennifer C., et al. "Comprehensive database of diameter-based biomass regressions for North American tree species." 
  # Gen. Tech. Rep. NE-319. Newtown Square, PA: US Department of Agriculture, Forest Service, Northeastern Research Station.(2004).
  #ratio = exp(B0 + (B1/DBH))
  
  component_names <- c("foliage","coarse_root", "stem_bark","stem_wood")
  
  # Hardwood params
  hwood_0params <-c(-4.08113,-1.6911,-2.0129,-0.3065)
  hwood_1params <- c(5.8816, 0.8160, -1.6805,-5.4240)
  hlist <-list(hwood_0params, hwood_1params)
  # Softwood params
  swood_0params <-c(-2.9584,-1.5619,-2.0980,-0.3737)
  swood_1params <- c(4.4766,0.6614,-1.1432,-1.8055)
  slist <-list(swood_0params, swood_1params)
  
  
  comp_ratios <- purrr::pmap(hlist,function(B0,B1){
    
    exp(B0 + (B1/dbh))

  }) %>% setNames(component_names)
   
  

  leaf_carbon_content        <- mean(as.vector(comp_ratios$foliage) * AGB, na.rm =TRUE)
  coarse_root_carbon_content <- mean(as.vector(comp_ratios$coarse_root) * AGB, na.rm =TRUE)
  tot_sbark_bmass            <- mean(as.vector(comp_ratios$stem_bark) * AGB, na.rm =TRUE)
  wood_carbon_content        <- mean(as.vector(comp_ratios$stem_wood) * AGB, na.rm =TRUE) +tot_sbark_bmass

  
  
  #Prep Arguments for pool_ic function
  dims <- list(time =1) #Time dimension may be irrelevant
  variables <-list(AGB =AGB,leaf_carbon_content=leaf_carbon_content,coarse_root_carbon_content= coarse_root_carbon_content,
                   wood_carbon_content=wood_carbon_content)
  input <- list(dims = dims,
                vals = variables)
  
  # Execute pool_ic function
  result <- PEcAn.data.land::pool_ic_list2netcdf(input = input, outdir = path, siteid = siteid)
  
  return(result)
}
