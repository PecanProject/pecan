##' cohort2pool function
##'Calculates total biomass using veg cohort file. 
##' @name cohort2pool
##' @title cohort2pool
##' @description Converts .rds files into pool netcdf files.
##' @export
##'
##' @param veg_file path to standard cohort veg_file
##' @param dbh_name Default is "DBH". This is the column name in the veg_file that represents DBH. May differ depending on data source.
##' @param allom_param parameters for allometric equation, a and b. Based on base-10 log-log linear model (power law)
##'
##' @author Saloni Shah
##'
##' \dontrun{
##' veg_file <- "~/downloads/FFT_site_1-25665/FFT.2008.veg.rds"
##' cohort2pool(veg_File = veg_file, allom_param = NULL)
##' }

cohort2pool <- function(veg_file, allom_param = NULL, dbh_name="stemDiameter") {
  
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
  dbh <- dat[[2]][,dbh_name]
  
  ## Grab allometry
  if(is.null(allom_param)){
    a <- 2                        
    b <- 0.3
    biomass = 10^(a + b*log10(dbh))
  } else {
    #Predict AGB using allom.predit code taken from Allom.Vignette.Rmd
    allom.fit = dat
    stand = allom.predict(allom.fit,dbh = dbh,pft = "LH",component = 3,use = "Bg",interval = "prediction")
    AGB = apply(stand,1,sum)
    hist(AGB)
    #print("user provided allometry parameters not yet supported")
    #return(NULL)
    return(AGB)
  }
  
  #Calculate AGB
  # biomass[is.na(biomass)] <- 0
  # tot_biomass <- sum(biomass,na.rm = TRUE)
  # AGB <- tot_biomass
  
  ## NEON SPECIFIC HACK
  obs <- dat[[2]]
  n.plot = length(unique(paste(obs$siteID.x,obs$plotID.x,obs$subplotID.x)))
  AGB = tot_biomass/(dat[[1]]$area*n.plot) ## express biomass on a per unit area basis
  
  #Prep Arguments for pool_ic function
  dims <- list(time =1) #Time dimension may be irrelevant
  variables <-list(AGB = tot_biomass)
  input <- list(dims = dims,
                vals = variables)
  
  # Execute pool_ic function
  result <- PEcAn.data.land::pool_ic_list2netcdf(input = input, outdir = path, siteid = siteid)
  
  return(result)
}