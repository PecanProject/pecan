##' cohort2pool function
##'Calculates total biomass using veg cohort file. 
##' @name cohort2pool
##' @title cohort2pool
##' @description Converts .rds files into pool netcdf files.
##' @export
##'
##' @param dbh_name Default is "DBH". This is the column name in the veg_file that represents DBH. May differ depending on data source.
##' @param dat veg_info file
##' @param allom_param parameters for allometric equation, a and b. Based on base-10 log-log linear model (power law)
##'
##' @author Saloni Shah
##' @examples
##' \dontrun{
##' veg_file <- "~/downloads/FFT_site_1-25665/FFT.2008.veg.rds"
##' cohort2pool(veg_File = veg_file, allom_param = NULL)
##' }
##' 

cohort2pool <- function(dat, allom_param = NULL, dbh_name="DBH") {
  
  ## Grab DBH
  dbh <- dat[[2]][,dbh_name]
  #Grab plot size
  plot_size <- dat[[1]]$area
  #Grab number of plots
  plot_num <- length(unique(paste(dat[[2]]$site_name,dat[[2]]$plot,dat[[2]]$Subplot)))
  
  ## Grab allometry
  if(is.null(allom_param)){
    a <- -2.0127                        
    b <- 2.4342
    biomass = exp(a + b*log(dbh))
    #Hard code foliage equation from Jenkins paper
    b0 <- -4.0813
    b1 <- 5.8816
    ratio = ifelse(dbh>=2.5,exp(b0 + (b1/dbh)),exp(b0 + (b1/2.5)))
  } else {
    #Predict AGB using allom.predit code taken from Allom.Vignette.Rmd
    # allom.fit = #outputs from AllomAve function
    # stand = allom.predict(allom.fit,dbh = dbh,pft = "LH",component = 3,use = "Bg",interval = "prediction")
    # AGB = apply(stand,1,sum)
    # hist(AGB)
    AGB <- NULL
    print("user provided allometry parameters not yet supported")
    #return(NULL)
    return(AGB)
  }
  
  #Calculate AGB
  biomass[is.na(biomass)] <- 0
  tot_biomass <- sum(biomass,na.rm = TRUE)
  
  #calculate total wood and leaf biomass
  ratio[is.na(ratio)] <- 0
  leaf <- ratio*biomass
  tot_leaf <- sum(leaf,na.rm = TRUE)
  
  #Divide by plot area, divide by 2 to convert from kg to kgC
  leaf_biomass = (tot_leaf/(plot_num*plot_size))/2
  AGB = (tot_biomass/(plot_num*plot_size))/2
  wood_biomass = AGB - leaf_biomass
  
  #Prep Arguments for pool_ic function
  dims <- list(time =1) #Time dimension may be irrelevant
  variables <-list(AbvGrndWood = AGB, wood_carbon_content = wood_biomass, leaf_carbon_content = leaf_biomass)
  input <- list(dims = dims,
                vals = variables)
  
  
  return(input)
}