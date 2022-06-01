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
<<<<<<< HEAD
##' @param veg_info veg_info object passed from write_ic
##' @param dryMass_name Default is "dryMass". This is the column name in the veg_file that represents herbaceous dry mass. May differ depending on data source.
=======
>>>>>>> e6ab17f40e988dac2d692b65c9ab2e4f2d87a6ef
##'
##' @author Saloni Shah
##' @examples
##' \dontrun{
##' veg_file <- "~/downloads/FFT_site_1-25665/FFT.2008.veg.rds"
##' cohort2pool(veg_File = veg_file, allom_param = NULL)
##' }
##' 


cohort2pool <- function(veg_info, allom_param = NULL, dbh_name="DBH", dryMass_name = "dryMass") {
  
  ## load data
  dat <- veg_info

  # Grab herbaceous dry mass
  dryMass <- dat[[3]][,dryMass_name]
  #filter for DBH < 2.5
  dat_less <- dat[[2]][dat[[2]]$DBH < 2.5,]
  dbh_less <- dat_less$DBH
  #filter for DBH > 2.5
  dat_greater <- dat[[2]][dat[[2]]$DBH > 2.5,]
  dbh_greater <- dat_greater$DBH
  #filter for DBH = 2.5
  dat_equal <- dat[[2]][dat[[2]]$DBH == 2.5,]
  dbh_equal <- dat_equal$DBH

  #Grab plot size
  herb_plot <- dat[[1]]$herb_clipArea
  plot_size <- dat[[1]]$subPlot
  #Grab number of plots
  tree_num <- length(unique(dat[[2]]$plot))
  herb_num <- length(unique(dat[[3]]$plot))
  
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
  
  # #Calculate AGB
  biomass[is.na(biomass)] <- 0
  tot_biomass <- sum(biomass,na.rm = TRUE)
  
  #calculate total herbaceous biomass, already in gC
  tot_herb <- sum(dat[[3]][,"dryMass"])/(herb_plot*herb_num)
  
  #calculate total wood and leaf biomass
  ratio[is.na(ratio)] <- 0
  leaf <- ratio*biomass
  tot_leaf <- sum(leaf,na.rm = TRUE)
  
  #Divide by plot area, divide by 2 to convert from kg to kgC then multiply by 1000 to get gC
  #THflag <- dat[[4]] #add different calculation for tree vs herb site

  leaf_biomass = ((tot_leaf/(tree_num*plot_size))/2)*1000 + tot_herb
  AGB = ((tot_biomass/(tree_num*plot_size))/2)*1000
  wood_biomass = AGB - leaf_biomass
  
  #grab soil carbon info
  soil_carbon = dat[[5]] #conversion done in extract_NEON_veg (gC/m^2)
  
  #Prep Arguments for pool_ic function
  dims <- list(time =1) #Time dimension may be irrelevant
  variables <-list(AbvGrndWood = AGB, wood_carbon_content = wood_biomass, leaf_carbon_content = leaf_biomass, soil_organic_carbon_content = soil_carbon) 
  input <- list(dims = dims,
                vals = variables)
  
  
  return(input)
}