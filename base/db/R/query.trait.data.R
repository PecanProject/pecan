##' Extract trait data from database
##'
##' Extracts data from database for a given trait and set of species,
##' converts all statistics to summary statistics, and prepares a dataframe for use in meta-analysis.
##' For Vcmax and SLA data, only data collected between  April and July are queried, and only data collected from the top of the canopy (canopy height > 0.66).
##' For Vcmax and root_respiration_rate, data are scaled
##' converted from measurement temperature to \eqn{25^oC} via the arrhenius equation.
##'
##' @param trait is the trait name used in the database, stored in variables.name
##' @param con database connection object
##' @param update.check.only if TRUE, returns results but does not print summaries
##' @param ... unused currently
##' @param spstr is the species.id integer or string of integers associated with the species
##' @param ids_are_cultivars if TRUE, the IDs in spstr are cultivar IDs, otherwise they are species IDs. Passed on to \code{\link{query.data}}
##'
##' @return dataframe ready for use in meta-analysis
##' @export query.trait.data
##' @examples
##' \dontrun{
##' settings <- read.settings()
##' query.trait.data("Vcmax", "938", con = con)
##' }
##' @author David LeBauer, Carl Davidson, Shawn Serbin
query.trait.data <- function(trait, spstr, con = NULL, update.check.only = FALSE, ids_are_cultivars = FALSE, ...){
  
  if(is.list(con)){
    PEcAn.logger::logger.warn("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  }
  
  # print trait info
  if(!update.check.only) {
    PEcAn.logger::logger.info("---------------------------------------------------------")
    PEcAn.logger::logger.info(trait)
  }
  
  ### Query the data from the database for trait X.
  data <- query.data(trait = trait, spstr = spstr, con = con, store.unconverted = TRUE, ids_are_cultivars = ids_are_cultivars)
  
  ### Query associated covariates from database for trait X.
  covariates <- query.covariates(trait.ids = data$id, con = con)
  canopy.layer.covs <- covariates[covariates$name == 'canopy_layer', ]
  
  ### Set small sample size for derived traits if update-checking only. Otherwise use default.
  if(update.check.only) {
    sample.size <- 10
  } else {
    sample.size <- 10^6  ## Same default as derive.trait(), derive.traits(), and take.samples()
  }
  
  if(trait == 'Vcmax') {
    #########################   VCMAX   ############################
    ### Apply Arrhenius scaling to convert Vcmax at measurement temp to that at 25 degC (ref temp).
    data <- arrhenius.scaling.traits(data = data, covariates = covariates, temp.covariates = c('leafT', 'airT','T'))
    
    ### Keep only top of canopy/sunlit leaf samples based on covariate.
    if(nrow(canopy.layer.covs) > 0) data <- filter_sunleaf_traits(data = data, covariates = canopy.layer.covs)
    
    ## select only summer data for Panicum virgatum
    ##TODO fix following hack to select only summer data
    if (spstr == "'938'"){
      data <- subset(data, subset = data$month %in% c(0,5,6,7))
    }
    
  } else if (trait == 'SLA') {
    #########################    SLA    ############################
    ## convert LMA to SLA
    data <- rbind(data,
                  derive.traits(function(lma){1/lma},
                                query.data('LMA', spstr, con=con, store.unconverted=TRUE,
                                           ids_are_cultivars=ids_are_cultivars),
                                sample.size=sample.size))
    
    ### Keep only top of canopy/sunlit leaf samples based on covariate.
    if(nrow(canopy.layer.covs) > 0) data <- filter_sunleaf_traits(data = data, covariates = canopy.layer.covs)
    
    ## select only summer data for Panicum virgatum
    ##TODO fix following hack to select only summer data
    if (spstr == "'938'"){
      data <- subset(data, subset = data$month %in% c(0,5,6,7,8,NA))
    }
    
  } else if (trait == 'leaf_turnover_rate'){
    #########################    LEAF TURNOVER    ############################
    ## convert Longevity to Turnover
    data <- rbind(data,
                  derive.traits(function(leaf.longevity){ 1 / leaf.longevity },
                                query.data('Leaf Longevity', spstr, con = con, store.unconverted = TRUE,
                                           ids_are_cultivars = ids_are_cultivars),
                                sample.size = sample.size))
    
  } else if (trait == 'root_respiration_rate') {
    #########################  ROOT RESPIRATION   ############################
    ## Apply Arrhenius scaling to convert root respiration at measurement temp
    ## to that at 25 degC (ref temp).
    data <- arrhenius.scaling.traits(data = data, covariates = covariates, temp.covariates = c('rootT', 'airT','soilT'))
    
  } else if (trait == 'leaf_respiration_rate_m2') {
    #########################  LEAF RESPIRATION   ############################
    ## Apply Arrhenius scaling to convert leaf respiration at measurement temp
    ## to that at 25 degC (ref temp).
    data <- arrhenius.scaling.traits(data = data, covariates = covariates, temp.covariates = c('leafT', 'airT','T'))
    
  } else if (trait == 'stem_respiration_rate') {
    #########################  STEM RESPIRATION   ############################
    ## Apply Arrhenius scaling to convert stem respiration at measurement temp
    ## to that at 25 degC (ref temp).
    data <- arrhenius.scaling.traits(data = data, covariates = covariates, temp.covariates = c('stemT', 'airT','T'))
    
  } else if (trait == 'c2n_leaf') {
    #########################  LEAF C:N   ############################
    
    data <- rbind(data,
                  derive.traits(function(leafN){ 48 / leafN },
                                query.data('leafN', spstr, con = con, store.unconverted = TRUE,
                                           ids_are_cultivars = ids_are_cultivars),
                                sample.size = sample.size))
    
  } else if (trait == 'fineroot2leaf') {
    #########################  FINE ROOT ALLOCATION  ############################
    ## FRC_LC is the ratio of fine root carbon to leaf carbon
    data <- rbind(data, query.data(trait = 'FRC_LC', spstr = spstr, con = con, store.unconverted = TRUE, ids_are_cultivars = ids_are_cultivars))
  }
  result <- data
  
  ## if result is empty, stop run
  
  if (nrow(result) == 0) {
    return(NA)
    warning(paste("there is no data for", trait))
  } else {
    
    ## Do we really want to print each trait table?? Seems like a lot of
    ## info to send to console.  Maybe just print summary stats?
    ## print(result)
    if (!update.check.only) {
      PEcAn.logger::logger.info(paste("Median ", trait, " : ", round(stats::median(result$mean, na.rm = TRUE), digits = 3), sep = ""))
      PEcAn.logger::logger.info("---------------------------------------------------------")
    }
    # print list of traits queried and number by outdoor/glasshouse
    return(result)
  }
}