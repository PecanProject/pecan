######################## COVARIATE FUNCTIONS #################################

##--------------------------------------------------------------------------------------------------#
##' Append covariate data as a column within a table
##'
##' \code{append.covariate} appends a data frame of covariates as a new column in a data frame
##'   of trait data.
##' In the event a trait has several covariates available, the first one found
##'   (i.e. lowest row number) will take precedence
##'
##' @param data trait dataframe that will be appended to.
##' @param column.name name of the covariate as it will appear in the appended column
##' @param covariates.data one or more tables of covariate data, ordered by the precedence
##' they will assume in the event a trait has covariates across multiple tables.
##' All tables must contain an 'id' and 'level' column, at minimum.
##'
##' @author Carl Davidson, Ryan Kelly
##' @export
##--------------------------------------------------------------------------------------------------#
append.covariate <- function(data, column.name, covariates.data){
  # Keep only the highest-priority covariate for each trait
  covariates.data <- covariates.data[!duplicated(covariates.data$trait_id), ]
  
  # Select columns to keep, and rename the covariate column
  covariates.data <- covariates.data[, c('trait_id', 'level')]
  names(covariates.data) <- c('id', column.name)
  
  # Merge on trait ID
  merged <- merge(covariates.data, data, all = TRUE, by = "id")
  return(merged)
}
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Queries covariates from database for a given vector of trait id's
##'
##' @param trait.ids list of trait ids
##' @param con database connection
##' @param ... extra arguments
##'
##' @author David LeBauer
query.covariates <- function(trait.ids, con = NULL, ...){
  covariate.query <- paste("select covariates.trait_id, covariates.level,variables.name",
                           "from covariates left join variables on variables.id = covariates.variable_id",
                           "where trait_id in (", PEcAn.utils::vecpaste(trait.ids), ")")
  covariates <- db.query(query = covariate.query, con = con)
  return(covariates)
}
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Apply Arrhenius scaling to 25 degC for temperature-dependent traits
##'
##' @param data data frame of data to scale, as returned by query.data()
##' @param covariates data frame of covariates, as returned by query.covariates().
##'   Note that data with no matching covariates will be unchanged.
##' @param temp.covariates names of covariates used to adjust for temperature;
##'   if length > 1, order matters (first will be used preferentially)
##' @param new.temp the reference temperature for the scaled traits. Curerntly 25 degC
##' @param missing.temp the temperature assumed for traits with no covariate found. Curerntly 25 degC
##' @author Carl Davidson, David LeBauer, Ryan Kelly
arrhenius.scaling.traits <- function(data, covariates, temp.covariates, new.temp = 25, missing.temp = 25){
  # Select covariates that match temp.covariates
  covariates <- covariates[covariates$name %in% temp.covariates,]
  
  if(nrow(covariates)>0) {
    # Sort covariates in order of priority
    covariates <- do.call(rbind,
                          lapply(temp.covariates, function(temp.covariate) covariates[covariates$name == temp.covariate, ])
    )
    
    data <- append.covariate(data, 'temp', covariates)
    
    # Assign default value for traits with no covariates
    data$temp[is.na(data$temp)] <- missing.temp
    
    # Scale traits
    data$mean <- PEcAn.utils::arrhenius.scaling(observed.value = data$mean, old.temp = data$temp, new.temp=new.temp)
    data$stat <- PEcAn.utils::arrhenius.scaling(observed.value = data$stat, old.temp = data$temp, new.temp=new.temp)
    
    #remove temporary covariate column.
    data<-data[,colnames(data)!='temp']
  } else {
    data <- NULL
  }
  return(data)
}
##==================================================================================================#


##--------------------------------------------------------------------------------------------------#
##' Function to filter out upper canopy leaves
##'
##' @name filter_sunleaf_traits
##' @aliases filter.sunleaf.traits
##' @param data input data
##' @param covariates covariate data
##'
##' @author David LeBauer
filter_sunleaf_traits <- function(data, covariates){
  if(length(covariates)>0) {
    data <- append.covariate(data = data, column.name = 'canopy_layer',
                             covariates.data = covariates[covariates$name == 'canopy_layer',])
    data <- data[data$canopy_layer >= 0.66 | is.na(data$canopy_layer),]
    
    # remove temporary covariate column
    data <- data[,colnames(data)!='canopy_layer']
  } else {
    data <- NULL
  }
  return(data)
}
##==================================================================================================#