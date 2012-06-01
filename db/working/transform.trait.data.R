#--------------------------------------------------------------------------------------------------#
##'
##' Transforms statistics to SE for data queried from the database and scales relevant traits to a common temperature
##'
##' @name transform.trait.data.R
##'
##
##'
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' 
##' @name transform.nas
##'
##'
##'
##'
#--------------------------------------------------------------------------------------------------#
transform.nas <- function(data){
  #control defaults to 1
  data$control[is.na(data$control)]     <- 1
  
  #site defaults to 0
  #TODO assign different site for each citation - dsl
  data$site_id[is.na(data$site_id)] <- 0
  
  #greenhouse defaults to false (0)
  data$greenhouse[is.na(data$greenhouse)] <- 1
  
  #number of observations defaults to 2 for statistics, 1 otherwise
  data$n[is.na(data$n)] <- 1
  data$n[data$n ==1 & !is.na(data$stat)] <- 2
  
  return(data)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Queries data from BETY and transforms statistics to SE
##'
##' Performs query and then uses \code{transformstats} to convert miscellaneous statistical summaries
##' to SE
##' @title Fetch data and transform stats to SE
##' @param connection connection to BETYdb
##' @param query MySQL query to traits table
##' @return dataframe with trait data
##' @seealso used in \code{\link{query.bety.trait.data}}; \code{\link{transformstats}} performs transformation calculations
#--------------------------------------------------------------------------------------------------#
fetch.stats2se <- function(connection, query){
  query.result <- dbSendQuery(connection, query)
  transformed <- transformstats(fetch(query.result, n = -1))
  return(transformed)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' 
##' @name filter.sunleaf.traits
##'
##'
##'
##'
#--------------------------------------------------------------------------------------------------#
filter.sunleaf.traits <- function(data, covariates){
  if(length(covariates)>0) {  
    data <- append.covariate(data, 'canopy_layer', 
                             covariates[covariates$name == 'canopy_layer',])
    data <-  data[data$canopy_layer >= 0.66 | is.na(data$canopy_layer),]
    #remove temporary covariate column
    data<-data[,colnames(data)!='canopy_layer']
  }
  return(data)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' 
##' @name arrhenius.scaling.traits
##'
##'
##'
##'
#--------------------------------------------------------------------------------------------------#
arrhenius.scaling.traits <- function(data, covariates, temp.covariates, new.temp=25){
  if(length(covariates)>0) {
    data <- append.covariate(data, 'temp', 
                             covariates.data=lapply(temp.covariates, 
                                                    function(temp.covariate)
                                                      {covariates[covariates$name == 
                                                        temp.covariate,]}))
    
    data$temp[is.na(data$temp)] <-  new.temp
    
    data$mean <- arrhenius.scaling(data$mean, old.temp = data$temp, new.temp=new.temp)
    data$stat <- arrhenius.scaling(data$stat, old.temp = data$temp, new.temp=new.temp)
    
    # Remove covariate column from trait.data
    data<-data[,colnames(data)!='temp']
  }
  return(data)
}
#==================================================================================================#
















