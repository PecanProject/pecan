#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' 
##'
##'
##'
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' 
##'
##'
##'
##'
#--------------------------------------------------------------------------------------------------#
query.data<-function(trait, spstr, extra.columns='', con=query.base.con(...), ...){
  query <- paste("select 
            traits.id, traits.citation_id, traits.site_id, treatments.name, 
                 traits.date, traits.time, traits.cultivar_id, traits.specie_id,
                 traits.mean, traits.statname, traits.stat, traits.n, 
                 variables.name as vname,
                 month(traits.date) as month,",
            extra.columns,
                 "treatments.control, sites.greenhouse
          from traits 
                 left join treatments on  (traits.treatment_id = treatments.id) 
                 left join sites on (traits.site_id = sites.id) 
                 left join variables on (traits.variable_id = variables.id) 
                 where specie_id in (", spstr,") 
                 and variables.name in ('", trait,"');", sep = "")
  return(fetch.stats2se(con, query))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' 
##'
##'
#--------------------------------------------------------------------------------------------------#
query.yields <- function(trait = 'yield', spstr, extra.columns='', con=query.base.con(...), ...){
  query <- paste("select 
            yields.id, yields.citation_id, yields.site_id, treatments.name, 
                 yields.date, yields.time, yields.cultivar_id, yields.specie_id,
                 yields.mean, yields.statname, yields.stat, yields.n, 
                 variables.name as vname,
                 month(yields.date) as month,",
                   extra.columns,
                 "treatments.control, sites.greenhouse
          from yields 
                 left join treatments on  (yields.treatment_id = treatments.id) 
                 left join sites on (yields.site_id = sites.id) 
                 left join variables on (yields.variable_id = variables.id) 
                 where specie_id in (", spstr,");", sep = "")
    if(!trait == 'yield'){
      query <- gsub(");", paste(" and variables.name in ('", trait,"');", sep = ""), query)
    }
  
  return(fetch.stats2se(con, query))
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' 
##' @name query.covariates
##'
##' \code{query.covariates} queries covariates from database for a given vector of trait id's
##' 
##' @param trait.ids list of trait ids
##'
##'
#--------------------------------------------------------------------------------------------------#
query.covariates<-function(trait.ids, con = query.bety.con(...), ...){
  covariate.query <- paste("select covariates.trait_id, covariates.level,variables.name",
                           "from covariates left join variables on variables.id = covariates.variable_id",
                           "where trait_id in (",vecpaste(trait.ids),")")
  q <- dbSendQuery(con, covariate.query)
  covariates = fetch(q, n = -1)  
  return(covariates)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Append covariate data as a column within a table
##' @name append.covariate
##'
##' \code{append.covariate} appends one or more tables of covariate data 
##' as a single column in a given table of trait data.
##' In the event a trait has several covariates across several given tables, 
##' the first table given will take precedence
##'
##' @param data trait dataframe that will be appended to.
##' @param column.name name of the covariate as it will appear in the appended column
##' @param covariates.data one or more tables of covariate data, ordered by the precedence 
##' they will assume in the event a trait has covariates across multiple tables.
##' All tables must contain an 'id' and 'level' column, at minimum. 
#--------------------------------------------------------------------------------------------------#
append.covariate<-function(data, column.name, ..., covariates.data=list(...)){
  merged <- data.frame()
  for(covariate.data in covariates.data){
    if(length(covariate.data)>1){
      #conditional added to prevent crash when trying to transform an empty data frame
      transformed <- transform(covariate.data, id = trait_id, level = level)
      selected <- transformed[!transformed$id %in% merged$id, c('id', 'level')]
      merged <- rbind(merged, selected)
    }
  }
  colnames(merged) <- c('id', column.name)
  merged <- merge(merged, data, all = TRUE)
  return(merged)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' 
##' @name rename.jags.columns
##'
##' \code{rename.jags.columns} renames the variables within output data frame trait.data
##' 
##' @param data data frame to with variables to rename
##' 
##' @seealso used with \code{\link{jagify}};
#--------------------------------------------------------------------------------------------------#
rename.jags.columns <- function(data) {
  
  # Change variable names and calculate obs.prec within data frame
  transformed <-  transform(data,
                            Y        = mean,
                            se       = stat,
                            obs.prec = 1 / (sqrt(n) * stat) ^2,
                            trt      = trt_id,
                            site     = site_id,
                            cite     = citation_id,
                            ghs      = greenhouse)
  
  # Subset data frame
  selected <- subset(transformed, select = c('Y', 'n', 'site', 'trt', 'ghs', 'obs.prec', 
                                             'se', 'cite'))
  # Return subset data frame
  return(selected)
}
#==================================================================================================#

# below is the main trait query that needs to be here.  Transforms should be
# applied on the save trait data in the next step

#--------------------------------------------------------------------------------------------------#
##' Extract trait data from BETYdb
##' @name query.trait.data
##'
##' \code{query.bety.trait.data} extracts data from BETYdb for a given trait and set of species,
##' converts all statistics to summary statistics, and prepares a dataframe for use in meta-analysis.
##' For Vcmax and SLA data, only data collected between  April and July are queried, and only data collected from the top of the canopy (canopy height > 0.66).
##' For Vcmax and root_respiration_rate, data are scaled
##' converted from measurement temperature to \eqn{25^oC} via the arrhenius equation.
##'
##' @param trait is the trait name used in BETY, stored in variables.name
##' @param spstr is the species.id integer or string of integers associated with the species
##'  
##' @return dataframe ready for use in meta-analysis
##' @examples
##' query.bety.trait.data("Vcmax", "938", con = newcon())
#--------------------------------------------------------------------------------------------------#
query.trait.data.new <- function(trait, spstr,con=query.base.con(...), ...){
  
  if(is.list(con)){
    print("query.bety.trait.data")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  } 
  print(trait)
  
  ### Query the data for each spp
  data <- query.data(trait, spstr, con=con)
  ### Query relevant covariates for each trait
  covariates <- query.covariates(data$id, con=con)
  
  #----------------------- Trait specific filtering below -----------------------#
  
  ### Vcmax 
  if(trait == 'Vcmax') {
    
    ## select only summer data for Panicum virgatum
    ## TODO fix following hack to select only summer data
    if (spstr == "'938'"){
      data <- subset(data, subset = data$month %in% c(0,5,6,7))
    }
    
  } ### End if/else for trait filtering
    
    #------------------------------- Setup output -------------------------------#
    
    ### Create new data frame with queried data.
    result <- data

    ## if result is empty, stop run
    if(nrow(result)==0) {
      return(NA)
      warning(paste("there is no data for", trait))
    } else {
      
      ### Display diagnostic info to the console/terminal
      
      # Do we really want to print each trait table?? Seems like a lot of
      # info to send to console.  Maybe just print summary stats?  sps
      #print(result)
      print(paste("Mean ",trait," : ",round(mean(result$mean),digits=3),sep=""))
      return(result)
      
      ### !!! Remove code below and place in transform.trait.data.R   sps
      # Convert to format applicable for JAGS meta-analysis. remove from this script file
      #     jagged <- jagify(result)
      #     renamed <- rename.jags.columns(jagged)
      #     return(renamed)
      ### !!!
    }
} ### End of query.trait.data
#==================================================================================================#

      
####################################################################################################
### EOF.  End of R script file.              
####################################################################################################