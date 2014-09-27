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
##' Convert queried data to format required by JAGS meta-analysis model 
##'
##' @name jagify
##' @title Prepare trait data for JAGS meta-analysis
##' @param result input trait data
##' @return result transformed to meet requirements of PEcAn meta-analysis model
##' @export
##' @author David LeBauer
jagify <- function(result){
  
  ## Rename 'name' column from 'treatment' table to trt_id.
  ## Remove NAs. Assign treatments.
  ## Finally, summarize the results by calculating summary statistics from experimental 
  ## replicates
  r <- result[!is.na(result$mean),] 
  colnames(r)[colnames(r)=="name"] <- 'trt_id'
  r <- transform.nas(r)
  r <- assign.treatments(r)
  r <- summarize.result(r)
  r$greenhouse[is.na(r$greenhouse)] <- 0
  r <- subset(transform(r,
                        stat = as.numeric(stat),
                        n    = as.numeric(n),
                        site_id = as.integer(factor(site_id, unique(site_id))),
                        greenhouse = as.integer(factor(greenhouse, unique(greenhouse))),
                        mean = mean,
                        citation_id = citation_id), 
              select = c('stat', 'n', 'site_id', 'trt_id', 'mean', 'citation_id', 'greenhouse'))
  
  if(length(r$stat[!is.na(r$stat) & r$stat <= 0.0]) > 0) {
    citationswithbadstats <- unique(r$citation_id[which(r$stat <= 0.0)])
    logger.warn('there are implausible values of SE: SE <= 0 \n',
                'for', names(result)[i], 'result from citation',
                citationswithbadstats,'\n',
                'SE <=0 set to NA \n')
    r$stat[r$stat <= 0.0] <- NA
  } 
  r <- rename.jags.columns(r)
  return(r)
  
} ### End of function
#==================================================================================================#

##' Function to remove NA values from database queries
##' 
##' Transform NA values in data exported from BETYdb
##' 
##' @name transform.nas
##' @param data input data
##' 
##' @return A data frame NAs sensibly replaced 
transform.nas <- function(data){
  #control defaults to 1
  data$control[is.na(data$control)] <- 1
  
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
####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
