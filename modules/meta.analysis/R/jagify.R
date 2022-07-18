#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##'
##' Convert queried data to format required by JAGS meta-analysis model 
##'
##' @name jagify
##' @title Prepare trait data for JAGS meta-analysis
##' @param result input trait data
##' @param use_ghs (Logical) If `FALSE`, exclude all greenhouse data. If `TRUE`, use all data, including greenhouse data.
##' @return result transformed to meet requirements of PEcAn meta-analysis model
##' @export
##' @author David LeBauer
jagify <- function(result, use_ghs = TRUE) {
  

  ## Create new column "trt_id" from column 'name'.  Remove NAs. Assign treatments.
  ## Finally, summarize the results by calculating summary statistics from experimental replicates
  r <- result[!is.na(result$mean), ]
  r$trt_id <- r$name
  r <- transform.nas(r)
  
  # exclude greenhouse data unless requested otherwise
  if(!use_ghs){
    r <- r[r$greenhouse != 1, ]
  }
  
  r <- PEcAn.DB::assign.treatments(r)
  r <- PEcAn.utils::summarize.result(r)
  r <- subset(transform(r, 
                        stat = as.numeric(stat), 
                        n = as.numeric(n), 
                        site_id = as.integer(factor(site_id, unique(site_id))), 
                        greenhouse = as.integer(factor(greenhouse, unique(greenhouse))),
                        mean = mean, 
                        citation_id = citation_id,
                        ghs = greenhouse,
                        site = site_id,
                        trt_name = name), 
              select = c("stat", "n", "site_id", "trt_id", "mean", "citation_id", "greenhouse", 
                         "ghs", "treatment_id", "site", "trt_name")) # original versions of greenhouse, treatment_id, site_id, and name
  
  #order by site_id and trt_id, but make sure "control" is the first trt of each site
  uniq <- setdiff(unique(r$trt_id), "control")
  r$trt_id <- factor(r$trt_id, levels = c("control", uniq[order(uniq)]))
  r <- r[order(r$site_id, r$trt_id), ]
  
  #add beta.trt index associated with each trt_id (performed in single.MA, replicated here for matching purposes)
  r$trt_num <- as.integer(factor(r$trt_id, levels = unique(r$trt_id)))
  
  if (length(r$stat[!is.na(r$stat) & r$stat <= 0]) > 0) {
    varswithbadstats <- unique(result$vname[which(r$stat <= 0)])
    citationswithbadstats <- unique(r$citation_id[which(r$stat <= 0)])
    
    PEcAn.logger::logger.warn("there are implausible values of SE: SE <= 0 \n",
                "for", varswithbadstats, 
                "result from citation", citationswithbadstats, "\n", 
                "SE <=0 set to NA \n")
    r$stat[r$stat <= 0] <- NA
  }
  
  rename_jags_columns(r)
} # jagify
# ==================================================================================================#


##' Function to remove NA values from database queries
##' 
##' Transform NA values in data exported from BETYdb
##' 
##' @name transform.nas
##' @param data input data
##' 
##' @return A data frame NAs sensibly replaced 
transform.nas <- function(data) {
  #set stat to NA if 0 (uncertainties can only asymptotically approach 0)
  data$stat[data$stat == 0] <- NA
  
  # control defaults to 1
  data$control[is.na(data$control)] <- 1
  
  # site defaults to 0 TODO assign different site for each citation - dsl
  data$site_id[is.na(data$site_id)] <- 0
  
  # greenhouse defaults to false (0)
  data$greenhouse[is.na(data$greenhouse)] <- 0
  
  # number of observations defaults to 2 for statistics, 1 otherwise
  data$n[is.na(data$n)] <- 1
  data$n[data$n == 1 & !is.na(data$stat)] <- 2
  
  return(data)
} # transform.nas
