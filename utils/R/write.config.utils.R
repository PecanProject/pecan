#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
### TODO: Generalize this code for all ecosystem models (e.g. ED2.2, SiPNET, etc).

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Returns a vector of quantiles specified by a given <quantiles> xml tag
##'
##' @title Get Quantiles  
##' @param quantiles.tag specifies tag used to specify quantiles
##' @return vector of quantiles
##' @export
##' @author David LeBauer
get.quantiles <- function(quantiles.tag) {
  quantiles<-vector()
  if (!is.null(quantiles.tag$quantile)) {
    quantiles <- as.numeric(quantiles.tag[names(quantiles.tag)=='quantile'])
  }
  if (!is.null(quantiles.tag$sigma)){
    sigmas <- as.numeric(quantiles.tag[names(quantiles.tag)=='sigma'])
    quantiles <- append(quantiles, 1 - pnorm(sigmas))
  }
  if (length(quantiles) == 0) {
    quantiles <- 1-pnorm(-3:3) #default
  }
  if (!0.5 %in% quantiles) {
    quantiles <- append(quantiles, 0.5)
  }
  return(sort(quantiles))
}
##==================================================================================================#

##' get sensitivity samples as a list
##'
##' @title get.sa.sample.list 
##' @param pft Plant Functional Type
##' @param env 
##' @param quantiles quantiles at which to obtain samples from parameter for
##' sensitivity analysis
##' @export
##' @return sa.sample.list
get.sa.sample.list <- function(pft, env, quantiles){
  sa.sample.list <- list()
  for(i in 1:length(pft)){
    sa.sample.list[[i]] = get.sa.samples(pft[[i]], quantiles)
  }
  sa.sample.list[[length(pft)+1]] <- get.sa.samples(env, quantiles)
  names(sa.sample.list) <- c(names(pft), "env")
  return(sa.sample.list)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Samples parameters for a model run at specified quantiles.
##' 
##' Samples from long (>2000) vectors that represent random samples from a trait distribution.
##' Samples are either the MCMC chains output from the Bayesian meta-analysis or are randomly sampled from
##' the closed-form distribution of the parameter probabiolity distribution function.
##' The list is indexed first by trait, then by quantile.
##' @title get sensitivity analysis samples
##' @param samples random samples from trait distribution   
##' @param quantiles list of quantiles to at which to sample, set in settings file
##' @return a list of lists representing quantile values of trait distributions
##' @export
##' @author David LeBauer
get.sa.samples <- function(samples, quantiles){
  sa.samples <- data.frame()
  for(trait in names(samples)){
    for(quantile in quantiles){
      sa.samples[as.character(round(quantile*100,3)), trait] <- quantile(samples[[trait]], quantile)
    }
  }
  return(sa.samples)
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##'   Counter function for writing configs
##' 
##' @title counter 
##' @param cnt 
##' @return updated value of cnt to global environment
##' @export
counter <- function(cnt){
  cnt = cnt + 1
  #return(cnt)
  assign("cnt",cnt,.GlobalEnv) # Assign count to the environment
}
#==================================================================================================#

##' checks that met2model function exists
##'
##' Checks if met2model.<model> exists for a particular
##' model
##' @title met2model.exists
##' @param model model package name
##' @return logical
met2model.exists <- function(model){
  load.modelpkg(model)
  exists(paste0("met2model.",model))  
}

####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
