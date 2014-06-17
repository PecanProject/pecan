#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Query priors associated with a string of traits and plant functional type
##'
##' @name query.priors
##' @title Query Priors
##' @param pft String name of the PFT in the database
##' @param trstr string of traits to query priors for
##' @param out output location
##' @param con database connection, can be list of arguments for connecting to database
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return priors for a given pft
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{
##' query.priors('ebifarm.pavi', vecpaste('SLA', 'Vcmax', 'leaf_width'))
##' }
query.priors <- function(pft, trstr=NULL, out=NULL, con=NULL,...){
  if(is.null(con)){
    con <- db.open(settings$database)
  }
  if(is.list(con)){
    print("query.priors")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  }
  
  query.text <- paste("select variables.name, distn, parama, paramb, n",
      "from priors",
      "join variables on priors.variable_id = variables.id",
      "join pfts_priors on pfts_priors.prior_id = priors.id",
      "join pfts on pfts.id = pfts_priors.pft_id",
      "where pfts.name in (", vecpaste(pft), ")")
  if(is.null(trstr) || trstr == "''"){
    query.text = paste(query.text,";",sep="")
  } else {
    query.text = paste(query.text,"and variables.name in (", trstr, ");")
  }
  
  priors <- db.query(query.text, con)
  
  if(nrow(priors) <= 0){
    warning(paste("No priors found for pft(s): ", pft))
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }
  else {    
    rownames(priors) <- priors$name
    priors <- priors[, which(colnames(priors)!='name')]
    return(priors)
  }
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.        			
####################################################################################################
