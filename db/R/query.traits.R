#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Query available trait data associated with a given pft and a list of traits
##' 
##' @name query.traits
##' @title Query trait data
##' @param spstr string of species id's from trait database
##' @param priors vector of parameters for which priors have been specified 
##' @param con 
##' @return dataframe with trait data
##' @seealso \code{\link{query.trait.data}}
##' @export
##' @examples
##' \dontrun{
##' species <- query.pft_species('ebifarm.c4crop')
##' spstr <- vecpaste(species$id)
##' trvec <- c('leafN', 'SLA')
##' trait.data <- query.traits(spstr, trvec)
##' }
##' @author David LeBauer, Carl Davidson, Shawn Serbin
query.traits <- function(spstr, priors, con = NULL){

  if(is.null(con)){
    con <- db.open(settings$database$bety)
  }
  if(is.list(con)){
    print("query.traits")
    print("WEB QUERY OF DATABASE NOTE IMPLEMENTED")
    return(NULL)
  }
  
  query <- paste("select distinct variables.name from traits join variables 
                 on (traits.variable_id = variables.id) where specie_id in (", spstr,");", sep = "")
  traits <- db.query(query, con)$name
  traits <- unique(traits[traits %in% priors])
  
  ### Grab trait data
  trait.data <- lapply(traits, function(trait) query.trait.data(trait, spstr, con=con))
  names(trait.data) <- traits
  return(trait.data)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
