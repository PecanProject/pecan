#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##'  select plant id's associated with pft
##'
##' @title Query species given pft name
##' @name query.pft_species
##' @param modeltype type of model that is used, this is is used to distinguis between different pfts with the same name.
##' @param pft string pft name
##' @param con database connection
##' @return string of species.id for species associated with pft
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{
##' query.pft_species('ebifarm.pavi')
##' query.pft_species(settings = read.settings("pecan.xml"))
##' }
query.pft_species <- function(pft, modeltype, con){
  # create pft subquery
  if (is.null(modeltype)) {
    query <- paste0("select species.id, species.genus, species.species, species.scientificname",
                    " from species, pfts, pfts_species",
                    " where species.id=pfts_species.specie_id",
                    " and pfts.id=pfts_species.pft_id",
                    " and pfts.name='", pft, "'")
  } else {
    query <- paste0("select species.id, species.genus, species.species, species.scientificname",
                    " from species, pfts, pfts_species, modeltypes",
                    " where species.id=pfts_species.specie_id",
                    " and pfts.id=pfts_species.pft_id",
                    " and pfts.name='", pft, "'",
                    " and pfts.modeltype_id=modeltypes.id",
                    " and modeltypes.name='", modeltype, "'")
  }
  print(query)

  species <- db.query(query, con)
  invisible(species)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
