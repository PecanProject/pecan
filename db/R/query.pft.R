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
##' @param pft string pft name
##' @param con database connection
##' @param settings list of settings that may provide pft name and database settings, 
##' if  pft and / or con are null
##' @return string of species.id for species associated with pft
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{
##' query.pft_species('ebifarm.pavi')
##' query.pft_species(settings = read.settings("pecan.xml"))
##' }
query.pft_species <- function(pft = NULL, con = NULL, settings = NULL){

  if(exists("settings")){
    if(is.null(con)){
      con <- db.open(settings$database) 
      newcon <- TRUE
    } else { 
      newcon <- FALSE
    }
    if(is.null(pft)){
      pft <- settings$pfts$pft$name
    }
  } 
  
  query <- paste0("select id, genus, species.species, species.scientificname ",
                 "from species ", 
                 "where id in (select specie_id from pfts_species ",
                 "where pft_id = (select id from pfts where name = '", pft,"'));")
  species <- db.query(query, con)
  if(newcon) db.close(con)
  invisible(species)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
