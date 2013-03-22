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
##' @param ... optional arguments for connecting to database (e.g. password, user name, database)
##' @return string of species.id for species associated with pft
##' @export
##' @author David LeBauer
##' @examples
##' \dontrun{
##' query.pft_species('ebifarm.pavi')
##' }
query.pft_species <- function(pft,con=NULL,...){
  if(is.null(con)){
    con <- query.base.con(settings)
  }
  query <- paste("select id, genus, species.species from species 
                 where id in ( select specie_id from pfts_species 
                 where pft_id = (select id from pfts where name = '",pft,"'));", sep = "")
  species <- db.query(query, con)
  print(" ")
  print("-------------------------------------------------------------------")
  print(paste("List of species included in PFT: ",pft,sep=""))
  print(species)
  print("-------------------------------------------------------------------")
  print(" ")
  print(" ")
  spstr <- vecpaste(species$id)
  return(spstr)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
