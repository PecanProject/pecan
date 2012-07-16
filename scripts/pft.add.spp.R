#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
## M. Dietze
##' adds a list of species to a pft based on USDA Plants acronyms
##'
##' This function is used to add PFT-Species pairs to the database table 'pfts_species'.  In the initial implementation the PFT has to be defined already and the species are added based on their USDA Symbol (genus/species acronym).  Multiple species can be added at once but only one PFT at a time.
##' @title Associate species with a PFT.
##' @param pft String name of the PFT in the database
##' @param acronym USDA Plants Database Symbol. (standard genus-species acronym) see \url{http://plants.usda.gov}
##' @param test  Runs the function in test mode.  No species are actually added, but checks are run for existing species-PFT pairs, unmatched acronyms, missing species, or duplicate species
##' @param con Database connection object.  If not provided will use \code{\link{query.bety.con}} to connect to BETY-db.
##' @param ... optional arguements for connecting to database (e.g. password, user name, database)
##' @return Function does not return a value but does print out diagnostic statements.
##' @author Michael C. Dietze
pft.add.spp <- function(pft,acronym,test=TRUE,con=NULL,...){

  ## establish database connection
  if(is.null(con)){
    con <- query.bety.con(...)
  }

  ## look up pfts.id based on pfts.name
  q = dbSendQuery(con,paste('select * from pfts where name ="',pft,'"',sep=""))
  my.pft = fetch(q,n = -1)
  
  ## if pfts.name does not exist, stop and send error
  if(nrow(my.pft) != 1 ){
    print("PFT did not match uniquely.  Match=")
    print(my.pft)
    print("Similar pfts")
    q = dbSendQuery(con,paste('select * from pfts where name like "%',pft,'%"',sep=""))
    print(fetch(q,n=-1))
    return()
  } else {
    print("using PFT:")
    print(my.pft)
  }
  
  ## loop over acronyms
  for(acro in acronym){
  
    ## look up plant_id based on acronyms
    q = dbSendQuery(con,paste("select id, ScientificName,Symbol from plants where Symbol = '",acro,"'",sep=""))  
    my.plant = fetch(q,n = -1 )
    if(nrow(my.plant) != 1){
      print(c("ACRONYM not matched",acro))
      print(my.plant)
      next()
    }
    
    ## look up species.id based on plant_id
    q = dbSendQuery(con,paste("select * from species where plant_id = '",my.plant$id,"'",sep=""))
    my.species = fetch(q,n=-1)
    if(nrow(my.species) != 1){
      print(c("PLANT_ID not matched",acro,my.plant$id))
      print(my.species)
      next()
    }

    ## look up pfts_species.specie_id to check for duplicates
    q = dbSendQuery(con,paste("select * from pfts_species where pft_id = '",my.pft$id,"' and specie_id = '",my.species$id,"'",sep=""))
    my.pft2spp = fetch(q,n = -1)
    if(nrow(my.pft2spp) > 0){
      print(c("Species already exists for PFT",acro))
      print(my.species)
      next()
    }

    
    ## give list of species
    print(c("ADDING",acro))
    #print(my.plant)
    #print(my.species)
    if(test){
    #  print("TEST ONLY")
      next()
    }
  
    ## if a species is not already in the pft, add
    q = dbSendQuery(con,paste("insert into pfts_species set pft_id = '",my.pft$id,"', specie_id = '",my.species$id,"'",sep=""))
    

  } ## end loop over acronyms

  if(test){
    print("THIS WAS A TEST, NO SPECIES WERE ADDED. SET 'test = FALSE' TO COMMIT CHANGES")
  }

}
