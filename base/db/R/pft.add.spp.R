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
##' @param con Database connection object.
##' @param ... optional arguements for connecting to database (e.g. password, user name, database)
##' @return Function does not return a value but does print out diagnostic statements.
##' @author Michael C. Dietze, Dongchen Zhang
pft.add.spp <- function(pft, acronym, test = TRUE, con = NULL, ...) {
  
  ## establish database connection
  # default points to psql-pecan.bu.edu.
  if (is.null(con)) {
    bety <- dplyr::src_postgres(dbname   = "bety",
                                host     = "psql-pecan.bu.edu",
                                user     = "bety",
                                password = "bety")
    con <- bety$con
  }
  
  #detect if acronym is ID or Symbol
  if(is.character(acronym[1])){
    ID_flag <- F
  }else if(is.numeric(acronym[1])){
    ID_flag <- T
  }else{
    print("can't detect the type of Acronym, please check it!!!")
    return(0)
  }
  
  ## look up pfts.id based on pfts.name
  pft_qry <- glue::glue_sql(paste0("select * from pfts where name = '", pft, "'"), .con = con)
  my.pft <- PEcAn.DB::db.query(con = con, query = pft_qry)
  
  ## if pfts.name does not exist, stop and send error
  if(nrow(my.pft) > 1){
    print("More than one pft matched!!! Please check your pft name and make sure it is unique to all other pfts!!!")
    return(my.pft)
  }else if(nrow(my.pft) == 0){
    print("No pft founded that matched the name!!! Please check your pft name!!!")
    return(0)
  }
  
  #initialize inputid to store IDs
  inputid <- c()
  
  #initialize bad to store any problematic items.
  bad_species <- c()
  bad_pft_speceis <- c()
  
  ## loop over acronyms
  for (acro in acronym) {
    ## look up species based on acronyms. (can be either Symbols or IDs)
    if(ID_flag){
      species_qry <- glue::glue_sql(paste0("select * from species where \"id\" = '", acro, "'"), .con = con)
    }else{
      species_qry <- glue::glue_sql(paste0("select * from species where \"Symbol\" = '", acro, "'"), .con = con)
    }
    my.species <- PEcAn.DB::db.query(con = con, query = species_qry)

    #if species not matched with bety records
    if (nrow(my.species) != 1) {
      print(c("ACRONYM not matched", acro))
      print(my.species)
      bad_species <- c(bad_species, acro)
      next
    }
    
    ## look up pfts_species.specie_id to check for duplicates
    species_pft_qry <- glue::glue_sql(paste0("select * from pfts_species where pft_id = '", my.pft$id,"' and specie_id = '",my.species$id ,"'"), .con = con)
    pft_species <- PEcAn.DB::db.query(con = con, query = species_pft_qry)
    
    #if record already exists
    if (nrow(pft_species) > 0) {
      print(c("Species already exists for PFT", acro))
      print(pft_species)
      bad_pft_speceis <- c(bad_pft_speceis, paste0("Specie_ID: ", my.species$id, " . pft_ID: ", my.pft$id, "."))
      next
    }
    
    ## give list of species
    print(c("ADDING", acro))
    # print(my.species) print(my.species)
    if (test) {
      print('TEST ONLY')
      if(nrow(pft_species) == 0 && nrow(my.species) == 1 && nrow(my.pft) == 1){
        print("pft exists and unique; specie exists and unique; pft_species does not exists; Therefore, it's ready to go!!!")
        print(acro)
      }
      next
    }
    
    ## if a species is not already in the pft, add
    cmd <- paste0(
      "INSERT INTO pfts_species ",
      "(pft_id, specie_id) VALUES ('",my.pft$id, "', '", my.species$id, "') RETURNING id"
    )
    # This is the id that we just registered
    inputid <- c(PEcAn.DB::db.query(query = cmd, con = con), inputid)
    
  }  ## end loop over acronyms
  return(list(input_ID = inputid, bad_species = bad_species, bad_pft_speceis = bad_pft_speceis))
  
} # pft.add.spp
