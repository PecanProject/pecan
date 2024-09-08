## M. Dietze
##' adds a list of species to a pft based on USDA Plants acronyms
##'
##' This function is used to add PFT-Species pairs to the database table 'pfts_species'.  In the initial implementation the PFT has to be defined already and the species are added based on their USDA Symbol (genus/species acronym).  Multiple species can be added at once but only one PFT at a time.
##' @title Associate species with a PFT.
##' @param pft String name of the PFT in the database
##' @param acronym Specie's Symbols. see \url{http://plants.usda.gov}
##' @param ID Species IDs in Bety. You can provide either IDs or Symbols as input, if you provide both ID and acronym, only acronym will be used.
##' @param test  Runs the function in test mode.  No species are actually added, but checks are run for existing species-PFT pairs, unmatched acronyms, missing species, or duplicate species
##' @param con Database connection object.
##' @param ... optional arguements for connecting to database (e.g. password, user name, database)
##' @return Function does not return a value but does print out diagnostic statements.
##' @details 
##' The Symbols object are 
##' @author Michael C. Dietze, Dongchen Zhang
pft.add.spp <- function(pft, acronym = NULL, ID = NULL, test = TRUE, con = NULL, ...) {
  
  ## establish database connection
  # default points to psql-pecan.bu.edu.
  if (is.null(con)) {
    con <- PEcAn.DB::db.open(...)
    on.exit(PEcAn.DB::db.close(con), add = TRUE)
  }
  
  #detect if we input Symbol or IDs
  if(!is.null(acronym)){
    Species_elements <- acronym
    print("Input is Symbol!")
  }else if(!is.null(ID)){
    Species_elements <- ID
    print("Input is ID!")
  }else{
    print("No IDs or Symbols imported!, Please check the data input!")
    return(0)
  }
  
  ## look up pfts.id based on pfts.name
  pft_qry <- glue::glue_sql(paste0("select * from pfts where name = '", pft, "'"), .con = con)
  my.pft <- PEcAn.DB::db.query(con = con, query = pft_qry)
  
  ## if pfts.name does not exist, stop and send error
  if(nrow(my.pft) > 1){
    print("More than one pft matched!!! Please check your pft name and make sure it is unique to all other pfts!!!")
    
    #find similar pfts that might match and return it
    similar_pft_query <- glue::glue_sql(paste0("select * from pfts where name like \'%", pft, "%\'"), .con = con)
    similar_pfts <- PEcAn.DB::db.query(con = con, query = similar_pft_query)
    print("similar pfts are returned, please check that!!!")
    return(similar_pfts)
  }else if(nrow(my.pft) == 0){
    print("No pft founded that matched the name!!! Please check your pft name!!!")
    return(0)
  }
  
  #initialize inputid to store IDs
  inputid <- c()
  
  #initialize bad to store any problematic items.
  bad_species <- c()
  bad_pft_species <- c()
  
  ## loop over acronyms or IDs
  for (acro in Species_elements) {
    ## look up species based on acronyms. (can be either Symbols or IDs)
    if(!is.null(ID)){
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
      bad_pft_species <- c(bad_pft_species, paste0("Specie_ID: ", my.species$id, " . pft_ID: ", my.pft$id, "."))
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
  return(list(input_ID = inputid, bad_species = bad_species, bad_pft_species = bad_pft_species))
  
} # pft.add.spp
