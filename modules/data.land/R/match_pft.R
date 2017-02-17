##' Match model PFTs
##' 
##' Matches BETYdb species IDs to model-specific PFTs
##' 
##' @param bety_species_id  vector of BETYdb species IDs
##' @param pfts             settings$pfts.  List of pfts with database matching based on name
##' @param con              database connection
##' 
##' @return table of BETYdb PFT IDs matched to species IDs
##' 
match_pft <- function(bety_species_id, pfts, con){
  
  ### get species to PFT mappting
  query <- NULL
  for (pft in pfts) {
    if (is.null(query)) {
      query <- paste0("SELECT bp.id as bety_pft_id, bp.name as pft, bs.id as bety_species_id, bs.scientificname as latin FROM pfts as bp INNER JOIN ", 
                      "pfts_species AS bps ON bps.pft_id = bp.id INNER JOIN species AS bs ON bs.id = bps.specie_id WHERE ", 
                      "bp.name = '", pft$name, "'")
    } else {
      query <- paste0(query, " OR bp.name = '", pft$name, "'")
    }
  }
  translation <- db.query(query, con = con)
  
  ## Check for duplicate bety_species_ids in PFTs
  bad <- translation[duplicated(translation$bety_species_id),]
  if (length(bad) > 0) {
    for(i in seq_along(bad)){
      PEcAn.utils::logger.warning(paste0("Duplicated species: ",paste(bad[i,], collapse = ", "), latin))
    }
  }

  ## Check for unmatched bety_species_ids
  bad2 <- bety_species_id[!(bety_species_id %in% translation$bety_species_id)]
  if (length(bad2) > 0) {
    for(i in seq_along(bad)){
      # Coerce id back into species names. Makes a more readable warning.
      latin <- db.query(paste("SELECT scientificnname FROM species where id =",bad2$bety_species_id[i]), con = con)
      
      PEcAn.utils::logger.warning(paste0("Unmatched species: ",bad2[i], latin))
    }
  }
  
  ## stop after checking both errors
  if (length(bad) > 0 | length(bad2) > 0) {
    PEcAn.utils::logger.severe("Within BETY PFT table, please address duplicated species and add unmatched species to PFTs.")
  }  

  ## Match
  return(merge(translation, bety_species_id, by = "bety_species_id"))
  
}
