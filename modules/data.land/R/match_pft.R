##' Match model PFTs
##' 
##' Matches BETYdb species IDs to model-specific PFTs
##' 
##' @param bety_species_id  vector of BETYdb species IDs
##' @param pfts             settings$pfts.  List of pfts with database matching based on name
##' @param con              database connection, if NULL use traits package
##' @param allow_missing    flag to indicate that settings file does not need to match exactly
##' 
##' @author Mike Dietze, Istem Fer
##' @return table of BETYdb PFT IDs matched to species IDs
##' 
##' @export
match_pft <- function(bety_species_id, pfts, query = NULL, con = NULL, allow_missing = FALSE){
  
  ### get species to PFT mappting
  if(!is.null(con)){
    
    for (pft in pfts) {
      if (is.null(query)) {
        query <- paste0("SELECT bp.id as bety_pft_id, bp.name as pft, bs.id as bety_species_id, bs.scientificname as latin FROM pfts as bp INNER JOIN ", 
                        "pfts_species AS bps ON bps.pft_id = bp.id INNER JOIN species AS bs ON bs.id = bps.specie_id WHERE ", 
                        "bp.name = '", pft$name, "'")
      } else {
        query <- paste0(query, " OR bp.name = '", pft$name, "'")
      }
    }
    translation <- PEcAn.DB::db.query(query, con = con)
    
    
  }else{ # use traits package
    
    bety_list <- list()
    
    for (pft in pfts) {
      # query pft id
      bety_pft <- traits::betydb_query(name = pft$name, table = 'pfts', user = 'bety', pwd = 'bety')
      # query species id
      bety_species <- traits::betydb_query(pft_id = bety_pft$id, table = 'pfts_species', user = 'bety', pwd = 'bety')
      bety_list[[pft$name]] <- bety_species$specie_id
    }
    tmp <- lapply(seq_along(bety_list), function(x){
        data.frame(pft = rep(names(bety_list)[x], length(bety_list[[x]])),
                   bety_species_id = bety_list[[x]])})
      
    translation <- do.call("rbind", tmp)
    
  }

  ## Check for duplicate bety_species_ids in PFTs
  bad <- translation[duplicated(translation$bety_species_id),]
  if (nrow(bad) > 0) {
    for(i in seq_along(nrow(bad))){
      error.pft <- translation[translation$bety_species_id == bad$bety_species_id[i],]
      PEcAn.logger::logger.warn(paste0("Duplicated species id: ", bad$bety_species_id[i], " under ", paste(error.pft$pft, collapse = ", ")))
    }
  }

  ## Check for unmatched bety_species_ids
  bad2 <- bety_species_id[!(bety_species_id %in% translation$bety_species_id)]
  
  # skip dead tree codes for 2TB, SNAG, DEAD
  dead_tree_ids <- c(1000020816, 1000020817, 1438)
  bad2 <- bad2[!(bad2 %in% dead_tree_ids)] 
  
  if (length(bad2) > 0) {
    ubad <- unique(bad2)
    for(i in seq_along(ubad)){
      # Coerce id back into species names. Makes a more readable warning.
      if(!is.na(ubad[i])){
        if(!is.null(con)){
          latin <- PEcAn.DB::db.query(paste("SELECT scientificname FROM species where id =", ubad[i]), con = con)
        }else{ # use traits package
          bety_latin <- traits::betydb_query(id = ubad[i], table = 'species', user = 'bety', pwd = 'bety')
          latin      <- bety_latin$scientificname
        }
        
      }else{
        latin <- NA
      }
      PEcAn.logger::logger.warn(paste0("Unmatched species: ", ubad[i]," ", latin))
    }
  }
  
  ## stop after checking both errors
  if (nrow(bad) > 0) {
    PEcAn.logger::logger.severe("Within BETY PFT table, please address duplicated species and add unmatched species to PFTs.")
  }
  
  if(allow_missing == FALSE & length(bad2) > 0){
    PEcAn.logger::logger.severe("Within BETY PFT table, please address duplicated species and add unmatched species to PFTs.")
  }

  ## Match
  matchedpft <- dplyr::right_join(translation,  as.data.frame(bety_species_id), type="right")

  return(matchedpft)
  
}
