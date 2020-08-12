library(dplyr)

#' Retrieve the details of a PEcAn PFT, based on pft_id
#' @param pft_id PFT ID (character)
#' @return PFT details
#' @author Tezan Sahu
#* @get /<pft_id>
getPfts <- function(pft_id, res){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  pft <- tbl(dbcon, "pfts") %>%
    select(pft_id = id, pft_name = name, definition, pft_type, modeltype_id) %>%
    filter(pft_id == !!pft_id)
  
  pft <- tbl(dbcon, "modeltypes") %>%
    select(modeltype_id = id, model_type = name) %>%
    inner_join(pft, by = "modeltype_id")
  
  qry_res <- pft %>% 
    select(-modeltype_id) %>% 
    collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="PFT not found"))
  }
  else {
    # Convert the response from tibble to list
    response <- list()
    for(colname in colnames(qry_res)){
      response[colname] <- qry_res[colname]
    }
    
    return(response)
  }
}

#########################################################################

#' Search for PFTs containing wildcards for filtering
#' @param pft_name PFT name search string (character)
#' @param pft_type PFT type (either 'plant' or 'cultivar') (character)
#' @param model_type Model type serch string (character)
#' @param ignore_case Logical. If `TRUE` (default) use case-insensitive search otherwise, use case-sensitive search
#' @return PFT subset matching the searc criteria
#' @author Tezan Sahu
#* @get /
searchPfts <- function(pft_name="", pft_type="", model_type="", ignore_case=TRUE, res){
  pft_name <- URLdecode(pft_name)
  pft_type <- URLdecode(pft_type)
  model_type <- URLdecode(model_type)
  
  if(! pft_type %in% c("", "plant", "cultivar")){
    res$status <- 400
    return(list(error = "Invalid pft_type"))
  }
  
  dbcon <- PEcAn.DB::betyConnect()
  
  pfts <- tbl(dbcon, "pfts") %>%
    select(pft_id = id, pft_name = name, pft_type, modeltype_id)
  
  pfts <- tbl(dbcon, "modeltypes") %>%
    select(modeltype_id = id, model_type = name) %>%
    inner_join(pfts, by = "modeltype_id")
  
  qry_res <- pfts %>% 
    filter(grepl(!!pft_name, pft_name, ignore.case=ignore_case)) %>%
    filter(grepl(!!pft_type, pft_type, ignore.case=ignore_case)) %>%
    filter(grepl(!!model_type, model_type, ignore.case=ignore_case)) %>%
    select(-modeltype_id) %>%
    arrange(pft_id) %>%
    collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="PFT(s) not found"))
  }
  else {
    return(list(pfts=qry_res, count = nrow(qry_res)))
  }
}