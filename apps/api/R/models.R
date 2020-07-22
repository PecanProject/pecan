library(dplyr)

#' Retrieve the details of a PEcAn model, based on model_id
#' @param model_id Model ID (character)
#' @return Model details
#' @author Tezan Sahu
#* @get /<model_id>
getModel <- function(model_id, res){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  Model <- tbl(dbcon, "models") %>%
    select(model_id = id, model_name, revision, modeltype_id) %>%
    filter(model_id == !!model_id)
  
  Model <- tbl(dbcon, "modeltypes") %>%
    select(modeltype_id = id, model_type = name) %>%
    inner_join(Model, by = "modeltype_id")
  
  qry_res <- Model %>% collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Model not found"))
  }
  else {
    return(qry_res)
  }
}

#########################################################################

#' Search for PEcAn model(s) containing wildcards for filtering
#' @param model_name Model name search string (character)
#' @param revision Model version/revision search string (character)
#' @param ignore_case Logical. If `TRUE` (default) use case-insensitive search otherwise, use case-sensitive search
#' @return Model subset matching the model search string
#' @author Tezan Sahu
#* @get /
searchModels <- function(model_name="", revision="", ignore_case=TRUE, res){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  Models <- tbl(dbcon, "models") %>%
    select(model_id = id, model_name, revision) %>%
    filter(grepl(!!model_name, model_name, ignore.case=ignore_case)) %>%
    filter(grepl(!!revision, revision, ignore.case=ignore_case)) %>%
    arrange(model_id)
  
  qry_res <- Models %>% collect()

  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Model(s) not found"))
  }
  else {
    return(list(models=qry_res, count = nrow(qry_res)))
  }
}