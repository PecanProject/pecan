library(dplyr)

#' Retrieve the details of a particular version of a model
#' @param name Model name (character)
#' @param revision Model version/revision (character)
#' @return Model details
#' @author Tezan Sahu
#* @get /
getModels <- function(model_name="all", revision="all", res){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  Models <- tbl(dbcon, "models") %>%
    select(model_id = id, model_name, revision, modeltype_id)
  
  if (model_name != "all"){
    Models <- Models %>%
      filter(model_name == !!model_name)
  }
  
  if (revision != "all"){
    Models <- Models %>%
      filter(revision == !!revision)
  }
  
  Models <- tbl(dbcon, "modeltypes") %>%
    select(modeltype_id = id, model_type = name) %>%
    inner_join(Models, by = "modeltype_id") %>%
    arrange(model_id)
  
  qry_res <- Models %>% collect()

  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Model(s) not found"))
  }
  else {
    return(list(models=qry_res))
  }
}