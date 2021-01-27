library(dplyr)

#' Retrieve the details of a PEcAn model, based on model_id
#' @param model_id Model ID (character)
#' @param dbcon Database connection object. Default is global database pool.
#' @return Model details
#' @author Tezan Sahu
#* @get /<model_id>
getModel <- function(model_id, res, dbcon = global_db_pool){
  
  Model <- tbl(dbcon, "models") %>%
    select(model_id = id, model_name, revision, modeltype_id) %>%
    filter(model_id == !!model_id)
  
  Model <- tbl(dbcon, "modeltypes") %>%
    select(modeltype_id = id, model_type = name) %>%
    inner_join(Model, by = "modeltype_id")
  
  qry_res <- Model %>% collect()
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Model not found"))
  }
  else {
    # Convert the response from tibble to list
    response <- list()
    for(colname in colnames(qry_res)){
      response[colname] <- qry_res[colname]
    }
    
    inputs_req <- tbl(dbcon, "modeltypes_formats") %>% 
      filter(modeltype_id == bit64::as.integer64(qry_res$modeltype_id)) %>% 
      select(input=tag, required) %>% collect()
    response$inputs <- jsonlite::fromJSON(gsub('(\")', '"', jsonlite::toJSON(inputs_req)))
    return(response)
  }
}

#########################################################################

#' Search for PEcAn model(s) containing wildcards for filtering
#' @param model_name Model name search string (character)
#' @param revision Model version/revision search string (character)
#' @param ignore_case Logical. If `TRUE` (default) use case-insensitive search otherwise, use case-sensitive search
#' @param dbcon Database connection object. Default is global database pool.
#' @return Model subset matching the model search string
#' @author Tezan Sahu
#* @get /
searchModels <- function(model_name="", revision="", ignore_case=TRUE, res,
                         dbcon = global_db_pool){
  model_name <- URLdecode(model_name)
  revision <- URLdecode(revision)
  
  Models <- tbl(dbcon, "models") %>%
    select(model_id = id, model_name, revision) %>%
    filter(grepl(!!model_name, model_name, ignore.case=ignore_case)) %>%
    filter(grepl(!!revision, revision, ignore.case=ignore_case)) %>%
    arrange(model_id)
  
  qry_res <- Models %>% collect()

  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Model(s) not found"))
  }
  else {
    return(list(models=qry_res, count = nrow(qry_res)))
  }
}
