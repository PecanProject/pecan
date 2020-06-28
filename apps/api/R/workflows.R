library(dplyr)

#' Get the list of workflows (using a particular model & site, if specified)
#' @param model_id Model id (character)
#' @param site_id Site id (character)
#' @param offset
#' @param limit 
#' @return List of workflows (using a particular model & site, if specified)
#' @author Tezan Sahu
#* @get /
getWorkflows <- function(req, model_id=NULL, site_id=NULL, offset=0, limit=50, res){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }
  
  dbcon <- PEcAn.DB::betyConnect()
  
  Workflow <- tbl(dbcon, "workflows") %>%
    select(id, model_id, site_id)
  
  Workflow <- tbl(dbcon, "attributes") %>%
    select(id = container_id, properties = value) %>%
    full_join(Workflow, by = "id")
  
  if (!is.null(model_id)) {
    Workflow <- Workflow %>%
      filter(model_id == !!model_id)
  }
  
  if (!is.null(site_id)) {
    Workflow <- Workflow %>%
      filter(site_id == !!site_id)
  }
  
  qry_res <- Workflow %>% 
    select(-model_id, -site_id) %>%
    arrange(id) %>%
    collect()

  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0 || as.numeric(offset) >= nrow(qry_res)) {
    res$status <- 404
    return(list(error="Workflows not found"))
  }
  else {
    has_next <- FALSE
    has_prev <- FALSE
    if (nrow(qry_res) > (as.numeric(offset) + as.numeric(limit))) {
      has_next <- TRUE
    }
    if (as.numeric(offset) != 0) {
      has_prev <- TRUE
    }
    
    qry_res <- qry_res[(as.numeric(offset) + 1):min((as.numeric(offset) + as.numeric(limit)), nrow(qry_res)), ]
    
    qry_res$properties[is.na(qry_res$properties)] = "{}"
    qry_res$properties <- purrr::map(qry_res$properties, jsonlite::parse_json)
    result <- list(workflows = qry_res)
    result$count <- nrow(qry_res)
    if(has_next){
      result$next_page <- paste0(
        req$rook.url_scheme, "://",
        req$HTTP_HOST,
        "/api/workflows",
        req$PATH_INFO,
        substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "offset=")[[2]]),
        (as.numeric(limit) + as.numeric(offset)),
        "&limit=", 
        limit
      )
    }
    if(has_prev) {
      result$prev_page <- paste0(
        req$rook.url_scheme, "://",
        req$HTTP_HOST,
        "/api/workflows",
        req$PATH_INFO, 
        substr(req$QUERY_STRING, 0, stringr::str_locate(req$QUERY_STRING, "offset=")[[2]]),
        max(0, (as.numeric(offset) - as.numeric(limit))),
        "&limit=", 
        limit
      )
    }
    
    return(result)
  }
}

#################################################################################################

#' Get the of the workflow specified by the id
#' @param id Workflow id (character)
#' @return Details of requested workflow
#' @author Tezan Sahu
#* @get /<id>
getWorkflowDetails <- function(id, res){
  dbcon <- PEcAn.DB::betyConnect()
  
  Workflow <- tbl(dbcon, "workflows") %>%
    select(id, model_id, site_id)
  
  Workflow <- tbl(dbcon, "attributes") %>%
    select(id = container_id, properties = value) %>%
    full_join(Workflow, by = "id") %>%
    filter(id == !!id)
  
  qry_res <- Workflow %>% collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Workflow with specified ID was not found"))
  }
  else {
    if(is.na(qry_res$properties)){
      res <- list(id = id, properties = list(modelid = qry_res$model_id, siteid = qry_res$site_id))
    }
    else{
      res <- list(id = id, properties = jsonlite::parse_json(qry_res$properties[[1]]))
    }
    
    return(res)
  }
}