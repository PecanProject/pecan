library(dplyr)
source("submit.workflow.R")

#' Get the list of workflows (using a particular model & site, if specified)
#' @param model_id Model id (character)
#' @param site_id Site id (character)
#' @param offset
#' @param limit Max number of workflows to retrieve (default = 50)
#' @param dbcon Database connection object. Default is global database pool.
#' @return List of workflows (using a particular model & site, if specified)
#' @author Tezan Sahu
#* @get /
getWorkflows <- function(req, model_id=NULL, site_id=NULL, offset=0, limit=50, res,
                         dbcon = global_db_pool){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }
  
  Workflow <- tbl(dbcon, "workflows") %>%
    select(-created_at, -updated_at, -params, -advanced_edit, -notes)
  
  if (!is.null(model_id)) {
    Workflow <- Workflow %>%
      filter(model_id == !!model_id)
  }
  
  if (!is.null(site_id)) {
    Workflow <- Workflow %>%
      filter(site_id == !!site_id)
  }
  
  qry_res <- Workflow %>% collect()

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

#' Post a workflow for execution
#' @param req Request sent
#' @return ID & status of the submitted workflow
#' @author Tezan Sahu
#* @post /
submitWorkflow <- function(req, res){
  if(req$HTTP_CONTENT_TYPE == "application/xml") {
    submission_res <- submit.workflow.xml(req$postBody, req$user)
  }
  else if(req$HTTP_CONTENT_TYPE == "application/json") {
    submission_res <- submit.workflow.json(req$postBody, req$user)
  }
  else{
    res$status <- 415
    return(paste("Unsupported request content type:", req$HTTP_CONTENT_TYPE))
  }
  
  if(submission_res$status == "Error"){
    res$status <- 400
    return(submission_res)
  }
  res$status <- 201
  return(submission_res)
}

#################################################################################################

#' Get the of the workflow specified by the id
#' @param id Workflow id (character)
#' @param dbcon Database connection object. Default is global database pool.
#' @return Details of requested workflow
#' @author Tezan Sahu
#* @get /<id>
getWorkflowDetails <- function(id, req, res, dbcon = global_db_pool){
  Workflow <- tbl(dbcon, "workflows") %>%
    select(id, model_id, site_id, folder, hostname, user_id)
  
  Workflow <- tbl(dbcon, "attributes") %>%
    select(id = container_id, properties = value) %>%
    full_join(Workflow, by = "id") %>%
    filter(id == !!id)
  
  qry_res <- Workflow %>% collect()
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Workflow with specified ID was not found"))
  }
  else {
    if(is.na(qry_res$properties)){
      res <- list(
        id = id, 
        folder=qry_res$folder, 
        hostname=qry_res$hostname,
        user_id=qry_res$user_id,
        properties = list(modelid = qry_res$model_id, siteid = qry_res$site_id)
      )
    }
    else{
      res <- list(
        id = id, 
        folder=qry_res$folder, 
        hostname=qry_res$hostname,
        user_id=qry_res$user_id,
        properties = jsonlite::parse_json(qry_res$properties[[1]])
      )
    }
    
    # Add the files for the workflow if they exist on disk
    filesdir <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", id)
    if(dir.exists(filesdir)){
      all_files <- list.files(filesdir)
      res$files <- all_files[!all_files %in% c("out", "rabbitmq.out", "pft", "run", "STATUS")]
    }
    
    return(res)
  }
}

#################################################################################################

#' Get the of the workflow specified by the id
#' @param id Workflow id (character)
#' @param dbcon Database connection object. Default is global database pool.
#' @return Details of requested workflow
#' @author Tezan Sahu
#* @get /<id>/status
getWorkflowStatus <- function(req, id, res, dbcon = global_db_pool){
  Workflow <- tbl(dbcon, "workflows") %>%
    select(id, user_id) %>%
    filter(id == !!id)

  
  qry_res <- Workflow %>% collect()
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Workflow with specified ID was not found on this host"))
  }
  else {
    # Check if the STATUS file exists on the host
    statusfile <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", qry_res$id, "/STATUS")
    if(! file.exists(statusfile)){
      res$status <- 404
      return(list(error="Workflow with specified ID was not found on this host"))
    }
    
    wf_status <- readLines(statusfile)
    wf_status <- stringr::str_replace_all(wf_status, "\t", "  ")
    return(list(workflow_id=id, status=wf_status))
  }
}

#################################################################################################

#' Get a specified file of the workflow specified by the id
#' @param id Workflow id (character)
#' @param dbcon Database connection object. Default is global database pool.
#' @return Details of requested workflow
#' @author Tezan Sahu
#* @serializer contentType list(type="application/octet-stream")
#* @get /<id>/file/<filename>
getWorkflowFile <- function(req, id, filename, res, dbcon = global_db_pool){
  Workflow <- tbl(dbcon, "workflows") %>%
    select(id, user_id) %>%
    filter(id == !!id)
  
  qry_res <- Workflow %>% collect()
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return()
  }
  else {
    # Check if the requested file exists on the host
    filepath <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", id, "/", filename)
    if(! file.exists(filepath)){
      res$status <- 404
      return()
    }
    
    if(Sys.getenv("AUTH_REQ") == TRUE){
      if(qry_res$user_id != req$user$userid) {
        res$status <- 403
        return()
      } 
    }
    
    # Read the data in binary form & return it
    bin <- readBin(filepath,'raw', n = file.info(filepath)$size)
    return(bin)
  }
}
