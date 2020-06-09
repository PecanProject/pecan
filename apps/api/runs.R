#' Get the list of runs (belonging to a particuar workflow)
#' @param workflow_id Workflow id (character)
#' @param offset
#' @param limit 
#' @return List of runs (belonging to a particuar workflow)
#' @author Tezan Sahu
#* @get /
getWorkflows <- function(req, workflow_id, offset=0, limit=50, res){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }
  settings <-list(database = list(bety = list(
    driver = "PostgreSQL", 
    user = "bety", 
    dbname = "bety", 
    password = "bety", 
    host="postgres"
  )))
  dbcon <- PEcAn.DB::db.open(settings$database$bety)
  qry_statement <- paste0(
    "SELECT r.id, e.runtype, r.model_id, r.site_id, r.parameter_list, r.ensemble_id, ", 
    "e.workflow_id, r.start_time, r.finish_time ", 
    "FROM runs r FULL OUTER JOIN ensembles e ", 
    "ON (r.ensemble_id = e.id) ", 
    "WHERE e.workflow_id = '", workflow_id,
    "' ORDER BY r.id DESC ",
    "LIMIT ", limit, 
    " OFFSET ", offset, ";"
  )
  
  PEcAn.DB::db.query(qry_statement, dbcon)
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Run(s) not found"))
  }
  else {
    result <- list(runs = qry_res)
    result$count <- nrow(qry_res)
    if(nrow(qry_res) == limit){
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
    if(as.numeric(offset) != 0) {
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

#' Get the of the run specified by the id
#' @param id Run id (character)
#' @return Details of requested run
#' @author Tezan Sahu
#* @get /<id>
getWorkflowDetails <- function(id, res){
  settings <-list(database = list(bety = list(
    driver = "PostgreSQL", 
    user = "bety", 
    dbname = "bety", 
    password = "bety", 
    host="postgres"
  )))
  dbcon <- PEcAn.DB::db.open(settings$database$bety)
  qry_statement <- paste0(
    "SELECT r.id, e.runtype, r.model_id, r.site_id, r.parameter_list, r.ensemble_id, e.workflow_id, ", 
    "r.start_time, r.finish_time, r.created_at, r.updated_at, r.started_at, r.finished_at ", 
    "FROM runs r FULL OUTER JOIN ensembles e ", 
    "ON (r.ensemble_id = e.id) ", 
    "WHERE r.id = '", id, "';"
  )
  
  qry_res <- PEcAn.DB::db.query(qry_statement, dbcon)
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Run with specified ID was not found"))
  }
  else {
    return(qry_res)
  }
}