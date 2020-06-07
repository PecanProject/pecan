#' Get the list of workflows (using a particular model & site, if specified)
#' @param model_id Model id (character)
#' @param site_id Site id (character)
#' @return List of workflows (using a particular model & site, if specified)
#' @author Tezan Sahu
#* @get /
getWorkflows <- function(model_id=NULL, site_id=NULL, res){
  settings <-list(database = list(bety = list(
    driver = "PostgreSQL", 
    user = "bety", 
    dbname = "bety", 
    password = "bety", 
    host="postgres"
  )))
  dbcon <- PEcAn.DB::db.open(settings$database$bety)
  qry_statement <- paste(
    "SELECT w.id, a.value AS properties",
    "FROM workflows w",
    "FULL OUTER JOIN attributes a",
    "ON (w.id = a.container_id)"
  )
  
  if (is.null(model_id) & is.null(site_id)){
    # Leave as it is
  }
  else if (!is.null(model_id) & is.null(site_id)){
    qry_statement <- paste0(qry_statement, " WHERE w.model_id = '", model_id, "'")
  }
  else if (is.null(model_id) & !is.null(site_id)){
    qry_statement <- paste0(qry_statement, " WHERE w.site_id = '", site_id, "'")
  }
  else{
    qry_statement <- paste0(qry_statement, " WHERE w.model_id = '", model_id, "' and w.site_id = '", site_id, "'")
  }
  
  qry_statement <- paste0(qry_statement, " ORDER BY id")
  
  qry_res <- PEcAn.DB::db.query(qry_statement, dbcon)
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Workflows not found"))
  }
  else {
    qry_res$properties[is.na(qry_res$properties)] = "{}"
    qry_res$properties <- purrr::map(qry_res$properties, jsonlite::parse_json)
    return(list(workflows = qry_res))
  }
}