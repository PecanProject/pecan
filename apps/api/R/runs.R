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

  dbcon <- PEcAn.DB::betyConnect()
  
  Runs <- tbl(dbcon, "runs") %>%
    select(id, model_id, site_id, parameter_list, ensemble_id, start_time, finish_time)
  
  Runs <- tbl(dbcon, "ensembles") %>%
    select(runtype, ensemble_id=id, workflow_id) %>%
    full_join(Runs, by="ensemble_id") %>%
    filter(workflow_id == !!workflow_id)
  
  qry_res <- Runs %>% 
    arrange(id) %>%
    collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0 || as.numeric(offset) >= nrow(qry_res)) {
    res$status <- 404
    return(list(error="Run(s) not found"))
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
    result <- list(runs = qry_res)
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

#' Get the of the run specified by the id
#' @param id Run id (character)
#' @return Details of requested run
#' @author Tezan Sahu
#* @get /<id>
getWorkflowDetails <- function(id, res){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  Runs <- tbl(dbcon, "runs") %>%
    select(-outdir, -outprefix, -setting)
  
  Runs <- tbl(dbcon, "ensembles") %>%
    select(runtype, ensemble_id=id, workflow_id) %>%
    full_join(Runs, by="ensemble_id") %>%
    filter(id == !!id)
  
  qry_res <- Runs %>% collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Run with specified ID was not found"))
  }
  else {
    return(qry_res)
  }
}