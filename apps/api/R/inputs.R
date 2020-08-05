library(dplyr)

#' Search for Inputs containing wildcards for filtering
#' @param model_id Model Id (character)
#' @param site_id Site Id (character)
#' @param offset
#' @param limit 
#' @return Information about Inputs based on model & site
#' @author Tezan Sahu
#* @get /
searchInputs <- function(req, model_id=NULL, site_id=NULL, offset=0, limit=50, res){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }
  
  dbcon <- PEcAn.DB::betyConnect()
  
  inputs <- tbl(dbcon, "inputs") %>%
    select(input_name=name, id, site_id, format_id, start_date, end_date)
  
  inputs <- tbl(dbcon, "dbfiles") %>%
    select(file_name, file_path, container_type, id=container_id, machine_id) %>%
    inner_join(inputs, by = "id") %>%
    filter(container_type == 'Input') %>%
    select(-container_type)
  
  inputs <- tbl(dbcon, "machines") %>%
    select(hostname, machine_id=id) %>%
    inner_join(inputs, by='machine_id') %>%
    select(-machine_id)
  
  inputs <- tbl(dbcon, "modeltypes_formats") %>%
    select(tag, modeltype_id, format_id, input) %>%
    inner_join(inputs, by='format_id') %>%
    filter(input) %>%
    select(-input)
  
  inputs <- tbl(dbcon, "formats") %>%
    select(format_id = id, format_name = name, mimetype_id) %>%
    inner_join(inputs, by='format_id') %>%
    select(-format_id)
  
  inputs <- tbl(dbcon, "mimetypes") %>%
    select(mimetype_id = id, mimetype = type_string) %>%
    inner_join(inputs, by='mimetype_id') %>%
    select(-mimetype_id)
  
  inputs <- tbl(dbcon, "models") %>%
    select(model_id = id, modeltype_id, model_name, revision) %>%
    inner_join(inputs, by='modeltype_id') %>%
    select(-modeltype_id)
  
  inputs <- tbl(dbcon, "sites") %>%
    select(site_id = id, sitename) %>%
    inner_join(inputs, by='site_id')
  
  if(! is.null(model_id)) {
    inputs <- inputs %>%
      filter(model_id == !!model_id)
  }
  
  if(! is.null(site_id)) {
    inputs <- inputs %>%
      filter(site_id == !!site_id)
  }
  
  qry_res <- inputs %>%
    select(-site_id, -model_id) %>%
    distinct() %>%
    arrange(id) %>%
    collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0 || as.numeric(offset) >= nrow(qry_res)) {
    res$status <- 404
    return(list(error="Input(s) not found"))
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
    
    result <- list(inputs = qry_res)
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