library(dplyr)

#' Search for Inputs containing wildcards for filtering
#' @param model_id Model Id (character)
#' @param site_id Site Id (character)
#' @param offset
#' @param limit
#' @param dbcon Database connection object. Default is global database pool.
#' @return Information about Inputs based on model & site
#' @author Tezan Sahu
#* @get /
searchInputs <- function(req, model_id=NULL, site_id=NULL, format_id=NULL, host_id=NULL, offset=0, limit=50, res,
                         dbcon = global_db_pool){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }
  
  inputs <- tbl(dbcon, "inputs") %>%
    select(input_name=name, id, site_id, format_id, start_date, end_date)
  
  inputs <- tbl(dbcon, "dbfiles") %>%
    select(file_name, file_path, container_type, id=container_id, machine_id) %>%
    inner_join(inputs, by = "id") %>%
    filter(container_type == 'Input') %>%
    select(-container_type)
  
  inputs <- tbl(dbcon, "machines") %>%
    select(hostname, machine_id=id) %>%
    inner_join(inputs, by='machine_id')
  
  inputs <- tbl(dbcon, "formats") %>%
    select(format_id = id, format_name = name, mimetype_id) %>%
    inner_join(inputs, by='format_id')
  
  inputs <- tbl(dbcon, "mimetypes") %>%
    select(mimetype_id = id, mimetype = type_string) %>%
    inner_join(inputs, by='mimetype_id') %>%
    select(-mimetype_id)
  
  inputs <- tbl(dbcon, "sites") %>%
    select(site_id = id, sitename) %>%
    inner_join(inputs, by='site_id')
  
  if(! is.null(model_id)) {
    inputs <- tbl(dbcon, "modeltypes_formats") %>%
      select(tag, modeltype_id, format_id, input) %>%
      inner_join(inputs, by='format_id') %>%
      filter(input) %>%
      select(-input)
    
    inputs <- tbl(dbcon, "models") %>%
      select(model_id = id, modeltype_id, model_name, revision) %>%
      inner_join(inputs, by='modeltype_id') %>%
      filter(model_id == !!model_id) %>%
      select(-modeltype_id, -model_id)
  }
  
  if(! is.null(site_id)) {
    inputs <- inputs %>%
      filter(site_id == !!site_id)
  }
  
  if(! is.null(format_id)) {
    inputs <- inputs %>%
      filter(format_id == !!format_id)
  }
  
  if(! is.null(host_id)) {
    inputs <- inputs %>%
      filter(machine_id == !!host_id)
  }
  
  qry_res <- inputs %>%
    select(-site_id, -format_id, -machine_id) %>%
    distinct() %>%
    arrange(id) %>%
    collect()
  
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

#################################################################################################

#' Download the input specified by the id
#' @param id Input id (character)
#' @param filename Optional filename specified if the id points to a folder instead of file (character)
#' If this is passed with an id that actually points to a file, this name will be ignored
#' @param dbcon Database connection object. Default is global database pool.
#' @return Input file specified by user
#' @author Tezan Sahu
#* @serializer contentType list(type="application/octet-stream")
#* @get /<input_id>
downloadInput <- function(input_id, filename="", req, res, dbcon = global_db_pool){
  db_hostid <- PEcAn.DB::dbHostInfo(dbcon)$hostid
  
  # This is just for temporary testing due to the existing issue in dbHostInfo()
  db_hostid <- ifelse(db_hostid == 99, 99000000001, db_hostid)
  
  input <- tbl(dbcon, "dbfiles") %>%
    select(file_name, file_path, container_id, machine_id, container_type) %>%
    filter(machine_id == !!db_hostid) %>%
    filter(container_type == "Input") %>%
    filter(container_id == !!input_id) %>%
    collect()
  
  if (nrow(input) == 0) {
    res$status <- 404
    return()
  }
  else {
    # Generate the full file path using the file_path & file_name
    filepath <- paste0(input$file_path, "/", input$file_name)
    
    # If the id points to a directory, check if 'filename' within this directory has been specified
    if(dir.exists(filepath)) {
      # If no filename is provided, return 400 Bad Request error
      if(filename == "") {
        res$status <- 400
        return()
      }
      
      # Append the filename to the filepath
      filepath <- paste0(filepath, filename)
    }
    
    # If the file doesn't exist, return 404 error
    if(! file.exists(filepath)){
      res$status <- 404
      return()
    }
    
    # Read the data in binary form & return it
    bin <- readBin(filepath,'raw', n = file.info(filepath)$size)
    return(bin)
  }
}
