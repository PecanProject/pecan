library(dplyr)
source("get.file.R")

#' Get the list of runs (belonging to a particuar workflow)
#' @param workflow_id Workflow id (character)
#' @param offset
#' @param limit
#' @param dbcon Database connection object. Default is global database pool.
#' @return List of runs (belonging to a particuar workflow)
#' @author Tezan Sahu
#* @get /
getRuns <- function(req, workflow_id=NULL, offset=0, limit=50, res,
                    dbcon = global_db_pool){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }

  Runs <- tbl(dbcon, "runs") %>%
    select(id, model_id, site_id, parameter_list, ensemble_id, start_time, finish_time)
  
  Runs <- tbl(dbcon, "ensembles") %>%
    select(runtype, ensemble_id=id, workflow_id) %>%
    full_join(Runs, by="ensemble_id") 
  
  if(! is.null(workflow_id)){
    Runs <- Runs %>%
      filter(workflow_id == !!workflow_id)
  }
  
  qry_res <- Runs %>% 
    arrange(id) %>%
    collect()
  
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
        "/api/runs",
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
        "/api/runs",
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

#' Get the details of the run specified by the id
#' @param run_id Run id (character)
#' @return Details of requested run
#' @author Tezan Sahu
#* @get /<run_id>
getRunDetails <- function(req, run_id, res, dbcon = global_db_pool){
  
  Runs <- tbl(dbcon, "runs") %>%
    select(-outdir, -outprefix, -setting, -created_at, -updated_at)
  
  Runs <- tbl(dbcon, "ensembles") %>%
    select(runtype, ensemble_id=id, workflow_id) %>%
    full_join(Runs, by="ensemble_id") %>%
    filter(id == !!run_id)
  
  qry_res <- Runs %>% collect()
  
  if(Sys.getenv("AUTH_REQ") == TRUE){
    user_id <- tbl(dbcon, "workflows") %>%
      select(workflow_id=id, user_id) %>% full_join(Runs, by="workflow_id")  %>%
      filter(id == !!run_id) %>%
      pull(user_id)
  }
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Run with specified ID was not found"))
  }
  else {
    
    if(Sys.getenv("AUTH_REQ") == TRUE) {
      # If user id of requested run does not match the caller of the API, return 403 Access forbidden
      if(is.na(user_id) || user_id != req$user$userid){
        res$status <- 403
        return(list(error="Access forbidden"))
      }
    }
    
    # Convert the response from tibble to list
    response <- list()
    for(colname in colnames(qry_res)){
      response[colname] <- qry_res[colname]
    }
    
    # If inputs exist on the host, add them to the response
    indir <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", response$workflow_id, "/run/", run_id)
    if(dir.exists(indir)){
      response$inputs <- getRunInputs(indir)
    }
    
    # If outputs exist on the host, add them to the response
    outdir <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", response$workflow_id, "/out/", run_id)
    if(dir.exists(outdir)){
      response$outputs <- getRunOutputs(outdir)
    }
    
    return(response)
  }
}

#################################################################################################

#' Get the input file specified by user for a run
#' @param run_id Run id (character)
#' @param filename Name of the input file (character)
#' @param dbcon Database connection object. Default is global database pool.
#' @return Input file specified by user for the run
#' @author Tezan Sahu
#* @serializer contentType list(type="application/octet-stream")
#* @get /<run_id>/input/<filename>
getRunInputFile <- function(req, run_id, filename, res, dbcon = global_db_pool){
  
  Run <- tbl(dbcon, "runs") %>%
    filter(id == !!run_id)
  
  workflow_id <- tbl(dbcon, "ensembles") %>%
    select(ensemble_id=id, workflow_id) %>%
    full_join(Run, by="ensemble_id")  %>%
    filter(id == !!run_id) %>%
    pull(workflow_id)
  
  inputpath <- paste0( Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", workflow_id, "/run/", run_id, "/", filename)

  result <- get.file(inputpath, req$user$userid)
  if(is.null(result$file_contents)){
    if(result$status == "Error" && result$message == "Access forbidden") {
      res$status <- 403
      return()
    }
    if(result$status == "Error" && result$message == "File not found") {
      res$status <- 404
      return()
    }
  }
  return(result$file_contents)
}

#################################################################################################

#' Get the output file specified by user for a run
#' @param run_id Run id (character)
#' @param filename Name of the output file (character)
#' @return Output file specified by user for the run
#' @author Tezan Sahu
#* @serializer contentType list(type="application/octet-stream")
#* @get /<run_id>/output/<filename>
getRunOutputFile <- function(req, run_id, filename, res, dbcon = global_db_pool){
  
  Run <- tbl(dbcon, "runs") %>%
    filter(id == !!run_id)
  
  workflow_id <- tbl(dbcon, "ensembles") %>%
    select(ensemble_id=id, workflow_id) %>%
    full_join(Run, by="ensemble_id")  %>%
    filter(id == !!run_id) %>%
    pull(workflow_id)
  
  outputpath <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", workflow_id, "/out/", run_id, "/", filename)
  
  result <- get.file(outputpath, req$user$userid)
  if(is.null(result$file_contents)){
    if(result$status == "Error" && result$message == "Access forbidden") {
      res$status <- 403
      return()
    }
    if(result$status == "Error" && result$message == "File not found") {
      res$status <- 404
      return()
    }
  }
  return(result$file_contents)
}

#################################################################################################

#' Plot the results obtained from a run
#' @param run_id Run id (character)
#' @param year the year this data is for
#' @param yvar the variable to plot along the y-axis.
#' @param xvar the variable to plot along the x-axis, by default time is used.
#' @param width the width of the image generated, default is 800 pixels.
#' @param height the height of the image generated, default is 600 pixels.
#' @return List of runs (belonging to a particuar workflow)
#' @author Tezan Sahu
#* @get /<run_id>/graph/<year>/<y_var>
#* @serializer contentType list(type='image/png')

plotResults <- function(req, run_id, year, y_var, x_var="time", width=800, height=600, res,
                        dbcon = global_db_pool) {
  # Get workflow_id for the run
  Run <- tbl(dbcon, "runs") %>%
    filter(id == !!run_id)
  
  workflow_id <- tbl(dbcon, "ensembles") %>%
    select(ensemble_id=id, workflow_id) %>%
    full_join(Run, by="ensemble_id")  %>%
    filter(id == !!run_id) %>%
    pull(workflow_id)
  
  if(Sys.getenv("AUTH_REQ") == TRUE){
    user_id <- tbl(dbcon, "workflows") %>%
      select(id, user_id) %>%
      filter(id == !!workflow_id) %>%
      pull(user_id)
  }
  
  # Check if the data file exists on the host
  datafile <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", workflow_id, "/out/", run_id, "/", year, ".nc")
  if(! file.exists(datafile)){
    res$status <- 404
    return()
  }
  
  if(Sys.getenv("AUTH_REQ") == TRUE) {
    # If user id of requested run does not match the caller of the API, return 403 Access forbidden
    if(is.na(user_id) || user_id != req$user$userid){
      res$status <- 403
      return(list(error="Access forbidden"))
    }
  }
  
  # Plot & return
  filename <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/temp", stringi::stri_rand_strings(1, 10), ".png")
  PEcAn.visualization::plot_netcdf(datafile, y_var, x_var, as.integer(width), as.integer(height), year=year, filename=filename)
  img_bin <- readBin(filename,'raw',n = file.info(filename)$size)
  file.remove(filename)
  return(img_bin)
}


#################################################################################################

#' Get the inputs of a run (if the files exist on the host)
#' @param indir Run input directory (character)
#' @return Input details of the run
#' @author Tezan Sahu

getRunInputs <- function(indir){
  inputs <- list()
  if(file.exists(paste0(indir, "/README.txt"))){
    inputs$info <- "README.txt"
  }
  all_files <- list.files(indir)
  inputs$others <- all_files[!all_files %in% c("job.sh", "rabbitmq.out", "README.txt")]
  return(inputs)
}

#################################################################################################

#' Get the outputs of a run (if the files exist on the host)
#' @param outdir Run output directory (character)
#' @return Output details of the run
#' @author Tezan Sahu

getRunOutputs <- function(outdir){
  outputs <- list()
  if(file.exists(paste0(outdir, "/logfile.txt"))){
    outputs$logfile <- "logfile.txt"
  }
  
  if(file.exists(paste0(outdir, "/README.txt"))){
    outputs$info <- "README.txt"
  }
  
  year_files <- list.files(outdir, pattern="*.nc$")
  years <- stringr::str_replace_all(year_files, ".nc", "")
  years_data <- c()
  outputs$years <- list()
  for(year in years){
    var_lines <- readLines(paste0(outdir, "/", year, ".nc.var"))
    keys <- stringr::word(var_lines, 1)
    values <- stringr::word(var_lines, 2, -1)
    vars <- list()
    for(i in 1:length(keys)){
      vars[keys[i]] <- values[i]
    }
    years_data <- c(years_data, list(list(
      data = paste0(year, ".nc"),
      variables = vars
    )))
  }
  for(i in 1:length(years)){
    outputs$years[years[i]] <- years_data[i]
  }
  return(outputs)
}
