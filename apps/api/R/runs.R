library(dplyr)

#' Get the list of runs (belonging to a particuar workflow)
#' @param workflow_id Workflow id (character)
#' @param offset
#' @param limit 
#' @return List of runs (belonging to a particuar workflow)
#' @author Tezan Sahu
#* @get /
getRuns <- function(req, workflow_id=NULL, offset=0, limit=50, res){
  if (! limit %in% c(10, 20, 50, 100, 500)) {
    res$status <- 400
    return(list(error = "Invalid value for parameter"))
  }

  dbcon <- PEcAn.DB::betyConnect()
  
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
getRunDetails <- function(run_id, res){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  Runs <- tbl(dbcon, "runs") %>%
    select(-outdir, -outprefix, -setting, -created_at, -updated_at)
  
  Runs <- tbl(dbcon, "ensembles") %>%
    select(runtype, ensemble_id=id, workflow_id) %>%
    full_join(Runs, by="ensemble_id") %>%
    filter(id == !!run_id)
  
  qry_res <- Runs %>% collect()
  
  PEcAn.DB::db.close(dbcon)
  
  if (nrow(qry_res) == 0) {
    res$status <- 404
    return(list(error="Run with specified ID was not found"))
  }
  else {
    # Convert the response from tibble to list
    response <- list()
    for(colname in colnames(qry_res)){
      response[colname] <- qry_res[colname]
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

plotResults <- function(run_id, year, y_var, x_var="time", width=800, height=600, res){
  # Get workflow_id for the run
  dbcon <- PEcAn.DB::betyConnect()
  
  Run <- tbl(dbcon, "runs") %>%
    filter(id == !!run_id)
  
  workflow_id <- tbl(dbcon, "ensembles") %>%
    select(ensemble_id=id, workflow_id) %>%
    full_join(Run, by="ensemble_id")  %>%
    filter(id == !!run_id) %>%
    pull(workflow_id)
  
  PEcAn.DB::db.close(dbcon)
  
  # Check if the data file exists on the host
  datafile <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", workflow_id, "/out/", run_id, "/", year, ".nc")
  if(! file.exists(datafile)){
    res$status <- 404
    return()
  }
  
  # Plot & return
  filename <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/temp", stringi::stri_rand_strings(1, 10), ".png")
  PEcAn.visualization::plot_netcdf(datafile, y_var, x_var, as.integer(width), as.integer(height), year=year, filename=filename)
  img_bin <- readBin(filename,'raw',n = file.info(filename)$size)
  file.remove(filename)
  return(img_bin)
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