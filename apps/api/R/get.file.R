library(dplyr)

#' Download a file associated with PEcAn
#'
#' @param filepath Absolute path to file on target machine
#' @param userid User ID associated with file (typically the same as the user
#'   running the corresponding workflow)
#' @return Raw binary file contents
#' @author Tezan Sehu
get.file <- function(filepath, userid) {
  # Check if the file path is valid
  if(! file.exists(filepath)){
    return(list(status = "Error", message = "File not found"))
  }
  
  # Check if the workflow for run after obtaining absolute path is owned by the user or not
  parent_dir <- normalizePath(dirname(filepath))

  run_id <- substr(parent_dir, stringi::stri_locate_last(parent_dir, regex="/")[1] + 1, stringr::str_length(parent_dir))
  
  if(Sys.getenv("AUTH_REQ") == TRUE) {

    Run <- tbl(global_db_pool, "runs") %>%
      filter(id == !!run_id)
    Run <- tbl(global_db_pool, "ensembles") %>%
      select(ensemble_id=id, workflow_id) %>%
      full_join(Run, by="ensemble_id")  %>%
      filter(id == !!run_id)
    user_id <- tbl(global_db_pool, "workflows") %>%
      select(workflow_id=id, user_id) %>% full_join(Run, by="workflow_id")  %>%
      filter(id == !!run_id) %>%
      pull(user_id)
    
    if(! user_id == userid) {
      return(list(status = "Error", message = "Access forbidden"))
    }
  }
  
  # Read the data in binary form & return it
  bin <- readBin(filepath,'raw', n = file.info(filepath)$size)
  return(list(file_contents = bin))
}
