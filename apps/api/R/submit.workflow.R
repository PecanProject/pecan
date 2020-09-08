library(dplyr)

#* Submit a workflow sent as XML
#* @param workflowXmlString String containing the XML workflow from request body
#* @param userDetails List containing userid & username
#* @return ID & status of the submitted workflow
#* @author Tezan Sahu
submit.workflow.xml <- function(workflowXmlString, userDetails){
  
  workflowXml <- XML::xmlParseString(stringr::str_replace(workflowXmlString, "<?.*?>\n", ""))
  workflowList <- XML::xmlToList(workflowXml)
  
  return(submit.workflow.list(workflowList, userDetails))
}

#################################################################################################

#* Submit a workflow sent as JSON
#* @param workflowJsonString String containing the JSON workflow from request body
#* @param userDetails List containing userid & username
#* @return ID & status of the submitted workflow
#* @author Tezan Sahu
submit.workflow.json <- function(workflowJsonString, userDetails){
  
  workflowList <- jsonlite::fromJSON(workflowJsonString)
  
  return(submit.workflow.list(workflowList, userDetails))
}

#################################################################################################

#* Submit a workflow (converted to list)
#* @param workflowList Workflow parameters expressed as a list
#* @param userDetails List containing userid & username
#* @return ID & status of the submitted workflow
#* @author Tezan Sahu
submit.workflow.list <- function(workflowList, userDetails) {
  # Fix details about the database
  workflowList$database <- list(
    bety = PEcAn.DB::get_postgres_envvars(
      host = "localhost",
      dbname = "bety",
      user = "bety",
      password = "bety", 
      driver = "PostgreSQL"
    )
  )
  if(! is.null(workflowList$model$id) && (is.null(workflowList$model$type) || is.null(workflowList$model$revision))) {
    dbcon <- PEcAn.DB::betyConnect()
    res <- dplyr::tbl(dbcon, "models") %>% 
      select(id, model_name, revision) %>%
      filter(id == !!workflowList$model$id) %>%
      collect()
    PEcAn.DB::db.close(dbcon)
    
    workflowList$model$type <- res$model_name
    workflowList$model$revision <- res$revision
  }
  # Fix RabbitMQ details
  dbcon <- PEcAn.DB::betyConnect()
  hostInfo <- PEcAn.DB::dbHostInfo(dbcon)
  PEcAn.DB::db.close(dbcon)
  workflowList$host <- list(
    rabbitmq = list(
      uri = Sys.getenv("RABBITMQ_URI", "amqp://guest:guest@localhost/%2F"),
      queue = paste0(workflowList$model$type, "_", workflowList$model$revision)
    )
  )
  workflowList$host$name <- if(hostInfo$hostname == "") "localhost" else hostInfo$hostname
  # Fix the info
  workflowList$info$notes <- workflowList$info$notes
  if(is.null(workflowList$info$userid)){
    workflowList$info$userid <- userDetails$userid
  }
  if(is.null(workflowList$info$username)){
    workflowList$info$username <- userDetails$username
  }
  if(is.null(workflowList$info$date)){
    workflowList$info$date <- Sys.time()
  }

  # Add entry to workflows table in database
  workflow_id <- insert.workflow(workflowList)
  workflowList$workflow$id <- workflow_id

  # Add entry to attributes table in database
  insert.attribute(workflowList)

  # Fix the output directory
  outdir <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_", workflow_id)
  workflowList$outdir <- outdir
  
  # Create output diretory
  dir.create(outdir, recursive=TRUE)

  # Modify the `dbfiles` path & create the directory if needed
  workflowList$run$dbfiles <- Sys.getenv("DBFILES_DIR", "/data/dbfiles/")
  if(! dir.exists(workflowList$run$dbfiles)){
    dir.create(workflowList$run$dbfiles, recursive = TRUE)
  }
  
  # Convert settings list to XML & save it into outdir
  workflowXml <- PEcAn.settings::listToXml(workflowList, "pecan")
  XML::saveXML(workflowXml, paste0(outdir, "/pecan.xml"))
  res <- file.copy("/work/workflow.R", outdir)
  
  # Post workflow to RabbitMQ
  message <- list(folder = outdir, workflowid = workflow_id)
  res <- PEcAn.remote::rabbitmq_post_message(workflowList$host$rabbitmq$uri, "pecan", message, "rabbitmq")
  
  if(res$routed){
    return(list(workflow_id = as.character(workflow_id), status = "Submitted successfully"))
  }
  else{
    return(list(status = "Error", message = "Could not submit to RabbitMQ"))
  }
  
  
}

#################################################################################################

#* Insert the workflow into workflows table to obtain the workflow_id
#* @param workflowList List containing the workflow details
#* @return ID of the submitted workflow
#* @author Tezan Sahu
insert.workflow <- function(workflowList){
  
  dbcon <- PEcAn.DB::betyConnect()
  
  model_id <- workflowList$model$id
  if(is.null(model_id)){
    model_id <- PEcAn.DB::get.id("models", c("model_name", "revision"), c(workflowList$model$type, workflowList$model$revision), dbcon)
  }
  
  start_time <- Sys.time()
  
  workflow_df <- tibble::tibble(
    "site_id" = c(bit64::as.integer64(workflowList$run$site$id)),
    "model_id" = c(bit64::as.integer64(model_id)),
    "folder" = "temp_dir",
    "hostname" = c("docker"),
    "start_date" = c(as.POSIXct(workflowList$run$start.date)),
    "end_date" = c(as.POSIXct(workflowList$run$end.date)),
    "advanced_edit" = c(FALSE),
    "started_at" = c(start_time),
    stringsAsFactors = FALSE
  )
  
  if(! is.na(workflowList$info$userid)){
    workflow_df <- workflow_df %>% tibble::add_column("user_id" = c(bit64::as.integer64(workflowList$info$userid)))
  }
  
  insert <- PEcAn.DB::insert_table(workflow_df, "workflows", dbcon)
  
  workflow_id <- dplyr::tbl(dbcon, "workflows") %>% 
    filter(started_at == start_time 
           && site_id == bit64::as.integer64(workflowList$run$site$id)
           && model_id == bit64::as.integer64(model_id)
    ) %>% 
    pull(id)
  
  update_qry <- paste0("UPDATE workflows SET folder = 'data/workflows/PEcAn_", workflow_id, "' WHERE id = '", workflow_id, "';")
  PEcAn.DB::db.query(update_qry, dbcon)
  
  PEcAn.DB::db.close(dbcon)
  
  return(workflow_id)
}

#################################################################################################

#* Insert the workflow into attributes table
#* @param workflowList List containing the workflow details
#* @author Tezan Sahu
insert.attribute <- function(workflowList){
  dbcon <- PEcAn.DB::betyConnect()
  
  # Create an array of PFTs
  pfts <- c()
  for(i in seq(length(workflowList$pfts))){
    pfts <- c(pfts, workflowList$pfts[i]$pft$name)
  }

  # Obtain the model_id
  model_id <- workflowList$model$id
  if(is.null(model_id)){
    model_id <- PEcAn.DB::get.id("models", c("model_name", "revision"), c(workflowList$model$type, workflowList$model$revision), dbcon)
  }
  
  # Fill in the properties
  properties <- list(
    start = as.POSIXct(workflowList$run$start.date),
    end = as.POSIXct(workflowList$run$end.date),
    pfts = pfts,
    runs = if(is.null(workflowList$ensemble$size)) 1 else workflowList$ensemble$size,
    modelid = model_id,
    siteid = bit64::as.integer64(workflowList$run$site$id),
    sitename = dplyr::tbl(dbcon, "sites") %>% filter(id == bit64::as.integer64(workflowList$run$site$id)) %>% pull(sitename),
    #sitegroupid <- 
    lat = if(is.null(workflowList$run$site$lat)) "" else workflowList$run$site$lat,
    lon = if(is.null(workflowList$run$site$lon)) "" else workflowList$run$site$lon,
    email = if(is.na(workflowList$info$userid) || workflowList$info$userid == -1) "" else
      dplyr::tbl(dbcon, "users") %>% filter(id == bit64::as.integer64(workflowList$info$userid)) %>% pull(email),
    notes = if(is.null(workflowList$info$notes)) "" else workflowList$info$notes,
    variables = workflowList$ensemble$variable
  )
  
  if(! is.null(workflowList$run$inputs$met$id)) {
    properties$input_met <- workflowList$run$inputs$met$id
  }
  else if(! is.null(workflowList$run$inputs$met$source)) {
    properties$input_met <- workflowList$run$inputs$met$source
  }
  
  if(! is.null(workflowList$ensemble$parameters$method)) properties$parm_method <- workflowList$ensemble$parameters$method
  if(! is.null(workflowList$sensitivity.analysis$quantiles)){
    sensitivity <- c()
    for(i in seq(length(workflowList$sensitivity.analysis$quantiles))){
      sensitivity <- c(sensitivity, workflowList$sensitivity.analysis$quantiles[i]$sigma)
    }
    properties$sensitivity <- paste0(sensitivity, collapse=",")
  }
  # More variables can be added later
  
  # Insert properties into attributes table
  value_json <- as.character(jsonlite::toJSON(properties, auto_unbox = TRUE))
  
  res <- DBI::dbSendStatement(dbcon, 
                              "INSERT INTO attributes (container_type, container_id, value) VALUES ($1, $2, $3)", 
                              list("workflows", bit64::as.integer64(workflowList$workflow$id), value_json))

  
}