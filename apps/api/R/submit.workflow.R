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

  # Set database details
  workflowList$database <- list(
    bety = PEcAn.DB::get_postgres_envvars(
      host = "localhost",
      dbname = "bety",
      user = "bety",
      password = "bety",
      driver = "PostgreSQL"
    )
  )

  if (is.null(workflowList$model$id)) {
    return(list(status = "Error",
                error = "Must provide model ID."))
  }

  # Get model revision and type for the RabbitMQ queue
  model_info <- dplyr::tbl(global_db_pool, "models") %>%
    dplyr::filter(id == !!workflowList$model$id) %>%
    dplyr::inner_join(dplyr::tbl(global_db_pool, "modeltypes"),
                      by = c("modeltype_id" = "id")) %>%
    dplyr::collect()

  if (nrow(model_info) < 1) {
    msg <- paste0("No models found with ID ", format(workflowList$model$id, scientific = FALSE))
    return(list(status = "Error", error = msg))
  } else if (nrow(model_info) > 1) {
    msg <- paste0(
      "Found multiple (", nrow(model_info), ") matching models for id ",
      format(workflowList$model$id, scientific = FALSE),
      ". This shouldn't happen! Check your database for errors."
    )
    return(list(status = "Error", error = msg))
  }

  model_type <- model_info$name
  model_revision <- model_info$revision

  # Fix RabbitMQ details
  hostInfo <- PEcAn.DB::dbHostInfo(global_db_pool)
  workflowList$host <- list(
    rabbitmq = list(
      uri = Sys.getenv("RABBITMQ_URI", "amqp://guest:guest@localhost/%2F"),
      queue = paste0(model_type, "_", model_revision)
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
  workflow_id_str <- format(workflow_id, scientific = FALSE)

  # Add entry to attributes table in database
  insert.attribute(workflowList)

  # Fix the output directory
  outdir <- paste0(Sys.getenv("DATA_DIR", "/data/"), "workflows/PEcAn_",
                   workflow_id_str)
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
  message <- list(folder = outdir, workflowid = workflow_id_str)
  res <- PEcAn.remote::rabbitmq_post_message(workflowList$host$rabbitmq$uri, "pecan", message)
  
  if(res$routed){
    return(list(workflow_id = workflow_id_str, status = "Submitted successfully"))
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
  
  model_id <- workflowList$model$id
  if(is.null(model_id)){
    model_id <- PEcAn.DB::get.id("models", c("model_name", "revision"), c(workflowList$model$type, workflowList$model$revision), global_db_pool)
  }
  
  start_time <- Sys.time()
  
  workflow_df <- tibble::tibble(
    "site_id" = bit64::as.integer64(workflowList$run$site$id),
    "model_id" = bit64::as.integer64(model_id),
    "folder" = "temp_dir",
    "hostname" = "docker",
    "start_date" = as.POSIXct(workflowList$run$start.date),
    "end_date" = as.POSIXct(workflowList$run$end.date),
    "advanced_edit" = FALSE,
    "started_at" = start_time
  )
  
  if (! is.na(workflowList$info$userid)){
    workflow_df <- workflow_df %>%
      tibble::add_column("user_id" = bit64::as.integer64(workflowList$info$userid))
  }

  # NOTE: Have to "checkout" a connection from the pool here to work with
  # dbSendStatement and friends. We make sure to return the connection when the
  # function exits (successfully or not).
  #con <- pool::poolCheckout(global_db_pool)
  #on.exit(pool::poolReturn(con), add = TRUE)
  con <- global_db_pool

  insert_query <- glue::glue(
    "INSERT INTO workflows ",
    "({paste(colnames(workflow_df), collapse = ', ')}) ",
    "VALUES ({paste0('$', seq_len(ncol(workflow_df)), collapse = ', ')}) ",
    "RETURNING id"
  )
  PEcAn.logger::logger.debug(insert_query)
  workflow_id <- PEcAn.DB::db.query(
    insert_query, con,
    values = unname(as.list(workflow_df))
  )[["id"]]

  PEcAn.logger::logger.debug(
    "Running workflow ID: ",
    format(workflow_id, scientific = FALSE)
  )

  PEcAn.DB::db.query(
    "UPDATE workflows SET folder = $1 WHERE id = $2", con, values = list(
      file.path("data", "workflows", paste0("PEcAn_", format(workflow_id, scientific = FALSE))),
      workflow_id
    )
  )

  return(workflow_id)
}

#################################################################################################

#* Insert the workflow into attributes table
#* @param workflowList List containing the workflow details
#* @author Tezan Sahu
insert.attribute <- function(workflowList){

  # Create an array of PFTs
  pfts <- c()
  for(i in seq(length(workflowList$pfts))){
    pfts <- c(pfts, workflowList$pfts[i]$pft$name)
  }

  # Obtain the model_id
  model_id <- workflowList$model$id
  if(is.null(model_id)){
    model_id <- PEcAn.DB::get.id("models", c("model_name", "revision"), c(workflowList$model$type, workflowList$model$revision), global_db_pool)
  }
  
  # Fill in the properties
  properties <- list(
    start = as.POSIXct(workflowList$run$start.date),
    end = as.POSIXct(workflowList$run$end.date),
    pfts = pfts,
    runs = if(is.null(workflowList$ensemble$size)) 1 else workflowList$ensemble$size,
    modelid = model_id,
    siteid = bit64::as.integer64(workflowList$run$site$id),
    sitename = dplyr::tbl(global_db_pool, "sites") %>% filter(id == bit64::as.integer64(workflowList$run$site$id)) %>% pull(sitename),
    #sitegroupid <- 
    lat = if(is.null(workflowList$run$site$lat)) "" else workflowList$run$site$lat,
    lon = if(is.null(workflowList$run$site$lon)) "" else workflowList$run$site$lon,
    email = if(is.na(workflowList$info$userid) || workflowList$info$userid == -1) "" else
      dplyr::tbl(global_db_pool, "users") %>% filter(id == bit64::as.integer64(workflowList$info$userid)) %>% pull(email),
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
  
  # con <- pool::poolCheckout(global_db_pool)
  # on.exit(pool::poolReturn(con), add = TRUE)
  con <- global_db_pool
  res <- DBI::dbSendStatement(con,
                              "INSERT INTO attributes (container_type, container_id, value) VALUES ($1, $2, $3)", 
                              list("workflows", bit64::as.integer64(workflowList$workflow$id), value_json))

  
}
