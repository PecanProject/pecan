# The sidebar is where both the model output and external data are loaded

# Loading Model Output(s) -----------------------------------------------------#

# Update workflow ids
observe({
  # get_workflow_ids function (line 137) in db/R/query.dplyr.R takes a flag to check
  # if we want to load all workflow ids.
  # get_workflow_id function from query.dplyr.R
  all_ids <- get_workflow_ids(bety, query, all.ids=TRUE)
  updateSelectizeInput(session, "all_workflow_id", choices = all_ids)
  # Get URL prameters
  query <- parseQueryString(session$clientData$url_search)
  # Pre-select workflow_id from URL prams
  updateSelectizeInput(session, "all_workflow_id", selected = query[["workflow_id"]])
})

# Update run ids
all_run_ids <- reactive({
  # Retrieves all run ids for seleted workflow ids
  # Returns ('workflow ',w_id,', run ',r_id)
  req(input$all_workflow_id)
  w_ids <- input$all_workflow_id
  # Will return a list
  run_id_list <- c()
  for(w_id in w_ids){
    # For all the workflow ids
    r_ids <- get_run_ids(bety, w_id)
    for(r_id in r_ids){
      # Each workflow id can have more than one run ids
      # ',' as a separator between workflow id and run id
      list_item <- paste0('workflow ', w_id,', run ', r_id)
      run_id_list <- c(run_id_list, list_item)
    }
  }
  return(run_id_list)
})
# Update all run_ids ('workflow ',w_id,', run ',r_id)
observe({
  updateSelectizeInput(session, "all_run_id", choices = all_run_ids())
  # Get URL parameters
  query <- parseQueryString(session$clientData$url_search)
  # Make the run_id string with workflow_id
  url_run_id <- paste0('workflow ', query[["workflow_id"]],', run ', query[["run_id"]])
  # Pre-select run_id from URL params
  updateSelectizeInput(session, "all_run_id", selected = url_run_id)
})


# Loads data for all workflow and run ids after the load button is pressed.
# All information about a model is contained in 'all_run_id' string
# Wrapper over 'load_data_single_run' in PEcAn.db::query.dplyr
# Model data different from observations data
load.model <- eventReactive(input$load_model,{
  req(input$all_run_id)
  # Get IDs DF from 'all_run_id' string
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  globalDF <- map2_df(ids_DF$wID, ids_DF$runID, ~load_data_single_run(bety, .x, .y))
  print("Yay the model data is loaded!")
  print(head(globalDF))
  globalDF$var_name <- as.character(globalDF$var_name)
  globalDF$run_id <- as.factor(as.character(globalDF$run_id))
  return(globalDF)
})

# Update all variable names
observeEvent(input$load_model, {
  req(input$all_run_id)
  # All information about a model is contained in 'all_run_id' string
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  var_name_list <- c()
  for(row_num in 1:nrow(ids_DF)){
    var_name_list <- c(var_name_list, var_names_all(bety, ids_DF$wID[row_num], ids_DF$runID[row_num]))
  }
  updateSelectizeInput(session, "var_name_model", choices = var_name_list)
})

observeEvent(input$load_model,{
  # Retrieves all site ids from multiple seleted run ids when load button is pressed
  req(input$all_run_id)
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  site_id_list <- c()
  for(row_num in 1:nrow(ids_DF)){
    settings <- getSettingsFromWorkflowId(bety,ids_DF$wID[row_num])
    site.id <- c(settings$run$site$id)
    site_id_list <- c(site_id_list,site.id)
  }
  updateSelectizeInput(session, "all_site_id", choices=site_id_list)
})
# Update input id list as (input id, name)
observe({
  req(input$all_site_id)
  inputs_df <- getInputs(bety, c(input$all_site_id))
  formats_1 <- dplyr::tbl(bety, 'formats_variables') %>%
    dplyr::filter(format_id %in% inputs_df$format_id)
  if (dplyr.count(formats_1) == 0) {
    logger.warn("No inputs found. Returning NULL.")
    return(NULL)
  } else {
    formats_sub <- formats_1 %>%
      dplyr::pull(format_id) %>%
      unique()
    inputs_df <- inputs_df %>% dplyr::filter(format_id %in% formats_sub) # Only data sets with formats with associated variables will show up
    updateSelectizeInput(session, "all_input_id", choices=inputs_df$input_selection_list)
  }
})

load.model.data <- eventReactive(input$load_data, {
  req(input$all_input_id)

  inputs_df <- getInputs(bety,c(input$all_site_id))
  inputs_df <- inputs_df %>% dplyr::filter(input_selection_list == input$all_input_id)

  input_id <- inputs_df$input_id
  # File_format <- getFileFormat(bety,input_id)
  File_format <- PEcAn.DB::query.format.vars(bety = bety, input.id = input_id)
  start.year <- as.numeric(lubridate::year(inputs_df$start_date))
  end.year <- as.numeric(lubridate::year(inputs_df$end_date))
  File_path <- inputs_df$filePath
  # TODO There is an issue with the db where file names are not saved properly.
  # To make it work with the VM, uncomment the line below
  # File_path <- paste0(inputs_df$filePath,'.csv')
  site.id <- inputs_df$site_id
  site <- PEcAn.DB::query.site(site.id,bety$con)
  observations <- PEcAn.benchmark::load_data(
    data.path = File_path, format = File_format, time.row = File_format$time.row,
    site = site, start_year = start.year, end_year = end.year)
  print("Yay the observational data is loaded!")
  print(head(observations))
  return(observations)
})


# Update all variable names
observeEvent(input$load_data, {
  model.df <- load.model()
  obvs.df <- load.model.data()
  updateSelectizeInput(session, "var_name_modeldata",
                       choices = intersect(model.df$var_name, names(obvs.df)))
})