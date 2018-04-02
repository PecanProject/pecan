# Update all workflow ids
observe({
  # get_workflow_ids function (line 137) in db/R/query.dplyr.R takes a flag to check
  # if we want to load all workflow ids.
  # get_workflow_id function from query.dplyr.R
  query <- isolate(shiny::parseQueryString(session$clientData$url_search))
  all_ids <- get_workflow_ids(bety, query, all.ids=TRUE)
  updateSelectizeInput(session, "all_workflow_id", choices = all_ids)
})
# Update all run ids
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
})

# Update all variable names
observeEvent(input$load, {
  req(input$all_run_id)
  # All information about a model is contained in 'all_run_id' string
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  var_name_list <- c()
  for(row_num in 1:nrow(ids_DF)){
    var_name_list <- c(var_name_list, var_names_all(bety, ids_DF$wID[row_num], ids_DF$runID[row_num]))
  }
  updateSelectizeInput(session, "variable_name", choices = var_name_list)
})

# Loads data for all workflow and run ids after the load button is pressed.
# All information about a model is contained in 'all_run_id' string
# Wrapper over 'load_data_single_run' in PEcAn.db::query.dplyr
# Model data different from observations data 
loadNewData <- eventReactive(input$load,{
  req(input$all_run_id)
  # Get IDs DF from 'all_run_id' string
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  globalDF <- map2_df(ids_DF$wID, ids_DF$runID, ~load_data_single_run(bety, .x, .y))
  return(globalDF)
})

observeEvent(input$load,{
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