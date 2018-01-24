library(PEcAn.visualization)
library(PEcAn.DB)
library(PEcAn.settings)
library(PEcAn.benchmark)
library(PEcAn.utils)
library(shiny)
library(ncdf4)
library(ggplot2)
# Helper allows to load functions and variables that could be shared both by server.R and ui.R 
# source('helper.R')
library(plotly)
library(scales)
library(lubridate)
library(dplyr)
library(reshape2)
library(purrr)
# Maximum size of file allowed to be uploaded: 100MB 
options(shiny.maxRequestSize=100*1024^2) 
# Define server logic
server <- shinyServer(function(input, output, session) {
  bety <- betyConnect()
  source("workflowPlot_fcns.R", local = TRUE) # Load all functions that need to be defined for this script
  
  
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
    
  # Update variable names observeEvent on input$load 
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
  # Renders ggplotly 
  output$outputPlot <- renderPlotly({
    # Error messages
    validate(
      need(input$all_workflow_id, 'Select workflow id'),
      need(input$all_run_id, 'Select Run id'),
      need(input$variable_name, 'Click the button to load data. Please allow some time')
    )
    # Load data
    masterDF <- loadNewData()
    # Convert from factor to character. For subsetting
    masterDF$var_name <- as.character(masterDF$var_name)
    # Convert to factor. Required for ggplot
    masterDF$run_id <- as.factor(as.character(masterDF$run_id))
    # Filter by variable name
    df <- masterDF %>%
      dplyr::filter(var_name == input$variable_name)
    # Another way to make dynamic slider
    # https://stackoverflow.com/questions/18700589/interactive-reactive-change-of-min-max-values-of-sliderinput
    # output$slider <- renderUI({
    #   sliderInput("smooth_n", "Value for smoothing:", min=0, max=nrow(df), value=80)
    # })
    updateSliderInput(session,"smooth_n", min=0, max=nrow(df))
    # Meta information about the plot
    title <- unique(df$title)
    xlab <- unique(df$xlab)
    ylab <- unique(df$ylab)
    # ggplot function for scatter plots.
    plt <- ggplot(df, aes(x=dates, y=vals, color=run_id))
    # model_geom <- switch(input$plotType, scatterPlot = geom_point, lineChart = geom_line)
    # plt <- plt + model_geom() 
    # Toggle chart type using switch
    switch(input$plotType,
           "scatterPlot"  = {
             plt <- plt + geom_point()
           },
           "lineChart"  = {
             plt <- plt + geom_line()
           }
    )
    # Check if user wants to load external data (==observations)
    # Similar to using event reactive
    if (input$load_data>0) {
      # Input ID is of the form (input id, Name). Split by space and use the first element
      inputs_df <- getInputs(bety,c(input$all_site_id))
      inputs_df <- inputs_df %>% dplyr::filter(input_selection_list == input$all_input_id)
      externalData <- loadObservationData(bety,inputs_df)
      # If variable found in the uploaded file.
      # TODO for now, actual observations can be plotted again a single model run (particular run id)
      # Have to enhance to allow multiple run ids
      if (input$variable_name %in% names(externalData)){
        # No need for subsetting though as align data returns for now only the provided variable name
        # externalData <- externalData %>% dplyr::select(posix,dplyr::one_of(input$variable_name))
        var = input$variable_name
        df = df %>% select(posix = dates, var = vals)
        colnames(df)[2]<-paste0(var) # Required for align data to work
        aligned_data = PEcAn.benchmark::align_data(model.calc = df, obvs.calc = externalData, var =var, align_method = "match_timestep")
        colnames(aligned_data) <- c("model","observations","Date") # Order returned by align_data
        # Melt dataframe to plot two types of columns together
        aligned_data <- reshape2::melt(aligned_data, "Date")
        data_geom <- switch(input$data_geom, point = geom_point, line = geom_line)
        plt <- ggplot(aligned_data, aes(x=Date, y=value, color=variable)) + data_geom()
        output$outputNoVariableFound <- renderText({
          paste0("Plotting data outputs.")
        })
      }
      # Shiny output if variable not found
      else {
        output$outputNoVariableFound <- renderText({
          paste0("Data related to variable not found in the observations uploaded. Select another variable")
        })
      }
    }
    plt <- plt + labs(title=title, x=xlab, y=ylab) + geom_smooth(n=input$smooth_n)
    # Earlier code for smoothing, y labels, color and fill values
    # Retaining if we want to use ggplot instead of ggplotly
    # geom_smooth(aes(fill = "Spline fit")) +
    # scale_y_continuous(labels=fancy_scientific) +
    # scale_color_manual(name = "", values = "black") +
    # scale_fill_manual(name = "", values = "grey50")
    plt<-ggplotly(plt)
    # Not able to add icon over ggplotly
    # add_icon()
  })
  
  
  # Source the server code for all the benchmarking tabs in the app
  source("bm.R", local = TRUE)
  
}) # Shiny server closes here  
# To run the shiny app locally
# runApp(port=6480, launch.browser=FALSE)
# runApp(port=5658, launch.browser=FALSE)
