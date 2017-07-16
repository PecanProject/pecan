library(PEcAn.visualization)
library(PEcAn.DB)
library(PEcAn.settings)
library(PEcAn.benchmark)
library(shiny)
library(ncdf4)
library(ggplot2)
# Helper allows to load functions and variables that could be shared both by server.R and ui.R 
source('helper.R')
library(plotly)
library(scales)
library(lubridate)
library(dplyr)
# Maximum size of file allowed to be uploaded
options(shiny.maxRequestSize=100*1024^2) 
# Define server logic
server <- shinyServer(function(input, output, session) {
  bety <- betyConnect()
  # Update all workflow ids
  observe({
    # Ideally get_workflow_ids function (line 137) in db/R/query.dplyr.R should take a flag to check
    # if we want to load all workflow ids.
    # get_workflow_id function from query.dplyr.R
    all_ids <- get_workflow_ids(bety, session,all.ids=TRUE)
    updateSelectizeInput(session, "all_workflow_id", choices=all_ids)
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
        list_item <- paste0('workflow ',w_id,', run ',r_id)
        run_id_list <- c(run_id_list,list_item)
      }
    }
    return(run_id_list)
  })
  # Update all run_ids ('workflow ',w_id,', run ',r_id)
  observe({
    updateSelectizeInput(session, "all_run_id", choices=all_run_ids())
  })
  return_DF_from_run_ID <- function(diff_ids){
    # Called by function parse_ids_from_input_runID
    # which is a wrapper of this function
    # Returns a DF for a particular run_id
    split_string <- strsplit(diff_ids,',')[[1]]
    # Workflow id is the first element. Trim leading and ending white spaces. Split by space now
    wID <- as.numeric(strsplit(trimws(split_string[1],which = c("both")),' ')[[1]][2])
    # Run id is the second element
    runID <- as.numeric(strsplit(trimws(split_string[2],which = c("both")),' ')[[1]][2])
    return(data.frame(wID,runID))
  }
  # Wrapper over return_DF_from_run_ID
  # @param list of multiple run ids
  # run_id_string: ('workflow' workflow_ID, 'run' run_id)
  # @return Data Frame of workflow and run ids
  parse_ids_from_input_runID <- function(run_id_list){
    globalDF <- data.frame()
    for(w_run_id in run_id_list){
      globalDF <- rbind(globalDF,return_DF_from_run_ID(w_run_id))
    }
    return(globalDF)
  }  
  # Update variable names observeEvent on input$load 
  observeEvent(input$load,{
    req(input$all_run_id)
    # All information about a model is contained in 'all_run_id' string
    ids_DF <- parse_ids_from_input_runID(input$all_run_id)
    var_name_list <- c()
    for(row_num in 1:nrow(ids_DF)){
      var_name_list <- c(var_name_list,var_names_all(bety,ids_DF$wID[row_num],ids_DF$runID[row_num]))
    }
    updateSelectizeInput(session, "variable_name", choices=var_name_list)
  })
  # Loads data for all workflow and run ids after the load button is pressed.
  # All information about a model is contained in 'all_run_id' string
  # Wrapper over 'load_data_single_run' 
  loadNewData <-eventReactive(input$load,{
    req(input$all_run_id)
    # Get IDs DF from 'all_run_id' string
    ids_DF <- parse_ids_from_input_runID(input$all_run_id)
    globalDF <- data.frame()
    for(row_num in 1:nrow(ids_DF)){
      globalDF <- rbind(globalDF, load_data_single_run(bety,ids_DF$wID[row_num],ids_DF$runID[row_num]))
    }
    return(globalDF)
  })
  # loadExternalData <-eventReactive(input$load_data,{
  #   inFile <- input$fileUploaded
  #   if (is.null(inFile))
  #     return(NULL)
  #   externalData <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
  #            quote=input$quote)
  #   externalData$dates <- as.Date(externalData$dates)
  #   externalData <- externalData %>%
  #     dplyr::filter(var_name == input$variable_name)
  #   # output$info1 <- renderText({
  #   #   paste0(nrow(externalData))
  #   #   # paste0(inFile$datapath)
  #   # })
  #   return(externalData)
  # })  
  loadObservationData <- function(bety,settings,File_path,File_format){
    start.year<-as.numeric(lubridate::year(settings$run$start.date))
    end.year<-as.numeric(lubridate::year(settings$run$end.date))
    site.id<-settings$run$site$id
    site<-PEcAn.DB::query.site(site.id,bety$con)
    observations<-PEcAn.benchmark::load_data(data.path = File_path, format= File_format, time.row = File_format$time.row,  site = site, start_year = start.year, end_year = end.year) 
    return(observations)
  }
  getFileFormat <- function(bety,input.id){
    File_format <- PEcAn.DB::query.format.vars(bety = bety, input.id = input.id) 
    return(File_format)
  }
  getSettings <- function(workflowID){
    configPath <- paste0("~/output/PEcAn_",workflowID,"/pecan.CONFIGS.xml")
    settings<-PEcAn.settings::read.settings(configPath)
    return(settings)
  }
  # Renders ggplotly 
  output$outputPlot <- renderPlotly({
    # Error messages
    validate(
      need(input$all_workflow_id, 'Select workflow id'),
      need(input$all_run_id, 'Select Run id'),
      need(input$variable_name, 'Click the button to load data. Please allow some time')
    )
    # output$info <- renderText({
    # #   inFile <- input$fileUploaded
    # #   paste0(inFile$datapath)
    # #   # paste0(input$load_data)
    #   # paste0(File_format$mimetype)
    #   ids_DF <- parse_ids_from_input_runID(input$all_run_id)
    #   settings <- getSettings(ids_DF$wID[1])
    #   paste0(settings$run$site$id)
    #  })
    # Load data
    externalData <- data.frame()
    modelData <- loadNewData()
    
    masterDF <- rbind(modelData,externalData)
    # Convert from factor to character. For subsetting 
    masterDF$var_name <- as.character(masterDF$var_name)
    # Convert to factor. Required for ggplot 
    masterDF$run_id <- as.factor(as.character(masterDF$run_id))
    # Filter by variable name
    df <- masterDF %>%
      dplyr::filter(var_name == input$variable_name) 
    # Meta information about the plot
    title <- unique(df$title)
    xlab <- unique(df$xlab)
    ylab <- unique(df$ylab)
    # ggplot function for now scatter plots.
    # TODO Shubham allow line plots as well
    plt <- ggplot(df, aes(x=dates, y=vals, color=run_id)) 
    # Toggle chart type using switch
      switch(input$plotType,
             "scatterPlot"  = {
               plt <- plt + geom_point()
             },
             "lineChart"  = {
               plt <- plt + geom_line()
             }
      )
    plt <- plt + labs(title=title, x=xlab, y=ylab) + geom_smooth()
    
    if (input$load_data>0) { 
      File_format <- getFileFormat(bety,input$inputRecordID)
      ids_DF <- parse_ids_from_input_runID(input$all_run_id)
      settings <- getSettings(ids_DF$wID[1])
      inFile <- input$fileUploaded
      externalData <- loadObservationData(bety,settings,inFile$datapath,File_format)
      externalData <- externalData %>% dplyr::select(posix,input$variable_name)
      if(nrow(externalData)>0){
        names(externalData) <- c("dates","vals")
        externalData$dates <- as.Date(externalData$dates) 
        output$info <- renderText({
          # #   inFile <- input$fileUploaded
          # #   paste0(inFile$datapath)
          # #   # paste0(input$load_data)
          #   # paste0(File_format$mimetype)
          #   ids_DF <- parse_ids_from_input_runID(input$all_run_id)
          # paste0(settings$run$site$id)
          # paste0(site)
          paste0(names(externalData))
        })
        plt <- plt + geom_line(data = externalData,aes(x=dates, y=vals), linetype = 'dashed')
      }
      # externalData <- loadExternalData()
    }
    # if (!is.null(loaded_data)) {
    #   loaded_data <- loadExternalData()
    #   output$info1 <- renderText({
    #     paste0(nrow(loaded_data))
    #     # paste0(inFile$datapath)
    #   plt <- plt + geom_line(data = loaded_data,aes(x=dates, y=vals), linetype = 'dashed')
    # }
      # geom_point() +
      # Earlier smoothing and y labels
      # geom_smooth(aes(fill = "Spline fit")) +
      # scale_y_continuous(labels=fancy_scientific) +
      # Earlier color and fill values
      # scale_color_manual(name = "", values = "black") +
      # scale_fill_manual(name = "", values = "grey50") 
    plt<-ggplotly(plt)
    # Not able to add icon over ggplotly
    # add_icon()
  })
# Shiny server closes here  
})

# runApp(port=6480, launch.browser=FALSE)
# runApp(port=5658, launch.browser=FALSE)
