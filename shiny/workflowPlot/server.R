library(PEcAn.visualization)
library(PEcAn.DB)
library(shiny)
library(ncdf4)
library(ggplot2)
source('helper.R')
library(plotly)
library(scales)
library(dplyr)
# Define server logic
server <- shinyServer(function(input, output, session) {
  # options(shiny.trace=TRUE)
  bety <- betyConnect()
  # bety <- betyConnect('/home/carya/pecan/web/config.php')
  ranges <- reactiveValues(x = NULL, y = NULL)
  print("RESTART")
  # set the workflow id(s)
  # Retrieving all workflow ids. 
  # Creating a new function here so that we wont have to modify the original one. 
  # Ideally the get_workflow_ids function in db/R/query.dplyr.R should take a flag to check
  # if we want to load all workflow ids.
  get_all_workflow_ids <- function(bety) {
      ids <- workflows(bety, ensemble = TRUE) %>% distinct(workflow_id) %>% collect %>% 
        .[["workflow_id"]] %>% sort(decreasing = TRUE)
    return(ids)
  }  
  # get_workflow_ids
  ids <- get_all_workflow_ids(bety)
  # ids <- get_all_workflow_ids(bety, session)
  updateSelectizeInput(session, "workflow_id", choices=ids)
  # Removing observe here as we want to load workflow ids first
  # observe({
  #   updateSelectizeInput(session, "workflow_id", choices=ids)
  # })
  workflow_id <- reactive({
    req(input$workflow_id)
    workflow_id <- input$workflow_id
  })
  # update the run_ids if user changes workflow
  # run_ids <- reactive(get_run_ids(bety, workflow_id()))
  run_ids <- reactive({
    w_ids <- input$workflow_id
    run_id_list <- c()
    for(w_id in w_ids){
      r_ids <- get_run_ids(bety, w_id)
      for(r_id in r_ids){
        list_item <- paste0('workflow ',w_id,', run ',r_id)
        run_id_list <- c(run_id_list,list_item)
      }
    }
    return(run_id_list)
  })
  parse_workflowID_runID_from_input <- function(run_id_string){
    id_list <- c()
    split_string <- strsplit(run_id_string,',')[[1]]
    # run_id_string: 'workflow' workflow_ID, 'run' run_id
    wID <- as.numeric(strsplit(split_string[1],' ')[[1]][2])
    runID <- as.numeric(strsplit(split_string[2],' ')[[1]][2])
    id_list <- c(id_list,wID)
    id_list <- c(id_list,runID)
    # c(workflow_id,run_id)
    return(id_list)
  }
  observe({
    updateSelectizeInput(session, "run_id", choices=run_ids())
  })
  # update variables if user changes run
  get_var_names_for_ID <- function(bety,wID,runID){
    var_names <- get_var_names(bety, wID, runID)
    return(var_names)
  }
  var_names <- reactive({
      # run_ids <- get_run_ids(bety, workflow_id())
      # var_names <- get_var_names(bety, workflow_id(), run_ids[1])
      # Removing the variables "Year" and "FracJulianDay" from the Variable Name input in the app
      
      # run_ids <- input$run_id[1]
      # # for(rID in run_ids){
      #   id_list <- parse_workflowID_runID_from_input(run_ids)
      # #   var_names <- get_var_names_for_ID(bety,id_list[1],id_list[2])
      # # # }
      # removeVarNames <- c('Year','FracJulianDay')
      # var_names <-var_names[!var_names %in% removeVarNames]
      # return(id_list)
  })
  observe({
    updateSelectizeInput(session, "variable_name", choices=var_names())
  })
  observe({
    ignore <- input$variable_name
    ranges$x <- NULL
    ranges$y <- NULL
  })
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01", tz = "UTC")
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  # If want to render text
  output$info <- renderText({
    paste0(input$variable_name)
    # paste0(run_ids(),length(run_ids()),ids)
    # ,session$clientData$url_search)
    # paste0("x=", input$plot_dblclick$x, "\ny=", input$plot_dblclick$y)
  })
  workFlowData <-eventReactive(input$go,{
    # workflow_id = 99000000077
    # run_id = 99000000002
    # var_name = var_names 
    globalDF <- data.frame()
    for(workflow_id in ids){
      run_ids <- get_run_ids(bety,workflow_id)
      for(run_id in run_ids){
        var_names <- get_var_names(bety, workflow_id, run_id)
        removeVarNames <- c('Year','FracJulianDay')
        var_names <-var_names[!var_names %in% removeVarNames]
        # if (workflow_id != "" && run_id != "" && var_name != "") {
        workflow <- collect(workflow(bety, workflow_id))
        if(nrow(workflow) > 0) {
          outputfolder <- file.path(workflow$folder, 'out', run_id)
          files <- list.files(outputfolder, "*.nc$", full.names=TRUE)
          for(file in files) {
            nc <- nc_open(file)
            for(var_name in var_names){
              dates <- NA
              vals <- NA
              title <- var_name
              ylab <- ""
              var <- ncdf4::ncatt_get(nc, var_name)
              #sw <- if ('Swdown' %in% names(nc$var)) ncdf4::ncvar_get(nc, 'Swdown') else TRUE
              sw <- TRUE
              if(!is.null(var$long_name)){
                title <- var$long_name
              }
              if(!is.null(var$units)){
                ylab <- var$units
              }
              x <- ncdays2date(ncdf4::ncvar_get(nc, 'time'), ncdf4::ncatt_get(nc, 'time'))
              y <- ncdf4::ncvar_get(nc, var_name)
              b <- !is.na(x) & !is.na(y) & sw != 0
              dates <- if(is.na(dates)) x[b] else c(dates, x[b])
              dates <- as.Date(dates)
              vals <- if(is.na(vals)) y[b] else c(vals, y[b])
              xlab <- "Time"
              # Not required to change xlab by ranges. Using ggplotly.
              # xlab <- if (is.null(ranges$x)) "Time" else paste(ranges$x, collapse=" - ")
              valuesDF <- data.frame(dates,vals)
              metaDF <- data.frame(workflow_id,run_id,title,xlab,ylab,var_name)
              # Populating metaDF as same length of values DF
              # metaDF1<-metaDF[rep(seq_len(nrow(valuesDF))),]
              currentDF <- cbind(valuesDF,metaDF)
              globalDF <- rbind(globalDF,currentDF)
            }
            ncdf4::nc_close(nc)
          }
        }
      }
    }
    globalDF$title <- as.character(globalDF$title)
    globalDF$xlab <- as.character(globalDF$xlab)
    globalDF$ylab <- as.character(globalDF$ylab)
    globalDF$var_name <- as.character(globalDF$var_name)
    return(globalDF)
  })
  output$outputPlot <- renderPlotly({
    # workflow_id <- isolate(input$workflow_id)
    # run_id <- isolate(input$run_id)
    # var_name <- input$variable_name
    # if (workflow_id != "" && run_id != "" && var_name != "") {
    #   workflow <- collect(workflow(bety, workflow_id))
    #   if(nrow(workflow) > 0) {
    #     outputfolder <- file.path(workflow$folder, 'out', run_id)
    #     files <- list.files(outputfolder, "*.nc$", full.names=TRUE)
    #     dates <- NA
    #     vals <- NA
    #     title <- var_name
    #     ylab <- ""
    #     for(file in files) {
    #       nc <- nc_open(file)
    #       var <- ncdf4::ncatt_get(nc, var_name)
    #       #sw <- if ('Swdown' %in% names(nc$var)) ncdf4::ncvar_get(nc, 'Swdown') else TRUE
    #       sw <- TRUE
    #       title <- var$long_name
    #       ylab <- var$units
    #       x <- ncdays2date(ncdf4::ncvar_get(nc, 'time'), ncdf4::ncatt_get(nc, 'time'))
    #       y <- ncdf4::ncvar_get(nc, var_name)
    #       b <- !is.na(x) & !is.na(y) & sw != 0
    #       dates <- if(is.na(dates)) x[b] else c(dates, x[b])
    #       vals <- if(is.na(vals)) y[b] else c(vals, y[b])
    #       ncdf4::nc_close(nc)
    #     }
    #     xlab <- if (is.null(ranges$x)) "Time" else paste(ranges$x, collapse=" - ")
    #     # plot result
    #     print(ranges$x)
    #     dates <- as.Date(dates)
    #     df <- data.frame(dates, vals)
      # df <- workFlowData(input$workflow_id,input$run_id,input$variable_names)
      masterDF <- workFlowData()
      output$info1 <- renderText({
        paste0(nrow(masterDF))
      })
      validate(
        need(input$workflow_id, 'Found workflow id'),
        need(input$run_id, 'Run id detected'),
        need(input$variable_name, 'Please wait! Loading data')
        )
      masterDF$var_name <- as.character(masterDF$var_name)
      # masterDF$var_name = as.factor(masterDF$var_name)
      # df1<-subset(masterDF,var_name==var_name)
      df <- masterDF %>% 
        dplyr::filter(workflow_id == input$workflow_id &
                      run_id == input$run_id & 
                      var_name == input$variable_name) %>%
        dplyr::select(dates,vals)
      title <- unique(df$title)[1]
      xlab <- unique(df$xlab)[1]
      ylab <- unique(df$ylab)[1]
      output$info2 <- renderText({
        paste0(nrow(df))
        # paste0(typeof(title))
      })
      output$info3 <- renderText({
        paste0('xlab')
        # paste0(typeof(title))
      })
      
        # df1<-masterDF %>% filter(masterDF$var_name %in% var_name)
        # workflow_id %in% workflow_id) 
      # & run_id == run_id & var_name == var_name)
      # df<-masterDF %>% dplyr::filter(workflow_id == input$workflow_id)
      plt <- ggplot(df, aes(x=dates, y=vals)) +
          # geom_point(aes(color="Model output")) +
          geom_point() +
#          geom_smooth(aes(fill = "Spline fit")) +
          # coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          # scale_y_continuous(labels=fancy_scientific) +
          labs(title=title, x=xlab, y=ylab) +
          # labs(title=unique(df$title)[1], x=unique(df$xlab)[1], y=unique(df$ylab)[1]) +
          scale_color_manual(name = "", values = "black") +
          scale_fill_manual(name = "", values = "grey50") 
          # theme(axis.text.x = element_text(angle = -90))
        plt<-ggplotly(plt)
        # plot(plt)
        # add_icon()
    #   }
    # }
  })

# Shiny server closes here  
})

# runApp(port=6480, launch.browser=FALSE)
# runApp(port=5658, launch.browser=FALSE)
