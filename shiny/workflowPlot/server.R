library(shiny)
library(ncdf4)

source("helper.R")

# Define server logic
server <- shinyServer(function(input, output, session) {
  print("RESTART")
  # set the workflow id(s)
  query <- isolate(parseQueryString(session$clientData$url_search))
  if ("workflow_id" %in% names(query)) {
    ids <- as.list(unlist(query[names(query) == 'workflow_id'], use.names=FALSE))
    updateSelectizeInput(session, "workflow_id", choices=ids)
  }

  # update the run_ids if user changes workflow
  observe({
    workflow_id <- input$workflow_id
    run_ids <- c("")
    if (workflow_id != "") {
      runs <- runs(bety, workflow_id)
      if (dplyr.count(runs) > 0) {
        run_ids <- collect(runs)[['run_id']]
      }
    }
    updateSelectizeInput(session, "run_id", choices=run_ids)
  })

  # update variables and dates if user changes run
  observe({
    # isolate workflow since we don't want to be triggered by this
    workflow_id <- isolate(input$workflow_id)
    run_id <- input$run_id
    var_names <- list()
    start_date <- NA
    end_date <- NA
    if (workflow_id != "" && run_id != "") {
      workflow <- collect(workflow(bety, workflow_id))
      if(nrow(workflow) > 0) {
        outputfolder <- file.path(workflow$folder, 'out', run_id)
        if(file_test('-d', outputfolder)) {
          files <- list.files(outputfolder, "*.nc$", full.names=TRUE)
          for(file in files) {
            nc <- nc_open(file)
            # get variable names
            lapply(nc$var, function(x) if(x$name != "") var_names[[x$longname]] <<- x$name)

            # get start/end date
            dates <- ncdays2date(nc$dim$time)
            date.min <- min(dates)
            date.max <- max(dates)
            if (is.na(start_date) || date.min < start_date) start_date = date.min
            if (is.na(end_date) || date.max > end_date) end_date = date.max

            nc_close(nc)
          }
        }
      }
    }
    if (length(var_names) == 0) {
      var_names <- list("")
    # } else {
    #   var_names <- sort(var_names)
    }
    updateSelectizeInput(session, "variable_name", choices=var_names)
    updateDateRangeInput(session, "dates", start=start_date, end=end_date, min=start_date, max=end_date)
  })

  observe({
    workflow_id <- isolate(input$workflow_id)
    run_id <- isolate(input$run_id)
    var_name <- input$variable_name
    dates <- input$dates
    if (workflow_id != "" && run_id != "" && var_name != "") {
      workflow <- collect(workflow(bety, workflow_id))
      if(nrow(workflow) > 0) {
        outputfolder <- file.path(workflow$folder, 'out', run_id)
        files <- list.files(outputfolder, "*.nc$", full.names=TRUE)

        # for(file in files) {
        #   nc <- nc_open(file)
        #
        #   nc_close(nc)
        # }
      }
    }
  })

  output$params <- renderTable({
    start_date <- format(input$dates[1], format="%B %d %Y")
    end_date <- format(input$dates[2], format="%B %d %Y")
    variable <- c("workflow_id", "run_id", "variable_name", "start_date", "end_date")
    value <- c(input$workflow_id, input$run_id, input$variable_name, start_date, end_date)
    data.frame(variable, value)
  })

  output$testOutput <- renderText({
    paste0("input$workflow_id is ", input$workflow_id, "\n",
           "input$run_id is ", input$run_id, "\n",
           "input$variable_name is ", input$variable_name, "\n")
  })
})

# runApp(port=5658, launch.browser=FALSE)
