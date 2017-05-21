library(PEcAn.visualization)
library(PEcAn.DB)
library(shiny)
library(ncdf4)
library(ggplot2)
source('helper.R')
require(plotly)
library(scales)
# Define server logic
server <- shinyServer(function(input, output, session) {
  bety <- betyConnect()

  ranges <- reactiveValues(x = NULL, y = NULL)

  print("RESTART")
  # set the workflow id(s)
  ids <- get_workflow_ids(bety, session)
  updateSelectizeInput(session, "workflow_id", choices=ids)
  workflow_id <- reactive({
    req(input$workflow_id)
    workflow_id <- input$workflow_id
  })

  # update the run_ids if user changes workflow
  run_ids <- reactive(get_run_ids(bety, workflow_id()))
  observe({
    updateSelectizeInput(session, "run_id", choices=run_ids())
  })

  # update variables if user changes run
  var_names <- reactive({
      run_ids <- get_run_ids(bety, workflow_id())
      var_names <- get_var_names(bety, workflow_id(), run_ids[1])
      # Removing the variables "Year" and "FracJulianDay" from the Variable Name input in the app
      removeVarNames = c('Year','FracJulianDay')
      var_names <-var_names[!var_names %in% removeVarNames]
      return(var_names)
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
    # paste0(testVal)
    # paste0("x=", input$plot_dblclick$x, "\ny=", input$plot_dblclick$y)
  })
  
  output$outputPlot <- renderPlotly({
    workflow_id <- isolate(input$workflow_id)
    run_id <- isolate(input$run_id)
    var_name <- input$variable_name
    if (workflow_id != "" && run_id != "" && var_name != "") {
      workflow <- collect(workflow(bety, workflow_id))
      if(nrow(workflow) > 0) {
        outputfolder <- file.path(workflow$folder, 'out', run_id)
        files <- list.files(outputfolder, "*.nc$", full.names=TRUE)
        dates <- NA
        vals <- NA
        title <- var_name
        ylab <- ""
        for(file in files) {
          nc <- nc_open(file)
          var <- ncdf4::ncatt_get(nc, var_name)
          #sw <- if ('Swdown' %in% names(nc$var)) ncdf4::ncvar_get(nc, 'Swdown') else TRUE
          sw <- TRUE
          title <- var$long_name
          ylab <- var$units
          x <- ncdays2date(ncdf4::ncvar_get(nc, 'time'), ncdf4::ncatt_get(nc, 'time'))
          y <- ncdf4::ncvar_get(nc, var_name)
          b <- !is.na(x) & !is.na(y) & sw != 0
          dates <- if(is.na(dates)) x[b] else c(dates, x[b])
          vals <- if(is.na(vals)) y[b] else c(vals, y[b])
          ncdf4::nc_close(nc)
        }
        xlab <- if (is.null(ranges$x)) "Time" else paste(ranges$x, collapse=" - ")
        # plot result
        print(ranges$x)
        dates = as.Date(dates)
        df = data.frame(dates, vals)
        # df$dates = as.factor(df$dates)

        plt <- ggplot(df, aes(x=dates, y=vals)) +
          # geom_point(aes(color="Model output")) +
          geom_point() +
#          geom_smooth(aes(fill = "Spline fit")) +
          # coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
          # scale_y_continuous(labels=fancy_scientific) +
          labs(title=title, x=xlab, y=ylab) + 
          scale_color_manual(name = "", values = "black") +
          scale_fill_manual(name = "", values = "grey50") 
          # theme(axis.text.x = element_text(angle = -90))

        plt<-ggplotly(plt)
        # plot(plt)
        # add_icon()
      }
    }
  })
})

# runApp(port=5658, launch.browser=FALSE)
