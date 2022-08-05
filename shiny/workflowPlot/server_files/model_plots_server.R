# Renders highcharter

output$modelPlot <- renderHighchart({
  validate(
    need(input$all_workflow_id, 'Select workflow id'),
    need(input$all_run_id, 'Select Run id'),
    need(input$load_model > 0, 'Select Load Model Outputs')
  )

  highchart() %>% 
    hc_add_series(data = c(), showInLegend = F) %>% 
    hc_xAxis(title = list(text = "Time")) %>% 
    hc_yAxis(title = list(text = "y")) %>% 
    hc_title(text = "You are ready to plot!") %>% 
    hc_add_theme(hc_theme_flat())
})

# Update units every time a variable is selected
observeEvent(input$var_name_model, {
  req(input$var_name_model)
  tryCatch({
    model.df <- load.model()
    default.unit <-
      model.df %>% filter(var_name == input$var_name_model) %>% pull(ylab) %>% unique()
    updateTextInput(session, "units_model", value = default.unit)
    
    #Signaling the success of the operation
    toastr_success("Variables were updated.")
  },
  error = function(e) {
    toastr_error(title = "Error in reading the run files.", conditionMessage(e))
  })
})

# Check that new units are parsible and can be used for conversion
observeEvent(input$units_model,{
  if(PEcAn.utils::unit_is_parseable(input$units_model)){
    model.df <- load.model()
    default.unit <- model.df %>% filter(var_name == input$var_name_model) %>% pull(ylab) %>% unique()
    if(units::ud_are_convertible(default.unit, input$units_model)){
      output$unit_text <-  renderText({"Conversion possible"})
    }else{
      output$unit_text <-  renderText({"Units are parsible but conversion is not possible"})
    }
  }else{
    output$unit_text <-  renderText({"Units are not parsible, please type units in udunits2 compatible format"})
  }
})

# update date range input limit
observe({
  df <- load.model()
  updateDateRangeInput(session, "date_range",
                       start = as.Date(min(df$dates)),
                       end = as.Date(max(df$dates)),
                       min = as.Date(min(df$dates)), 
                       max = as.Date(max(df$dates))
  )
})

# update "function" select box choice according to "agrregation" select box
observe({
  if(input$agg == "NONE"){
    updateSelectInput(session, "func", choices = "NONE")
  }else{
    updateSelectInput(session, "func", choices = c("mean", "sum"))
  }
})

observeEvent(input$ex_plot_model,{
  req(input$units_model)
  
  output$modelPlot <- renderHighchart({
    
    input$ex_plot_model
    isolate({
      tryCatch({
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...',{
                       
                       df <- dplyr::filter(load.model(), var_name == input$var_name_model)
                       
                       #updateSliderInput(session,"smooth_n_model", min = 0, max = nrow(df))
                    
                       title <- unique(df$title)
                       xlab <- unique(df$xlab)
                       ylab <- unique(df$ylab)
                       
                       unit <- ylab
                       if(input$units_model != unit & units::ud_are_convertible(unit, input$units_model)){
                         df$vals <- PEcAn.utils::ud_convert(df$vals,unit,input$units_model)
                         ylab <- input$units_model
                       }
                       
                       date_range <- paste0(input$date_range, collapse = "/")
                       
                       plot_type <- switch(input$plotType_model, point = "scatter", line = "line")
                       
                       smooth_param <- input$smooth_n_model * 100
                       
                       # function that converts dataframe to xts object, 
                       # selects subset of a date range and does data aggregtion
                       func <- function(df){
                         xts.df <- xts(df$vals, order.by = df$dates)
                         xts.df <- xts.df[date_range]
                         
                         if(input$agg=="NONE") return(xts.df)
                         
                         if(input$agg == "daily"){
                           xts.df <- apply.daily(xts.df, input$func)
                         }else if(input$agg == "weekly"){
                           xts.df <- apply.weekly(xts.df, input$func)
                         }else if(input$agg == "monthly"){
                           xts.df <- apply.monthly(xts.df, input$func)
                         }else if(input$agg == "quarterly"){
                           xts.df <- apply.quarterly(xts.df, input$func)
                         }else{
                           xts.df <- apply.yearly(xts.df, input$func)
                         }
                       }
                      
                       list <- split(df, df$run_id)
                       xts.list <- lapply(list, func)

                       ply <- highchart() 
                       
                       for(i in 1:length(xts.list)){
                         ply <- ply %>% 
                           hc_add_series(xts.list[[i]], type = plot_type, name = names(xts.list[i]), 
                                         regression = TRUE, 
                                         regressionSettings = list(type = "loess", loessSmooth = smooth_param)) 
                       }
                       
                       ply <- ply %>%
                         hc_add_dependency("plugins/highcharts-regression.js") %>% 
                         hc_title(text = title) %>% 
                         hc_xAxis(title = list(text = xlab), type = 'datetime') %>% 
                         hc_yAxis(title = list(text = ylab)) %>% 
                         hc_tooltip(pointFormat = " Date: {point.x:%Y-%m-%d %H:%M} <br> y: {point.y}") %>% 
                         hc_exporting(enabled = TRUE) %>% 
                         hc_chart(zoomType = "x")
 
                     })
        #Signaling the success of the operation
        toastr_success("Generate plot")
      },
      error = function(e) {
        toastr_error(title = "Error", conditionMessage(e))
      })
    })
    ply
  })
})


# masterDF <- loadNewData()
# # Convert from factor to character. For subsetting
# masterDF$var_name <- as.character(masterDF$var_name)
# # Convert to factor. Required for ggplot
# masterDF$run_id <- as.factor(as.character(masterDF$run_id))
# plt <- callModule(testModule, "model", masterDF)
# plt()





# annotate_text = sprintf("This is plot number %i", as.numeric(input$ex_plot_model))
# plt <- ggplot(data.frame(x = 0, y = 0), aes(x,y)) +
#   annotate("text", x = 0, y = 0, label = annotate_text,
#            size = 10, color = "grey")
