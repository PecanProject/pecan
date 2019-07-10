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
  model.df <- load.model()
  default.unit <- model.df %>% filter(var_name == input$var_name_model) %>% pull(ylab) %>% unique()
  updateTextInput(session, "units_model", value = default.unit)
})

# Check that new units are parsible and can be used for conversion
observeEvent(input$units_model,{
  parseable <- ifelse(udunits2::ud.is.parseable(input$units_model), "can", "cannot")
  if(udunits2::ud.is.parseable(input$units_model)){
    model.df <- load.model()
    default.unit <- model.df %>% filter(var_name == input$var_name_model) %>% pull(ylab) %>% unique()
    if(udunits2::ud.are.convertible(default.unit, input$units_model)){
      output$unit_text <-  renderText({"Conversion possible"})
    }else{
      output$unit_text <-  renderText({"Units are parsible but conversion is not possible"})
    }
  }else{
    output$unit_text <-  renderText({"Units are not parsible, please type units in udunits2 compatible format"})
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
                       if(input$units_model != unit & udunits2::ud.are.convertible(unit, input$units_model)){
                         df$vals <- udunits2::ud.convert(df$vals,unit,input$units_model)
                         ylab <- input$units_model
                       }
                       
                       df$run_id <- as.numeric(as.character(df$run_id))
                       xts.df <- xts(df[,c("vals", "run_id")], order.by = df$dates)
                       
                       plot_type <- switch(input$plotType_model, point = "scatter", line = "line")
                   
                       #smooth_param <- input$smooth_n_model / nrow(df) *100
                       smooth_param <- input$smooth_n_model * 100
                       
                       ply <- highchart() 
                       
                       for(i in unique(xts.df$run_id)){
                         ply <- ply %>% 
                           hc_add_series(xts.df[xts.df$run_id ==  i, "vals"], 
                                         type = plot_type, name = i, regression = TRUE, 
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
