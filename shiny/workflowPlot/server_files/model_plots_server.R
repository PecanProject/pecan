# Renders ggplotly 

output$modelPlot <- renderPlotly({
  validate(
    need(input$all_workflow_id, 'Select workflow id'),
    need(input$all_run_id, 'Select Run id'),
    need(input$load_model > 0, 'Select Load Model Outputs')
  )
  
  plt <- ggplot(data.frame(x = 0, y = 0), aes(x,y)) +
    annotate("text", x = 0, y = 0, label = "Ready to plot!",
             size = 10, color = "grey")
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
  
  output$modelPlot <- renderPlotly({
    input$ex_plot_model
    isolate({
      df <- dplyr::filter(load.model(), var_name == input$var_name_model)
     
      updateSliderInput(session,"smooth_n_model", min = 0, max = nrow(df))
      
      title <- unique(df$title)
      xlab <- unique(df$xlab)
      ylab <- unique(df$ylab)
      
      unit <- ylab
      if(input$units_model != unit & udunits2::ud.are.convertible(unit, input$units_model)){
        df$vals <- udunits2::ud.convert(df$vals,unit,input$units_model)
        ylab <- input$units_model
      }
      
      data_geom <- switch(input$plotType_model, point = geom_point, line = geom_line)
      
      plt <- ggplot(df, aes(x = dates, y = vals, color = run_id))
      plt <- plt + data_geom()
      plt <- plt + labs(title=title, x=xlab, y=ylab) 
      plt <- plt + geom_smooth(n=input$smooth_n_model)
      ply <- ggplotly(plt)
    })
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
