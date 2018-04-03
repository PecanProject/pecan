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

observeEvent(input$ex_plot_model,{
  output$modelPlot <- renderPlotly({
    input$ex_plot_model
    isolate({
      masterDF <- load.model()
      df <- dplyr::filter(masterDF, var_name == input$var_name_model)
      
      updateSliderInput(session,"smooth_n_model", min = 0, max = nrow(df))
      title <- unique(df$title)
      xlab <- unique(df$xlab)
      ylab <- unique(df$ylab)
      
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
