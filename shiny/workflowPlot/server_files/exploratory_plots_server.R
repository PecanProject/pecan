# Renders ggplotly 

output$outputPlot <- renderPlotly({
  validate(
    need(input$all_workflow_id, 'Select workflow id'),
    need(input$all_run_id, 'Select Run id'),
    need(input$variable_name, 'Select Load Data')
  )
    plt <- ggplot(data.frame(x = 0, y = 0), aes(x,y)) + 
      annotate("text", x = 0, y = 0, label = "You are ready to plot!", 
               size = 10, color = "grey")
})

observeEvent(input$ex_plot,{
  
  # Renders ggplotly 
  output$outputPlot <- renderPlotly({
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
        df = df %>% select(posix = dates, vals) 
        colnames(df)[which(colnames(df) == "vals")] <- var 
        aligned_data = PEcAn.benchmark::align_data(model.calc = df, obvs.calc = externalData, var =var, align_method = "mean_over_larger_timestep")
        colnames(aligned_data)[grep("[.]m", colnames(aligned_data))] <- "model"
        colnames(aligned_data)[grep("[.]o", colnames(aligned_data))] <- "observations"
        colnames(aligned_data)[which(colnames(aligned_data) == "posix")] <- "Date"
        
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
    # if(input$plotview){
      plt<-ggplotly(plt)
    # }else{
    #   plt<-ggplot(data.frame(x = 0, y = 0), aes(x,y)) + annotate("text", x = 0, y = 0, label = "You chose to skip plotting
    #                                                            proceed to benchmarking", size = 10, color = "grey")
    # }
    # Not able to add icon over ggplotly
    # add_icon()
  })
  
})


