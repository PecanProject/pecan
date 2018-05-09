# Renders ggplotly 

output$modelDataPlot <- renderPlotly({
  validate(
    need(length(input$all_workflow_id) == 1, "Select only ONE workflow ID"),
    need(length(input$all_run_id) == 1, "Select only ONE run ID"),
    need(input$load_model > 0, 'Select Load Data'),
    need(length(input$all_site_id) == 1, 'Select only ONE Site ID'),
    need(length(input$all_input_id) == 1, 'Select only ONE Input ID'),
    need(input$load_data > 0, 'Select Load External Data')
  )
  plt <- ggplot(data.frame(x = 0, y = 0), aes(x,y)) + 
    annotate("text", x = 0, y = 0, label = "You are ready to plot!", 
             size = 10, color = "grey")
})

# Update units every time a variable is selected
observeEvent(input$var_name_modeldata, {
  model.df <- load.model()
  default.unit <- model.df %>% filter(var_name == input$var_name_modeldata) %>% pull(ylab) %>% unique()
  updateTextInput(session, "units_modeldata", value = default.unit)
})

# Check that new units are parsible and can be used for conversion
observeEvent(input$units_modeldata,{
  if(udunits2::ud.is.parseable(input$units_modeldata)){
    model.df <- load.model()
    default.unit <- model.df %>% filter(var_name == input$var_name_modeldata) %>% pull(ylab) %>% unique()
    if(udunits2::ud.are.convertible(default.unit, input$units_modeldata)){
      output$unit_text2 <-  renderText({"Conversion possible"})
    }else{
      output$unit_text2 <-  renderText({"Units are parsible but conversion is not possible"})
    }
  }else{
    output$unit_text2 <-  renderText({"Units are not parsible, please type units in udunits2 compatible format"})
  }
})



observeEvent(input$ex_plot_modeldata,{
  output$modelDataPlot <- renderPlotly({
    input$ex_plot_modeldata
    isolate({
      
      var = input$var_name_modeldata
      
      model_data <- dplyr::filter(load.model(), var_name == var) 

      updateSliderInput(session,"smooth_n_modeldata", min = 0, max = nrow(model_data))
      title <- unique(model_data$title)
      xlab  <- unique(model_data$xlab)
      ylab  <- unique(model_data$ylab)
      
      model_data <- model_data %>% dplyr::select(posix = dates, !!var := vals)
      external_data <- load.model.data()
      aligned_data = PEcAn.benchmark::align_data(
        model.calc = model_data, obvs.calc = external_data, 
        var = var, align_method = "mean_over_larger_timestep") %>% 
        dplyr::select(everything(), 
                      model = matches("[.]m"), 
                      observations = matches("[.]o"), 
                      Date = posix)
      
      print(head(aligned_data))
      # Melt dataframe to plot two types of columns together
      aligned_data <- tidyr::gather(aligned_data, variable, value, -Date)
      
      unit <- ylab
      if(input$units_modeldata != unit & udunits2::ud.are.convertible(unit, input$units_modeldata)){
        aligned_data$value <- udunits2::ud.convert(aligned_data$value,unit,input$units_modeldata)
        ylab <- input$units_modeldata
      }
      
      
      data_geom <- switch(input$plotType_modeldata, point = geom_point, line = geom_line)
      
      plt <- ggplot(aligned_data, aes(x=Date, y=value, color=variable)) 
      plt <- plt + data_geom()
      plt <- plt + labs(title=title, x=xlab, y=ylab) 
      plt <- plt + geom_smooth(n=input$smooth_n_modeldata)
      ply <- ggplotly(plt)
      
      
    })
  })  
})




# observeEvent(input$ex_plot_modeldata,{
#   output$modelDataPlot <- renderPlotly({
#     annotate_text = sprintf("This is plot number %i", as.numeric(input$ex_plot_modeldata))
#     plt <- ggplot(data.frame(x = 0, y = 0), aes(x,y)) +
#       annotate("text", x = 0, y = 0, label = annotate_text,
#                size = 10, color = "grey")
#   })
# })  



# observeEvent(input$ex_modelData_plot,{
# 
# # Renders ggplotly 
# output$modelDataPlot <- renderPlotly({
#   # Load data
#   masterDF <- load.model()
#   # Convert from factor to character. For subsetting
#   masterDF$var_name <- as.character(masterDF$var_name)
#   # Convert to factor. Required for ggplot
#   masterDF$run_id <- as.factor(as.character(masterDF$run_id))
#   # Filter by variable name
#   df <- masterDF %>%
#     dplyr::filter(var_name == input$variable_name)
#   # Another way to make dynamic slider
#   # https://stackoverflow.com/questions/18700589/interactive-reactive-change-of-min-max-values-of-sliderinput
#   # output$slider <- renderUI({
#   #   sliderInput("smooth_n", "Value for smoothing:", min=0, max=nrow(df), value=80)
#   # })
#   updateSliderInput(session,"smooth_n", min=0, max=nrow(df))
#   # Meta information about the plot
#   title <- unique(df$title)
#   xlab <- unique(df$xlab)
#   ylab <- unique(df$ylab)
#   # ggplot function for scatter plots.
#   plt <- ggplot(df, aes(x=dates, y=vals, color=run_id))
#   # model_geom <- switch(input$plotType, scatterPlot = geom_point, lineChart = geom_line)
#   # plt <- plt + model_geom() 
#   # Toggle chart type using switch
#   switch(input$plotType,
#          "scatterPlot"  = {
#            plt <- plt + geom_point()
#          },
#          "lineChart"  = {
#            plt <- plt + geom_line()
#          }
#   )
#   # Check if user wants to load external data (==observations)
#   # Similar to using event reactive
#   if (input$load_data>0) {
#     # Input ID is of the form (input id, Name). Split by space and use the first element
#     inputs_df <- getInputs(bety,c(input$all_site_id))
#     inputs_df <- inputs_df %>% dplyr::filter(input_selection_list == input$all_input_id)
#     externalData <- loadObservationData(bety,inputs_df)
#     # If variable found in the uploaded file.
#     # TODO for now, actual observations can be plotted again a single model run (particular run id)
#     # Have to enhance to allow multiple run ids
#     if (input$variable_name %in% names(externalData)){
#       # No need for subsetting though as align data returns for now only the provided variable name
#       # externalData <- externalData %>% dplyr::select(posix,dplyr::one_of(input$variable_name))
#       var = input$variable_name
#       df = df %>% select(posix = dates, vals) 
#       colnames(df)[which(colnames(df) == "vals")] <- var 
#       aligned_data = PEcAn.benchmark::align_data(model.calc = df, obvs.calc = externalData, var =var, align_method = "mean_over_larger_timestep")
#       colnames(aligned_data)[grep("[.]m", colnames(aligned_data))] <- "model"
#       colnames(aligned_data)[grep("[.]o", colnames(aligned_data))] <- "observations"
#       colnames(aligned_data)[which(colnames(aligned_data) == "posix")] <- "Date"
#       
#       # Melt dataframe to plot two types of columns together
#       aligned_data <- reshape2::melt(aligned_data, "Date")
#       data_geom <- switch(input$data_geom, point = geom_point, line = geom_line)
#       plt <- ggplot(aligned_data, aes(x=Date, y=value, color=variable)) + data_geom()
#       output$outputNoVariableFound <- renderText({
#         paste0("Plotting data outputs.")
#       })
#     }
#     # Shiny output if variable not found
#     else {
#       output$outputNoVariableFound <- renderText({
#         paste0("Data related to variable not found in the observations uploaded. Select another variable")
#       })
#     }
#   }
#   plt <- plt + labs(title=title, x=xlab, y=ylab) + geom_smooth(n=input$smooth_n)
#   # Earlier code for smoothing, y labels, color and fill values
#   # Retaining if we want to use ggplot instead of ggplotly
#   # geom_smooth(aes(fill = "Spline fit")) +
#   # scale_y_continuous(labels=fancy_scientific) +
#   # scale_color_manual(name = "", values = "black") +
#   # scale_fill_manual(name = "", values = "grey50")
#   # if(input$plotview){
#   plt<-ggplotly(plt)
#   # }else{
#   #   plt<-ggplot(data.frame(x = 0, y = 0), aes(x,y)) + annotate("text", x = 0, y = 0, label = "You chose to skip plotting
#   #                                                            proceed to benchmarking", size = 10, color = "grey")
#   # }
#   # Not able to add icon over ggplotly
#   # add_icon()
# })
#   
# })


