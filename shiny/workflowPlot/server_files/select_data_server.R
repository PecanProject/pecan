observeEvent(input$load_model,{
  masterDF <- load.model()
  output$results_table <- DT::renderDataTable(DT::datatable(head(masterDF)))
  output$date_message <- renderText({sprintf("Date range: %s to %s", range(masterDF$dates)[1], range(masterDF$dates)[2])})
  diff.m <- diff(masterDF$dates)
  mode.m <- diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))]
  diff_units.m = units(mode.m)
  output$diff_message <- renderText({sprintf("Timestep: %.2f %s", mode.m, diff_units.m)})
  output$dim_message <- renderText({sprintf("This data has %.0f rows, think about subsetting if this is a large number (things slow down around ... ?)", dim(masterDF)[1])})
})
