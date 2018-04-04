observeEvent(input$load_model,{
  req(input$all_run_id)
  
  df <- load.model()
  # output$results_table <- DT::renderDataTable(DT::datatable(head(masterDF)))
  
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  README.text <- c()
  
  for(i in 1:nrow(ids_DF)){
    
    dfsub <- df %>% filter(run_id == ids_DF$runID[i])
    
    diff.m <- diff(dfsub$dates)
    mode.m <- diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))]
    diff_units.m = units(mode.m)
    
    diff_message <- sprintf("timestep: %.2f %s", mode.m, diff_units.m)
    wf.folder <- workflow(bety, ids_DF$wID[i]) %>% collect() %>% pull(folder)
    
    README.text <- c(README.text, 
                     paste("SELECTION",i), 
                     "============",
                     readLines(file.path(wf.folder, 'run', ids_DF$runID[i], "README.txt")),
                     diff_message,
                     ""
    )
  }
  
  output$README <- renderUI({HTML(paste(README.text, collapse = '<br/>'))})
  
  output$dim_message <- renderText({sprintf("This data has %.0f rows, think about skipping exploratory plots if this is a large number...", dim(df)[1])})
  
})


