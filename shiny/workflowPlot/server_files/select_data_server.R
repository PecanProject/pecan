observeEvent(input$load_model,{
  tryCatch({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,{
                   
                   req(input$all_run_id)
                   incProgress(1 / 15)
                   
                   df <- load.model()
                   # output$results_table <- DT::renderDataTable(DT::datatable(head(masterDF)))
                   incProgress(10 / 15)
                   
                   ids_DF <- parse_ids_from_input_runID(input$all_run_id)
                   README.text <- c()
                   
                   
                   for(i in seq(nrow(ids_DF))){
                     
                     dfsub <- df %>% filter(run_id == ids_DF$runID[i])
                     
                     diff.m <- diff(dfsub$dates)
                     mode.m <- diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))]
                     diff_units.m = units(mode.m)
                     
                     diff_message <- sprintf("timestep: %.2f %s", mode.m, diff_units.m)
                     wf.folder <- workflow(dbConnect$bety, ids_DF$wID[i]) %>% collect() %>% pull(folder)
                     
                     README.text <- c(README.text, 
                                      tryCatch({
                                        readLines(file.path(wf.folder, 'run', ids_DF$runID[i], "README.txt"))
                                        },
                                        error = function(e){
                                          return(NULL)
                                        }),
                                      diff_message
                     )
                   }
                   
                   select.data <- read.delim(textConnection(README.text),
                                             header=FALSE,sep=":",strip.white=TRUE) %>%
                     unstack(V2 ~ V1) %>%  
                     dplyr::rename(site.id = site..id) %>% 
                     select(runtype, workflow.id, ensemble.id, pft.name, quantile, trait, run.id, 
                            model, site.id, start.date, end.date, hostname, timestep, rundir, outdir)
                     
                    
                   output$datatable <- DT::renderDataTable(
                     DT::datatable(select.data,options = list(scrollX = TRUE))
                     )
                  
                   #output$README <- renderUI({HTML(paste(README.text, collapse = '<br/>'))})
                   
                   output$dim_message <- renderText({sprintf("This data has %.0f rows,\nthink about skipping exploratory plots if this is a large number...", dim(df)[1])})
                   incProgress(4 / 15) 
                 })
    
    #Signaling the success of the operation
    toastr_success("Load model outputs")
  },
  error = function(e) {
    toastr_error(title = "Error", conditionMessage(e))
  })
})


