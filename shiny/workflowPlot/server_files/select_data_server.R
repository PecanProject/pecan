observeEvent(input$load_model,{
  tryCatch({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...',
                 value = 0,{
                   
                   req(input$all_run_id)
                   incProgress(1 / 15)
                   
                   df <- load.model()
                   if (nrow(df)==0) return(NULL)
                   # output$results_table <- DT::renderDataTable(DT::datatable(head(masterDF)))
                   incProgress(10 / 15)
                   
                   ids_DF <- parse_ids_from_input_runID(input$all_run_id)
                   
                   select.df <- data.frame()

                   for(i in seq(nrow(ids_DF))){
                    
                     dfsub <- df %>% filter(run_id == ids_DF$runID[i])
                     
                     diff.m <- diff(dfsub$dates)
                     mode.m <- diff.m[which.max(tabulate(match(unique(diff.m), diff.m)))]
                     diff_units.m = units(mode.m)
                     
                     diff_message <- sprintf("timestep: %.2f %s", mode.m, diff_units.m)
                     wf.folder <- workflow(dbConnect$bety, ids_DF$wID[i]) %>% collect() %>% pull(folder)
                     
                     README.text <- tryCatch({
                       c(readLines(file.path(wf.folder, 'run', ids_DF$runID[i], "README.txt")),
                         diff_message)
                     },
                     error = function(e){
                       return(NULL)
                     })
                     
                     README.df <- data.frame()
                     
                     if(!is.null(README.text)){
                       README.df <- read.delim(textConnection(README.text),
                                               header=FALSE,sep=":",strip.white=TRUE)
                       
                       if("pft names"  %in% levels(README.df$V1)){
                         levels(README.df$V1)[levels(README.df$V1)=="pft names"] <- "pft name"
                       }
                       if(!"trait" %in% levels(README.df$V1)){
                         README.df <- rbind(README.df, data.frame(V1 = "trait", V2 = "-"))
                       }
                       if(!"quantile" %in% levels(README.df$V1)){
                         README.df <- rbind(README.df, data.frame(V1 = "quantile", V2 = "-"))
                       }
                     }
                     
                     select.df <- rbind(select.df, README.df)
                   }
                   
                    select.data <- select.df %>% 
                      dlply(.(V1), function(x) x[[2]]) %>% 
                      as.data.frame() %>% 
                      dplyr::rename(site.id = site..id) %>% 
                      select(runtype, workflow.id, ensemble.id, pft.name, quantile, trait, run.id, 
                            model, site.id, start.date, end.date, hostname, timestep, rundir, outdir)
                     
                    
                   output$datatable <- DT::renderDataTable(
                     DT::datatable(select.data,
                                   options = list(
                                     dom = 'ft',
                                     scrollX = TRUE,
                                     initComplete = DT::JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#404040', 'color': '#fff'});",
                                       "}")
                                     )
                                   )
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


