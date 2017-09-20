##### Benchmarking

bm <- reactiveValues()

observeEvent(input$load,{
  req(input$all_run_id)
  ids_DF <- parse_ids_from_input_runID(input$all_run_id)
  button <- FALSE
  if(nrow(ids_DF) == 1){
    
    # Check to see if the run has been saved as a reference run 
    # (note this is straight from the beginning of create.BRR and could 
    # potentially be turned in to its own mini function)
    ens_id <- dplyr::tbl(bety, 'runs') %>% dplyr::filter(id == ids_DF$runID) %>% dplyr::pull(ensemble_id)
    ens_wf <- dplyr::tbl(bety, 'ensembles') %>% dplyr::filter(id == ens_id) %>%
      dplyr::rename(ensemble_id = id) %>%
      dplyr::left_join(.,tbl(bety, "workflows") %>% dplyr::rename(workflow_id = id), by="workflow_id") %>% dplyr::collect()
    
    settingsXML <- file.path(ens_wf$folder,"pecan.CHECKED.xml")
    # Automatically creates a new pecan.xml I think. Need to fix this. 
    clean <- PEcAn.settings::clean.settings(inputfile = settingsXML,write=FALSE)
    # Remove database & host information
    clean$database <- NULL 
    clean$host <- NULL
    clean$info <- NULL
    clean$outdir <- NULL
    clean$meta.analysis <- NULL
    clean$ensemble <- NULL
    str(clean)
    settings_xml <- toString(PEcAn.utils::listToXml(clean, "pecan"))
    ref_run <- db.query(paste0(" SELECT * from reference_runs where settings = '", settings_xml,"'"), bety$con)
    
    if(length(ref_run) == 0){
      # If not registered, button appears with option to run create.BRR
      brr_message <- sprintf("Would you like to save this run (run id = %.0f, ensemble id = %0.f) as a reference run?", ids_DF$runID, ens_id) 
      button <- TRUE
    }else if(dim(ref_run)[1] == 1){
      bm$BRR <- ref_run %>% rename(.,reference_run_id = id)
      bm$BRR
      brr_message <- sprintf("This run has been registered as a reference run (id = %.0f)", bm$BRR$reference_run_id)
    }else if(dim(ref_run)[1] > 1){ # There shouldn't be more than one reference run per run
      brr_message <- ("There is more than one reference run in the database for this run. Review for duplicates.")
    }
  }else if(nrow(ids_DF) > 1){
    brr_message <- "Benchmarking currently only works when one run is selected."
  }else{
    brr_message <- "Cannot do benchmarking"
  }
  
  # This is redundant but better for debugging
  bm$brr_message <- brr_message
  bm$button_BRR <- button
  bm$ens_wf <- ens_wf
  
})

# When button to register run is clicked, create.BRR is run and the button is removed.
observeEvent(input$create_bm,{
  req(input$all_run_id)
  bm$BRR <- PEcAn.benchmark::create_BRR(bm$ens_wf, con = bety$con)
  bm$brr_message <- sprintf("This run has been successfully registered as a reference run (id = %.0f)", bm$BRR$reference_run_id)
  bm$button_BRR <- FALSE
})

observeEvent({
  bm$brr_message
  bm$button_BRR
},{
  output$brr_message <- renderText({bm$brr_message})
  output$button_BRR <- renderUI({
    if(bm$button_BRR){actionButton("create_bm", "Create Benchmarking Reference Run")}
  })
})


##### Setup benchmarks
observeEvent(input$load_data,{
  require(input$all_input_id)
  
  bm$metrics <- dplyr::tbl(bety,'metrics') %>% dplyr::select(one_of("id","name","description")) %>% collect()
  bm$vars_list <- c("NPP", "LAI")
  
  # Need to write warning message that can only use one input id
  inputs_df <- getInputs(bety,c(input$all_site_id)) %>% dplyr::filter(input_selection_list == input$all_input_id)
  format <- PEcAn.DB::query.format.vars(bety = bety, input.id = inputs_df$input_id)
  # Are there more human readable names?
  bm$vars_list <- format$vars$pecan_name[-grep("%",format$vars$storage_type)]
  
  
  #This will be a longer set of conditions
  bm$ready <- !is.null(bm$BRR)
})

observeEvent({
  bm$ready
  bm$metrics_list
  bm$plots_list
  bm$vars_list
  },{
  output$bm_settings <- renderUI({
    if(bm$ready){
      list(
        column(4, wellPanel(
          checkboxGroupInput("vars", label = h3("Variables"),
                             choices = bm$vars_list),
          actionButton("selectall.var","Select /Deselect all variables"),
          label=h6("Label")
        )),
        column(4, wellPanel(
          checkboxGroupInput("metrics", label = h3("Numerical Metrics"),
                             choices = bm$metrics$description[-grep("_plot",bm$metrics$name)]),
          actionButton("selectall.num","Select/Deselect all numerical metrics") ,
          label=h6("Label")
        )),
        column(4, wellPanel(
          checkboxGroupInput("plots", label = h3("Plot Metrics"),
                             choices = bm$metrics$description[grep("_plot",bm$metrics$name)]),
          actionButton("selectall.plot","Select/Deselect all plot metrics"),
          label=h6("Label")
        ))
      )
    }
  })
})


